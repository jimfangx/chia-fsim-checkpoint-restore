// See LICENSE for license details.
package midas.widgets

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.io.Source
import chisel3.stage.ChiselStage
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import firesim.lib.nasti.NastiParameters
import midas.checkpoint.{GeneratedStateBankAccess, SemanticStateInventory, SimulatorStateInventory}
import org.scalatest.flatspec.AnyFlatSpec

class CheckpointCaptureDescriptorsSpec extends AnyFlatSpec {
  private val resource = Source.fromResource("checkpoint/InventoryDUT.firrtl")
  private val source = (try resource.mkString finally resource.close()).replace(
    "reg harnessOnly : UInt<1>, clock", "reg harnessOnly : UInt<1>, clock with :\n      reset => (reset, UInt<1>(0))")
  private val state = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(source), ChirrtlForm)).copy(form = LowForm)
  private val manifest = SemanticStateInventory.discover(state, state.circuit.main)
  private val result = GeneratedStateBankAccess.generate(state, manifest)
  private val descriptors = CheckpointCaptureDescriptors.fromGenerated(result, manifest)
  private val out = sys.props.get("checkpoint.widget.out").map(Paths.get(_).resolve("generated-network"))
    .getOrElse(Files.createTempDirectory("checkpoint-network-"))
  Files.createDirectories(out)
  private def save(name: String, text: String): Unit = Files.write(out.resolve(name), text.getBytes(UTF_8))

  behavior.of("Manifest-bound capture descriptor ROM")

  it should "bind every ordered storage descriptor and retain independent simulator identities" in {
    assert(descriptors.entries == result.entries && descriptors.entries.size == 9)
    val simulatorManifest = SimulatorStateInventory.discover(state)
    val simulator = GeneratedStateBankAccess.generate(state, simulatorManifest)
    val simDescriptors = CheckpointCaptureDescriptors.fromGenerated(simulator, simulatorManifest)
    assert(descriptors.manifestSha256 != simDescriptors.manifestSha256)
    assert(descriptors.entries.map(_.stateID).toSet.intersect(simDescriptors.entries.map(_.stateID).toSet).isEmpty)
    intercept[IllegalArgumentException] { CheckpointCaptureDescriptors.fromGenerated(result, simulatorManifest) }
  }

  it should "reject stale shape, missing words, reordered bindings and changed instrumented circuits" in {
    def reject(bad: GeneratedStateBankAccess.Result): Unit =
      intercept[IllegalArgumentException] { CheckpointCaptureDescriptors.fromGenerated(bad, manifest) }
    reject(result.copy(entries = result.entries.drop(1)))
    reject(result.copy(entries = result.entries.reverse))
    reject(result.copy(binding = result.binding.replace("\"depth\":8", "\"depth\":7")))
    reject(result.copy(binding = result.binding.replace("latched-banked64-v1", "unknown-v1")))
    reject(result.copy(binding = result.binding.replace("all_reachable_firrtl_state_only", "partial")))
    reject(result.copy(state = result.state.copy(circuit = result.state.circuit.copy(main = "wrong"))))
  }

  it should "reject ROM and transport hash or count disagreement" in {
    val cfg = CheckpointCaptureWidgetParams(9, descriptors.manifestSha256,
      Seq("fixture.storage", "fixture.host"), descriptors = Some(descriptors))
    intercept[IllegalArgumentException] { cfg.copy(entries = 8) }
    intercept[IllegalArgumentException] { cfg.copy(manifestSha256 = "1" * 64) }
    intercept[IllegalArgumentException] { cfg.copy(entries = 65536) }
  }

  it should "emit a bounded ROM, actual generated storage, Widget and matching driver constructor" in {
    (new ChiselStage).emitVerilog(new CheckpointCaptureDescriptorROM(descriptors) {
      override def desiredName = "CheckpointCaptureDescriptorROMProbe"
    },
      Array("--target-dir", out.resolve("rom").toString))
    val emitted = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(result.state.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter]))))
    save("generated-bank.sv", emitted.annotations.collectFirst {
      case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value }.get)
    save("generated-bank.fir", result.state.circuit.serialize)
    save("generated-bank-input.fir", state.circuit.serialize)
    save("generated-bank-binding.json", result.binding)
    save("generated-bank-semantic.json", manifest)
    implicit val p: Parameters = Parameters.empty.alterPartial {
      case CtrlNastiKey => NastiParameters(32, 32, 4)
    }
    val cfg = CheckpointCaptureWidgetParams(9, descriptors.manifestSha256,
      Seq("fixture.storage", "fixture.host"), descriptors = Some(descriptors))
    val widget = LazyModule(new CheckpointCaptureWidget(cfg))
    val rtl = (new ChiselStage).emitVerilog(widget.module,
      Array("--target-dir", out.resolve("widget").toString))
    assert(rtl.contains("module CheckpointCaptureDescriptorROM"))
    val header = new StringBuilder
    widget.module.genHeader(BigInt(0x1000), Map.empty, header)
    save("checkpoint_network_constants.h", header.toString)
    assert(header.toString.contains(descriptors.bindingSha256))
    assert(header.toString.contains("new checkpoint_capture_t(simif, *registry.get_stream_engine(), 4096ULL"))
    assert(header.toString.contains(descriptors.cppManifest))
    save("checkpoint_network_manifest.inc", descriptors.cppManifest)
    println(s"CHECKPOINT_NETWORK_ELABORATED out=$out entries=${descriptors.entries.size}")
  }
}
