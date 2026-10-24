// See LICENSE for license details.
package midas.widgets

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.io.Source
import chisel3.stage.ChiselStage
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.checkpoint._
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import firesim.lib.nasti.NastiParameters

class CheckpointNativeDescriptorsSpec extends AnyFlatSpec {
  private val resource = Source.fromResource("checkpoint/NativeStorage.firrtl")
  private val source = try resource.mkString finally resource.close()
  private def input = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(source),
    ChirrtlForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  private val reference = input
  private val manifest = SimulatorStateInventory.discover(reference)
  private val plan = PhysicalClockCutPlan.discover(reference, manifest)
  private val deps = PhysicalClockCutDependencies.discover(reference, manifest, plan)
  private val inventory = GeneratedStateBankAccess.physicalFunctionalInventory(reference, manifest, plan, deps)
  private val generated = GeneratedStateBankAccess.generatePhysicalCompanion(reference, manifest, plan, deps)
  private def compose = CheckpointCaptureDescriptors.fromNativeGenerated(generated, reference, manifest, plan, deps)
  private val out = sys.props.get("checkpoint.widget.out").map(Paths.get(_).resolve("native-descriptors"))
    .getOrElse(Files.createTempDirectory("native-descriptors-"))
  Files.createDirectories(out)
  private def save(name: String, text: String): Unit = Files.write(out.resolve(name), text.getBytes(UTF_8))
  private def emit(s: CircuitState): String = {
    val emitter = new SemanticCleanEmitter
    new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(s.copy(annotations = s.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
      .getEmittedCircuit.value
  }

  behavior.of("Same-reference native functional and CE descriptors")

  it should "bind all native storage and distinct CE owners to one reconstructed original circuit" in {
    val d = compose
    assert(d.entries.take(5) == generated.entries && d.entries.size == 7 && d.physicalClockGates == 2)
    assert(d.entries.takeRight(2).zipWithIndex.forall { case (e, i) =>
      e.bank == 255 && e.slot == i && e.width == 1 && e.depth == 1
    })
    val bound = parse(d.binding)
    assert((bound \ "functional_manifest") == parse(inventory))
    assert((bound \ "functional_binding") == parse(generated.binding))
    assert((bound \ "same_reference_sha256") == JString(SemanticStateInventory.sha256(reference.circuit.serialize)))
    assert((parse(d.manifest) \ "same_reference_sha256") == (bound \ "same_reference_sha256"))
    assert((bound \ "physical_bank_binding" \ "reference_sha256") == (bound \ "same_reference_sha256"))
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").foreach { k =>
      assert((bound \ k) == JBool(false) && (parse(d.manifest) \ k) == JBool(false))
    }
    // Legacy descriptor admission stays unchanged.
    intercept[IllegalArgumentException] { CheckpointCaptureDescriptors.fromGenerated(generated, inventory) }
    save("composition.json", d.manifest); save("composition-binding.json", d.binding)
    save("host.inc", d.cppManifest)
    save("reference.fir", reference.circuit.serialize); save("manifest.json", manifest)
    save("physical-clock-plan.json", plan); save("physical-clock-dependencies.json", deps)
    save("functional-inventory.json", inventory); save("functional-binding.json", generated.binding)
    save("instrumented.fir", generated.state.circuit.serialize); save("instrumented.sv", emit(generated.state))
    save("NativeReference.sv", emit(reference.copy(circuit = reference.circuit.copy(main = "NativeReference",
      modules = reference.circuit.modules.map {
        case m: firrtl.ir.Module if m.name == reference.circuit.main => m.copy(name = "NativeReference")
        case m => m
      }), annotations = Nil)))
    PhysicalClockStateBank.emit(reference, manifest, plan, deps).foreach { f =>
      save(f.fileSuffix.stripPrefix("."), f.body)
    }
    (new ChiselStage).emitVerilog(new CheckpointCaptureDescriptorROM(d) {
      override def desiredName = "NativeDescriptorROMProbe"
    }, Array("--target-dir", out.resolve("rom").toString))
    implicit val p: Parameters = Parameters.empty.alterPartial { case CtrlNastiKey => NastiParameters(32, 32, 4) }
    val cfg = CheckpointCaptureWidgetParams(7, d.manifestSha256, Seq("fixture.functional", "fixture.host"),
      descriptors = Some(d), physicalClockGates = 2)
    val widget = LazyModule(new CheckpointCaptureWidget(cfg))
    (new ChiselStage).emitVerilog(widget.module, Array("--target-dir", out.resolve("widget").toString))
    val header = new StringBuilder
    widget.module.genHeader(BigInt(0x1000), Map.empty, header)
    save("constants.h", header.toString)
    assert(header.toString.contains(d.bindingSha256) && header.toString.contains(d.cppManifest))
    val probe = NativeFunctionalBankProbe(reference.copy(annotations = reference.annotations ++ Seq(
      NativeFunctionalBankProbeAnnotation, PhysicalClockCutPlanAnnotation,
      SimulatorCheckpointInventoryAnnotation(reference.circuit.main),
      midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, None))))
    probe.annotations.collect { case a: midas.stage.GoldenGateOutputFileAnnotation => a }.foreach { f =>
      save("FireSim-generated" + f.fileSuffix, f.body)
    }
    println(s"NATIVE_DESCRIPTORS_ELABORATED out=$out functional=5 physical=2")
  }

  it should "reject different references even when every supplied physical sidecar is independently valid" in {
    val other = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(
      source.replace("value <= add(value, UInt<1>(1))", "value <= add(value, UInt<2>(2))")),
      ChirrtlForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
    val m = SimulatorStateInventory.discover(other); val p = PhysicalClockCutPlan.discover(other, m)
    val d = PhysicalClockCutDependencies.discover(other, m, p)
    intercept[IllegalArgumentException] {
      CheckpointCaptureDescriptors.fromNativeGenerated(generated, other, m, p, d)
    }
    Seq((manifest + " ", plan, deps), (manifest, plan + " ", deps),
      (manifest, plan, deps + " ")).foreach { case (m, p, d) =>
      intercept[IllegalArgumentException] {
        CheckpointCaptureDescriptors.fromNativeGenerated(generated, reference, m, p, d)
      }
    }
  }

  it should "reject coherently rehashed instrumentation, selectors, coverage flags and substituted owner identities" in {
    val alteredState = generated.state.copy(circuit = generated.state.circuit.copy(info = firrtl.ir.FileInfo("changed")))
    val alteredBinding = SemanticStateInventory.canonical(parse(generated.binding) merge JObject(List(
      "instrumented_sha256" -> JString(SemanticStateInventory.sha256(alteredState.circuit.serialize)))))
    val bad = Seq(
      generated.copy(state = alteredState, binding = alteredBinding),
      generated.copy(entries = generated.entries.updated(0, generated.entries.head.copy(slot = 90))),
      generated.copy(binding = generated.binding.replace("\"complete_fpga_capture\":false", "\"complete_fpga_capture\":true")),
      generated.copy(binding = generated.binding.replace("NativeStorage.left_.gate", "NativeStorage.right_.gate")))
    bad.foreach { b => intercept[IllegalArgumentException] {
      CheckpointCaptureDescriptors.fromNativeGenerated(b, reference, manifest, plan, deps)
    }}
    val d = compose
    intercept[IllegalArgumentException] {
      CheckpointCaptureDescriptors.withPhysicalClockState(d, reference, manifest, plan, deps)
    }
    Seq(0, 1, 3).foreach { n => intercept[IllegalArgumentException] {
      CheckpointCaptureWidgetParams(7, d.manifestSha256, Seq("fixture.functional"),
        descriptors = Some(d), physicalClockGates = n)
    }}
    CheckpointCaptureWidgetParams(7, d.manifestSha256, Seq("fixture.functional"),
      descriptors = Some(d), physicalClockGates = 2)
  }
}
