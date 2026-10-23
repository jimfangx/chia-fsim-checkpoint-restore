// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class IndependentAccessClockSpec extends AnyFlatSpec {
  private def lower(circuit: Circuit) = new Compiler(Forms.LowForm).execute(
    CircuitState(circuit, ChirrtlForm)).copy(form = LowForm)
  private def emit(state: CircuitState): String = new Compiler(
    Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(
      state.copy(annotations = state.annotations ++ Seq(
        firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation,
        firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))).getEmittedCircuit.value

  behavior.of("Independent always-running checkpoint access clock")

  it should "bind only the response endpoint to the transport clock and reject clock-contract tampering" in {
    val state = lower(Parser.parse("""circuit Storage :
  module Storage :
    input clock : Clock
    input data : UInt<8>
    output result : UInt<8>
    reg saved : UInt<8>, clock
    saved <= data
    result <= saved
"""))
    val manifest = SimulatorStateInventory.discover(state)
    val legacy = GeneratedStateBankAccess.generate(state, manifest)
    val separate = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    assert(legacy.entries == separate.entries)
    assert((parse(legacy.binding) \ "access_clock") == JString("clock"))
    assert((parse(separate.binding) \ "access_clock") == JString("ckpt_clock"))
    assert(!legacy.state.circuit.modules.head.ports.exists(_.name == "ckpt_clock"))
    val text = separate.state.circuit.serialize
    assert(text.contains("reg saved : UInt<8>, clock"))
    for (name <- Seq("ckpt_pending", "ckpt_saved_data", "ckpt_saved_error"))
      assert(text.linesIterator.exists(line => line.contains(s"reg $name :") && line.contains(", ckpt_clock")))
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(separate, manifest)
    for ((before, after) <- Seq("\"ckpt_clock\"" -> "\"clock\"",
                               "\"access_clock\"" -> "\"unknown_clock\"",
                               "externally_held_all_functional_owners_before_access_until_drain" -> "assumed_atomic")) {
      val bad = separate.copy(binding = separate.binding.replace(before, after))
      assert(intercept[IllegalArgumentException] {
        midas.widgets.CheckpointCaptureDescriptors.fromGenerated(bad, manifest)
      }.getMessage.contains("access clock/freeze contract"))
    }
  }

  it should "instrument all actual retained Rocket data-array RAMs and synchronous read owners" in {
    val filename = sys.props.getOrElse("checkpoint.rocket.reference",
      fail("checkpoint.rocket.reference must name the retained exact-recipe FIRRTL input"))
    val input = scala.io.Source.fromFile(filename)
    val circuit = try Parser.parse(input.mkString) finally input.close()
    val selected = circuit.copy(main = "DCacheDataArray",
      modules = circuit.modules.filter(_.name == "DCacheDataArray"))
    val state = lower(selected)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    assert(result.entries.size == 128)
    assert(result.entries.count(e => e.width == 8 && e.depth == 512) == 64)
    assert(result.entries.count(e => e.width == 8 && e.depth == 1) == 64)
    val original = state.copy(circuit = state.circuit.copy(main = "OriginalDCacheDataArray",
      modules = state.circuit.modules.map { case m: Module => m.copy(name = "OriginalDCacheDataArray") }))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("rocket-dcache-array"); Files.createDirectories(out)
      Seq("generated-bank-input.fir" -> state.circuit.serialize,
        "generated-bank.fir" -> result.state.circuit.serialize,
        "generated-bank.sv" -> emit(result.state), "reference.sv" -> emit(original),
        "generated-bank-manifest.json" -> manifest, "generated-bank-binding.json" -> result.binding
      ).foreach { case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8)) }
    }
  }
}
