// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

/** The integration test uses generated native clocks, never a hand-written
  * substitute network. This deliberately does not admit production banking.
  */
class PhysicalClockCaptureNetworkSpec extends AnyFlatSpec {
  behavior.of("Generated clock network for physical MMIO/DMA capture")
  it should "emit three original native gates and the ungated reset dependency" in {
    val source = """circuit PhysicalTransportNetwork :
  extmodule Native :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    defname = BUFGCE
  module Leaf :
    input clock : Clock
    input reset : UInt<1>
    input ce : UInt<1>
    input data : UInt<32>
    input initial : UInt<32>
    output value : UInt<32>
    inst gate of Native
    gate.I <= clock
    gate.CE <= ce
    reg saved : UInt<32>, gate.O with :
      reset => (reset, initial)
    saved <= xor(bits(mul(saved, UInt<6>(33)), 31, 0), data)
    value <= saved
  module PhysicalTransportNetwork :
    input clock : Clock
    input reset : UInt<1>
    output target_cycle : UInt<64>
    output state0 : UInt<32>
    output state1 : UInt<32>
    output state2 : UInt<32>
    reg cycles : UInt<64>, clock with :
      reset => (reset, UInt<64>(0))
    cycles <= add(cycles, UInt<1>(1))
    target_cycle <= cycles
    inst left of Leaf
    inst middle of Leaf
    inst right of Leaf
    left.clock <= clock
    middle.clock <= clock
    right.clock <= clock
    left.reset <= reset
    middle.reset <= reset
    right.reset <= reset
    left.ce <= or(reset, not(bits(cycles, 0, 0)))
    middle.ce <= or(reset, bits(cycles, 2, 2))
    right.ce <= or(reset, xor(bits(cycles, 1, 1), bits(cycles, 0, 0)))
    left.data <= bits(cycles, 31, 0)
    middle.data <= bits(add(cycles, UInt<2>(1)), 31, 0)
    right.data <= bits(add(cycles, UInt<2>(2)), 31, 0)
    left.initial <= UInt<32>(1)
    middle.initial <= UInt<32>(2)
    right.initial <= UInt<32>(3)
    state0 <= left.value
    state1 <= middle.value
    state2 <= right.value
"""
    val s = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(source), ChirrtlForm,
      Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
    val manifest = SimulatorStateInventory.discover(s)
    val requested = s.copy(annotations = s.annotations ++ Seq(PhysicalClockCutPlanAnnotation,
      PhysicalClockCutWiringProbeAnnotation, SimulatorCheckpointInventoryAnnotation(s.circuit.main)))
    val files = PhysicalClockCutPlan.emit(requested, manifest, "integration fixture")
    val dep = parse(files.find(_.fileSuffix == ".physical-clock-dependencies.json").get.body)
    assert((dep \ "external_inputs_require_freeze") == JArray(List(JString("PhysicalTransportNetwork.reset"))))
    assert((dep \ "production_freeze_owners_bound") == JBool(false))
    assert(intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(s, manifest, separateAccessClock = true)
    }.getMessage.contains("requires a synthesized capture owner"))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("physical-transport-network"); Files.createDirectories(out)
      (Seq("reference.fir" -> s.circuit.serialize, "manifest.json" -> manifest) ++
        files.map(a => a.fileSuffix.stripPrefix(".") -> a.body)).foreach { case (name, text) =>
        Files.write(out.resolve(name), text.getBytes(UTF_8))
      }
    }
  }
}
