// See LICENSE for license details.
package midas.checkpoint

import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class PhysicalClockStateBankSpec extends AnyFlatSpec {
  private def input: CircuitState = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse("""circuit Top :
  extmodule Native :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    defname = BUFGCE
  module Leaf :
    input clock : Clock
    input enable : UInt<1>
    output value : UInt<8>
    inst gate of Native
    gate.I <= clock
    gate.CE <= enable
    reg count : UInt<8>, gate.O
    count <= add(count, UInt<1>(1))
    value <= count
  module Top :
    input clock : Clock
    input enable : UInt<1>
    output left_value : UInt<8>
    output right_value : UInt<8>
    inst left of Leaf
    inst right of Leaf
    left.clock <= clock
    right.clock <= clock
    left.enable <= enable
    right.enable <= not(enable)
    left_value <= left.value
    right_value <= right.value
"""), ChirrtlForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)

  behavior.of("Physical CE bank companion")
  it should "bind repeated native gates to distinct physical IDs and slots without admitting capture or restore" in {
    val s = input
    val manifest = SimulatorStateInventory.discover(s)
    val plan = PhysicalClockCutPlan.discover(s, manifest)
    val deps = PhysicalClockCutDependencies.discover(s, manifest, plan)
    val files = PhysicalClockStateBank.emit(s, manifest, plan, deps)
    val doc = parse(files.head.body)
    val entries = (doc \ "state").asInstanceOf[JArray].arr
    val gates = (parse(plan) \ "physical_gates").asInstanceOf[JArray].arr
    assert(entries.size == 2)
    assert(entries.map(_ \ "slot") == List(JInt(0), JInt(1)))
    assert(entries.map(_ \ "state_id").distinct.size == 2)
    entries.zip(gates).foreach { case (e, g) =>
      assert((e \ "state_id") == (g \ "logical_latch_state_id"))
      assert((e \ "state_id") != (g \ "metasim_output_state_id"))
      assert((e \ "restore_adapter") == JString("unsupported"))
      assert((e \ "bank") == JInt(255))
    }
    assert((doc \ "endpoint_sha256") == JString(SemanticStateInventory.sha256(files(1).body)))
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").foreach(k => assert((doc \ k) == JBool(false)))
    assert(intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(s, manifest, separateAccessClock = true)
    }.getMessage.contains("requires a synthesized capture owner"))
  }

  it should "reject stale topology and original CE dependency documents before emitting a bank" in {
    val s = input
    val manifest = SimulatorStateInventory.discover(s)
    val plan = PhysicalClockCutPlan.discover(s, manifest)
    val deps = PhysicalClockCutDependencies.discover(s, manifest, plan)
    Seq((plan + " ", deps), (plan, deps + " ")).foreach { case (p, d) =>
      assert(intercept[IllegalArgumentException] {
        PhysicalClockStateBank.emit(s, manifest, p, d)
      }.getMessage.contains("stale topology"))
    }
    assert(intercept[IllegalArgumentException] {
      PhysicalClockStateBank.emit(s.copy(annotations = s.annotations :+
        GoldenGateOutputFileAnnotation("stale", ".physical-clock-state-bank.json")), manifest, plan, deps)
    }.getMessage.contains("existing state bank output"))
  }
}
