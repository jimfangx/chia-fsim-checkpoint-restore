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

class PhysicalClockCutPlanSpec extends AnyFlatSpec {
  private val source = """circuit PhysicalPlanTop :
  extmodule Native :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    defname = BUFGCE
  module Leaf :
    input clock : Clock
    input reset : UInt<1>
    input ce : UInt<1>
    output value : UInt<8>
    inst gate of Native
    gate.I <= clock
    gate.CE <= ce
    reg saved : UInt<8>, gate.O with :
      reset => (reset, UInt<8>(0))
    saved <= add(saved, UInt<8>(3))
    value <= saved
  module PhysicalPlanTop :
    input clock : Clock
    input reset : UInt<1>
    output a : UInt<8>
    output b : UInt<8>
    output count : UInt<8>
    reg cycles : UInt<8>, clock with :
      reset => (reset, UInt<8>(0))
    cycles <= add(cycles, UInt<8>(1))
    inst left of Leaf
    inst right of Leaf
    left.clock <= clock
    left.reset <= reset
    left.ce <= or(reset, bits(cycles, 0, 0))
    right.clock <= clock
    right.reset <= reset
    right.ce <= or(reset, bits(cycles, 1, 1))
    a <= left.value
    b <= right.value
    count <= cycles
"""
  private def lower(src: String = source): CircuitState = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(src), ChirrtlForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  private def plan(s: CircuitState): JValue = parse(PhysicalClockCutPlan.discover(s, SimulatorStateInventory.discover(s)))
  behavior.of("Native physical clock cut topology plan")

  it should "resolve shared hierarchical native gates and every register without changing the netlist" in {
    val s = lower(); val before = s.circuit.serialize
    val manifest = SimulatorStateInventory.discover(s)
    val doc = plan(s)
    assert((doc \ "common_source_clock") == JString("clock"))
    assert((doc \ "deployable_capture_image") == JBool(false))
    assert((doc \ "clock_rewrite_applied") == JBool(false))
    assert((doc \ "ce_dependency_closure_proven") == JBool(false))
    val gates = (doc \ "physical_gates").asInstanceOf[JArray].arr
    assert(gates.map(_ \ "hierarchy") == List(JString("PhysicalPlanTop.left.gate"), JString("PhysicalPlanTop.right.gate")))
    assert(gates.map(_ \ "index") == List(JInt(0), JInt(1)))
    assert(gates.forall(r => (r \ "logical_latch_state_id") != (r \ "metasim_output_state_id")))
    val consumers = (doc \ "sequential_consumers").asInstanceOf[JArray].arr
    assert(consumers.size == 3 && consumers.map(_ \ "hierarchy").toSet == Set(
      JString("PhysicalPlanTop.cycles"), JString("PhysicalPlanTop.left.saved"), JString("PhysicalPlanTop.right.saved")))
    assert(consumers.forall(r => (r \ "origin" \ "root_clock") == JString("clock")))
    assert((doc \ "state_clock_coverage").asInstanceOf[JArray].arr.size == 5)
    assert(s.circuit.serialize == before)
    val emitter = new SemanticCleanEmitter
    new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(s.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(emitter.getClass))))
    val emittedState = emitter.preparedState.get
    val emittedPlan = plan(emittedState)
    assert((emittedPlan \ "common_source_clock") == (doc \ "common_source_clock"))
    assert((emittedPlan \ "sequential_consumers").asInstanceOf[JArray].arr.size == consumers.size)
    val requested = s.copy(annotations = s.annotations ++ Seq(PhysicalClockCutPlanAnnotation,
      SimulatorCheckpointInventoryAnnotation(s.circuit.main)))
    val sv = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(s.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))).getEmittedCircuit.value
    val files = PhysicalClockCutPlan.emit(requested, manifest, sv)
    val binding = parse(files.find(_.fileSuffix == ".physical-clock-plan-emission.json").get.body)
    assert((binding \ "plan_sha256") == JString(SemanticStateInventory.sha256(SemanticStateInventory.canonical(doc))))
    assert((binding \ "emitted_verilog_sha256") == JString(SemanticStateInventory.sha256(sv)))
    intercept[IllegalArgumentException] { PhysicalClockCutPlan.emit(requested.copy(annotations = requested.annotations ++ files), manifest, sv) }
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("physical-plan"); Files.createDirectories(out)
      (Seq("reference.fir" -> before, "manifest.json" -> manifest, "reference.sv" -> sv) ++
        files.map(a => a.fileSuffix.stripPrefix(".") -> a.body)).foreach { case (name, value) =>
        Files.write(out.resolve(name), value.getBytes(UTF_8))
      }
    }
    assert(intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(s, manifest, separateAccessClock = true)
    }.getMessage.contains("requires a synthesized capture owner"))
  }

  it should "reject independent roots and cascaded physical sources" in {
    val independent = source.replace("    input clock : Clock\n    input reset : UInt<1>\n    output a", "    input clock : Clock\n    input other : Clock\n    input reset : UInt<1>\n    output a")
      .replace("right.clock <= clock", "right.clock <= other")
    assert(intercept[IllegalArgumentException](plan(lower(independent))).getMessage.contains("one functional root"))
    val cascaded = source.replace("    left.clock <= clock", "    inst outer of Native\n    outer.I <= clock\n    outer.CE <= UInt<1>(1)\n    left.clock <= outer.O")
    assert(intercept[IllegalArgumentException](plan(lower(cascaded))).getMessage.contains("cascaded physical source"))
  }

  it should "reject stale inventory computed clocks opaque owners and altered native definitions" in {
    val s = lower()
    intercept[IllegalArgumentException] { PhysicalClockCutPlan.discover(s, SimulatorStateInventory.discover(lower(
      source.replace("UInt<8>(3)", "UInt<8>(4)")))) }
    val computed = source.replace("right.clock <= clock", "right.clock <= asClock(bits(cycles, 2, 2))")
    intercept[IllegalArgumentException] { plan(lower(computed)) }
    val altered = s.copy(circuit = s.circuit.copy(modules = s.circuit.modules.map {
      case m: ExtModule => m.copy(params = Seq(StringParam("CE_TYPE", StringLit("ASYNC"))))
      case m => m
    }))
    intercept[IllegalArgumentException] { plan(altered) }
    val sourceOverride = s.copy(annotations = s.annotations :+ firrtl.transforms.BlackBoxInlineAnno(
      firrtl.annotations.ModuleName("Native", firrtl.annotations.CircuitName(s.circuit.main)), "BUFGCE.v", "module BUFGCE; endmodule"))
    intercept[IllegalArgumentException] { plan(sourceOverride) }
  }

  it should "audit sequential memory ports without treating an asynchronous reader as a clock owner" in {
    val mem = """    mem ram :
      data-type => UInt<8>
      depth => 4
      reader => r
      writer => w
      read-latency => 0
      write-latency => 1
      read-under-write => undefined
    ram.r.clk is invalid
    ram.r.en <= UInt<1>(1)
    ram.r.addr <= UInt<2>(0)
    ram.w.clk <= gate.O
    ram.w.en <= ce
    ram.w.addr <= UInt<2>(0)
    ram.w.mask <= UInt<1>(1)
    ram.w.data <= saved
"""
    val s = lower(source.replace("    value <= saved", mem + "    value <= saved"))
    val consumers = (plan(s) \ "sequential_consumers").asInstanceOf[JArray].arr
    assert(consumers.count(r => (r \ "kind") == JString("memory_port")) == 2)
    assert(consumers.filter(r => (r \ "kind") == JString("memory_port")).forall(
      r => (r \ "hierarchy").asInstanceOf[JString].s.endsWith(".ram.w")))
    val sync = lower(source.replace("    value <= saved", mem.replace("read-latency => 0", "read-latency => 1")
      .replace("ram.r.clk is invalid", "ram.r.clk <= gate.O") + "    value <= saved"))
    val syncPlan = plan(sync)
    assert((syncPlan \ "sequential_consumers").asInstanceOf[JArray].arr.count(
      r => (r \ "kind") == JString("memory_port")) == 4)
    assert((syncPlan \ "state_clock_coverage").asInstanceOf[JArray].arr.size == 9)
  }

  it should "validate request cardinality and preserve independent capture admission" in {
    val s = lower()
    val valid = Seq(PhysicalClockCutPlanAnnotation, SimulatorCheckpointInventoryAnnotation(s.circuit.main))
    PhysicalClockCutPlan.validateRequest(s.copy(annotations = valid))
    for (as <- Seq(valid :+ PhysicalClockCutPlanAnnotation, Seq(PhysicalClockCutPlanAnnotation),
      valid :+ SimulatorBankProbeAnnotation, valid :+ SemanticFAMEStateMapAnnotation,
      Seq(PhysicalClockCutPlanAnnotation, SimulatorCheckpointInventoryAnnotation("Leaf")))) {
      intercept[IllegalArgumentException] { PhysicalClockCutPlan.validateRequest(s.copy(annotations = as)) }
    }
    assert(PhysicalClockCutPlan.emit(s, "unused", "unused").isEmpty)
  }
}
