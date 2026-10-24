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

class PhysicalClockCutDependenciesSpec extends AnyFlatSpec {
  private val source = """circuit Top :
  extmodule Native :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    defname = BUFGCE
  module Leaf :
    input clock : Clock
    input external : UInt<1>
    output ce : UInt<1>
    reg saved : UInt<1>, clock
    saved <= external
    ce <= or(saved, external)
  module Top :
    input clock : Clock
    input control : UInt<1>
    output value : UInt<1>
    inst left of Leaf
    inst right of Leaf
    left.clock <= clock
    right.clock <= clock
    left.external <= control
    right.external <= UInt<1>(0)
    inst gate of Native
    gate.I <= clock
    gate.CE <= or(left.ce, right.ce)
    reg result : UInt<1>, gate.O
    result <= control
    value <= result
"""
  private def lower(src: String = source): CircuitState = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(src), ChirrtlForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  private def discover(s: CircuitState): JValue = {
    val inv = SimulatorStateInventory.discover(s)
    parse(PhysicalClockCutDependencies.discover(s, inv, PhysicalClockCutPlan.discover(s, inv)))
  }
  private def terminals(d: JValue): List[JValue] =
    (d \ "physical_gates").asInstanceOf[JArray].arr.flatMap(r => (r \ "terminals").asInstanceOf[JArray].arr)
  private def retain(s: CircuitState, name: String): Unit = sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
    val out = Paths.get(dir).resolve("physical-ce-" + name); Files.createDirectories(out)
    val manifest = SimulatorStateInventory.discover(s)
    val plan = PhysicalClockCutPlan.discover(s, manifest)
    Seq("reference.fir" -> s.circuit.serialize, "manifest.json" -> manifest, "plan.json" -> plan,
      "dependencies.json" -> PhysicalClockCutDependencies.discover(s, manifest, plan)).foreach { case (file, body) =>
      Files.write(out.resolve(file), body.getBytes(UTF_8))
    }
  }

  behavior.of("Physical CE dependency prerequisites")
  it should "follow repeated child outputs and input substitutions to distinct complete clocked owners" in {
    val s = lower(); val before = s.circuit.serialize
    val d = discover(s)
    assert(terminals(d).map(_ \ "hierarchy").toSet == Set("Top.control", "Top.left.saved", "Top.right.saved").map(JString(_)))
    assert((d \ "external_inputs_require_freeze") == JArray(List(JString("Top.control"))))
    assert(terminals(d).filter(r => (r \ "kind") == JString("clocked_state")).forall(r =>
      (r \ "clock_consumers").asInstanceOf[JArray].arr.nonEmpty && (r \ "state_id").asInstanceOf[JString].s.length == 64))
    assert((d \ "production_freeze_owners_bound") == JBool(false))
    assert((d \ "deployable_capture_image") == JBool(false))
    assert(s.circuit.serialize == before)
    // Use the final emitter's internal primops and packed connectors too.
    val emitter = new SemanticCleanEmitter
    new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(s.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(emitter.getClass))))
    assert(terminals(discover(emitter.preparedState.get)) == terminals(d))
    retain(emitter.preparedState.get, "input")
  }

  it should "trace asynchronous memory reads through address and enable but stop at actual synchronous pipeline state" in {
    val mem = """    mem ram :
      data-type => UInt<1>
      depth => 2
      reader => r
      writer => w
      read-latency => 0
      write-latency => 1
      read-under-write => undefined
    ram.r.clk is invalid
    ram.r.en <= external
    ram.r.addr <= external
    ram.w.clk <= clock
    ram.w.en <= external
    ram.w.addr <= external
    ram.w.data <= saved
    ram.w.mask <= UInt<1>(1)
    ce <= ram.r.data
"""
    val s = lower(source.replace("    ce <= or(saved, external)\n", mem))
    assert(terminals(discover(s)).map(_ \ "hierarchy").toSet == Set("Top.control", "Top.left.ram", "Top.right.ram").map(JString(_)))
    retain(s, "memory-async")
    // Preserve simple emitter memories rather than lowering them to address
    // registers: the generated inventory names their implicit pipeline owners.
    val sync = s.copy(circuit = s.circuit.copy(modules = s.circuit.modules.map {
      case m: Module if m.name == "Leaf" =>
        def change(x: Statement): Statement = x match {
          case d: DefMemory => d.copy(readLatency = 1)
          case i: IsInvalid if i.expr.serialize == "ram.r.clk" =>
            Connect(NoInfo, i.expr, firrtl.WRef("clock", ClockType))
          case x => x.mapStmt(change)
        }
        m.copy(body = change(m.body))
      case m => m
    }))
    val d = discover(sync)
    assert(terminals(d).forall(r => (r \ "kind") == JString("clocked_state")))
    assert(terminals(d).count(r => (r \ "adapter") == JString("firrtl-emitter-memory-pipeline-v1")) == 2)
    retain(sync, "memory-sync-new")
    val old = sync.copy(circuit = sync.circuit.copy(modules = sync.circuit.modules.map {
      case m: Module =>
        def change(x: Statement): Statement = x match {
          case d: DefMemory => d.copy(readUnderWrite = ReadUnderWrite.Old)
          case x => x.mapStmt(change)
        }
        m.copy(body = change(m.body))
      case m => m
    }))
    assert(terminals(discover(old)).size == 2)
    assert(terminals(discover(old)).forall(r => (r \ "hierarchy").asInstanceOf[JString].s.endsWith("_r_data")))
    retain(old, "memory-sync-old")
    val rwSource = source.replace("    ce <= or(saved, external)\n", mem.replace("      reader => r\n      writer => w", "      readwriter => rw")
      .replace("read-latency => 0", "read-latency => 1")
      .replace("    ram.r.clk is invalid\n    ram.r.en <= external\n    ram.r.addr <= external\n", "")
      .replace("ram.w.", "ram.rw.").replace("ram.rw.data", "ram.rw.wdata").replace("ram.rw.mask", "ram.rw.wmask")
      .replace("    ce <= ram.r.data", "    ram.rw.wmode <= saved\n    ce <= ram.rw.rdata"))
    val rwLowered = lower(rwSource)
    // Native LowForm may expand readwriters. Both forms must retain concrete
    // state identity and cannot collapse repeated instance pipelines.
    retain(rwLowered, "memory-readwrite-lowered")
    val working = firrtl.passes.ToWorkingIR.run(Parser.parse(rwSource))
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed)
    firrtl.passes.CheckFlows.run(flowed)
    val rwClocked = CircuitState(flowed, LowForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))
    assert(terminals(discover(rwClocked)).size == 2)
    assert(terminals(discover(rwClocked)).forall(r => (r \ "hierarchy").asInstanceOf[JString].s.endsWith("_rw_rdata")))
    retain(rwClocked, "memory-readwrite-emitter")
  }

  it should "reject combinational loops invalid CE and oscillating clocks used as CE data" in {
    val s = lower()
    def replaceCE(value: Option[Expression]): CircuitState = s.copy(circuit = s.circuit.copy(modules = s.circuit.modules.map {
      case m: Module if m.name == "Top" =>
        def change(x: Statement): Statement = x match {
          case c: Connect if c.loc.serialize == "gate.CE" => value match {
            case Some(e) => c.copy(expr = e)
            case None => IsInvalid(NoInfo, c.loc)
          }
          case x => x.mapStmt(change)
        }
        m.copy(body = change(m.body))
      case m => m
    }))
    // Inventory/topology remains complete; neither a valid clock topology nor
    // the masked-zero CE may discharge the original combinational dependency.
    val cycle = replaceCE(Some(firrtl.WSubField(firrtl.WRef("gate"), "CE", UIntType(IntWidth(1)))))
    assert(intercept[IllegalArgumentException](discover(cycle)).getMessage.contains("combinational cycle"))
    intercept[IllegalArgumentException](discover(replaceCE(None)))
    val clock = replaceCE(Some(DoPrim(firrtl.PrimOps.AsUInt, Seq(firrtl.WRef("clock", ClockType)), Nil, UIntType(IntWidth(1)))))
    assert(intercept[IllegalArgumentException](discover(clock)).getMessage.contains("oscillating root clock"))
    val nativeOutput = replaceCE(Some(DoPrim(firrtl.PrimOps.AsUInt, Seq(
      firrtl.WSubField(firrtl.WRef("gate"), "O", ClockType)), Nil, UIntType(IntWidth(1)))))
    assert(intercept[IllegalArgumentException](discover(nativeOutput)).getMessage.contains("physical clock used as data"))
    val inv = SimulatorStateInventory.discover(s)
    intercept[IllegalArgumentException] { PhysicalClockCutDependencies.discover(s, inv, "{}") }
  }
}
