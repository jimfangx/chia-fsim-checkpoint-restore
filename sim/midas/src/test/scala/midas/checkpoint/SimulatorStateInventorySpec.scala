// See LICENSE for license details.
package midas.checkpoint

import scala.io.Source
import firrtl.{CircuitState, ChirrtlForm, LowForm, Parser}
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.annotations.{CircuitName, ModuleName}
import firrtl.transforms.BlackBoxInlineAnno
import midas.passes.DefineAbstractClockGate
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class SimulatorStateInventorySpec extends AnyFlatSpec {
  private val resource = Source.fromResource("checkpoint/InventoryDUT.firrtl")
  private val source = try resource.mkString finally resource.close()
  private def lower(src: String): CircuitState =
    new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(src), ChirrtlForm)).copy(form = LowForm)
  private def emitted(src: String): CircuitState =
    new firrtl.SystemVerilogEmitter().transforms.foldLeft(
      new Compiler(Seq(Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(lower(src))) {
      case (s, transform) => transform.runTransform(s)
    }
  private def entries(state: CircuitState): List[JValue] =
    (parse(SimulatorStateInventory.discover(state)) \ "state").asInstanceOf[JArray].arr

  behavior.of("Final generated simulator state inventory")

  it should "include all top/harness state and repeated instances with independent Level-1 identities" in {
    val state = lower(source)
    val es = entries(state)
    assert(es.size == 9)
    assert(es.exists(e => (e \ "hierarchy") == JString("Harness.harnessOnly")))
    val semantic = (parse(SemanticStateInventory.discover(lower(source), "Harness.dut")) \ "state")
      .asInstanceOf[JArray].arr
    assert(es.map(e => (e \ "state_id").values).toSet.intersect(
      semantic.map(e => (e \ "state_id").values).toSet).isEmpty)
    assert(SimulatorStateInventory.discover(state) == SimulatorStateInventory.discover(state))
    assert(es.map(e => (e \ "state_id").values).distinct.size == 9)
    val doc = parse(SimulatorStateInventory.discover(state))
    assert((doc \ "external_runtime_required") == JBool(true))
    assert((doc \ "instances").asInstanceOf[JArray].arr.size == 4)
    assert(SimulatorStateInventory(state) == state)
    val captured = SimulatorStateInventory(state.copy(annotations =
      state.annotations :+ SimulatorCheckpointInventoryAnnotation("Harness")))
    assert(captured.circuit == state.circuit)
    val files = captured.annotations.collect { case a: midas.stage.GoldenGateOutputFileAnnotation =>
      a.fileSuffix -> a.body }.toMap
    assert(files(".simulator-reference.fir") == state.circuit.serialize)
    assert(files(".simulator-state.json") == SimulatorStateInventory.discover(state))
  }

  it should "inventory synchronous memory pipeline registers introduced by emitter prerequisites" in {
    val src = source.replace("read-latency => 0", "read-latency => 1")
    assert(entries(lower(src)).exists(e => (e \ "adapter") == JString("firrtl-emitter-memory-pipeline-v1")))
    val state = emitted(src)
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      java.nio.file.Files.write(java.nio.file.Paths.get(dir, "simulator-pipeline-reference.fir"),
        state.circuit.serialize.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    }
    val es = entries(state)
    assert(es.exists(e => (e \ "hierarchy").values.toString.contains("pipe")))
    assert(es.count(e => (e \ "kind") == JString("memory")) == 1)
    assert((es.find(e => (e \ "kind") == JString("memory")).get \ "depth") == JInt(8))
  }

  it should "account exactly for emitter passthrough read and readwrite storage" in {
    for ((port, policy) <- Seq(("reader", "old"), ("reader", "new"), ("reader", "undefined"),
                              ("readwriter", "old"), ("readwriter", "undefined"))) {
      val readwrite = port == "readwriter"
      val src = s"""circuit Top :
  module Top :
    input clock : Clock
    input enable : UInt<1>
    input address : UInt<3>
    input data : UInt<9>
    input write : UInt<1>
    output result : UInt<9>
    mem ram :
      data-type => UInt<9>
      depth => 5
      read-latency => 1
      write-latency => 1
      $port => p
      ${if (readwrite) "" else "writer => w"}
      read-under-write => $policy
    ram.p.clk <= clock
    ram.p.en <= enable
    ram.p.addr <= address
    ${if (readwrite) "ram.p.wmode <= write\n    ram.p.wmask <= UInt<1>(1)\n    ram.p.wdata <= data"
      else "ram.w.clk <= clock\n    ram.w.en <= write\n    ram.w.addr <= address\n    ram.w.mask <= UInt<1>(1)\n    ram.w.data <= data"}
    result <= ram.p.${if (readwrite) "rdata" else "data"}
"""
      val initial = lower(src).copy(annotations = Seq(
        firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation,
        firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter]),
      ))
      val emittedState = new Compiler(Seq(Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(initial)
      val state = new firrtl.SystemVerilogEmitter().transforms.foldLeft(emittedState) {
        case (s, transform) => transform.runTransform(s)
      }
      val es = entries(state)
      val field = if (readwrite) "rdata" else if (policy == "old") "data" else "addr"
      assert(es.size == 2)
      val pipeline = es.find(e => (e \ "hierarchy") == JString(s"Top.ram_p_$field")).get
      assert((pipeline \ "width") == JInt(if (field == "addr") 3 else 9))
      assert((pipeline \ "adapter") == JString("firrtl-emitter-memory-pipeline-v1"))
      sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
        val base = java.nio.file.Paths.get(dir, s"passthrough-$port-$policy")
        def save(suffix: String, text: String): Unit = java.nio.file.Files.write(
          java.nio.file.Paths.get(base.toString + suffix), text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        save(".fir", state.circuit.serialize)
        save(".json", SimulatorStateInventory.discover(state))
        save(".sv", emittedState.annotations.collectFirst {
          case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value
        }.get)
      }
    }
    val complex = lower(source.replace("read-latency => 0", "read-latency => 2"))
    intercept[IllegalArgumentException](SimulatorStateInventory.discover(complex))
    intercept[IllegalArgumentException](SimulatorStateInventory.discover(
      lower(source.replace("write-latency => 1", "write-latency => 2"))))
  }

  it should "reject opaque state and altered clock-gate implementations instead of guessing by name" in {
    val state = lower(source)
    val gate = DefineAbstractClockGate.blackbox
    // Replace a reachable leaf with an opaque module: no silent omission.
    val opaque = state.copy(circuit = state.circuit.copy(modules = state.circuit.modules.map {
      case m: firrtl.ir.Module if m.name == "Leaf" =>
        firrtl.ir.ExtModule(m.info, m.name, m.ports, "OpaqueLeaf", Nil)
      case m => m
    }))
    assert(intercept[IllegalArgumentException](SimulatorStateInventory.discover(opaque))
      .getMessage.contains("opaque external module"))
    // A reachable exact adapter is deliberately separate from FIRRTL registers.
    val gateSource = """circuit Top :
  module Top :
    input clock : Clock
    input en : UInt<1>
    output out : Clock
    inst gate of AbstractClockGate
    gate.I <= clock
    gate.CE <= en
    out <= gate.O
  extmodule AbstractClockGate :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    defname = AbstractClockGate
"""
    val s = lower(gateSource)
    def inline(text: String) = s.copy(annotations = s.annotations :+
      BlackBoxInlineAnno(ModuleName(gate.name, CircuitName("Top")), "AbstractClockGate.v", text))
    intercept[IllegalArgumentException](SimulatorStateInventory.discover(s))
    intercept[IllegalArgumentException](SimulatorStateInventory.discover(inline(DefineAbstractClockGate.impl + "\n")))
    val es = entries(inline(DefineAbstractClockGate.impl))
    assert(es.size == 1 && (es.head \ "hierarchy") == JString("Top.gate.enable"))
    assert((es.head \ "adapter") == JString("abstract-clock-gate-v1"))
  }

  it should "reject non-bit state and async resets while retaining wide signed state" in {
    val wide = lower(source.replace("reg harnessOnly : UInt<1>", "reg harnessOnly : SInt<257>")
      .replace("harnessOnly <= not(harnessOnly)", "harnessOnly <= SInt<257>(0)"))
    val e = entries(wide).find(e => (e \ "hierarchy") == JString("Harness.harnessOnly")).get
    assert((e \ "width") == JInt(257) && (e \ "signed") == JBool(true))
    intercept[IllegalArgumentException](SimulatorStateInventory.discover(
      lower(source.replace("input reset : UInt<1>", "input reset : AsyncReset"))))
  }
}
