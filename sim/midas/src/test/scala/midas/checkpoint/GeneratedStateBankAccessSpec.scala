// See LICENSE for license details.
package midas.checkpoint

import scala.io.Source
import firrtl.{CircuitState, ChirrtlForm, LowForm, Parser}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class GeneratedStateBankAccessSpec extends AnyFlatSpec {
  private val resource = Source.fromResource("checkpoint/InventoryDUT.firrtl")
  private val rawSource = try resource.mkString finally resource.close()
  // Unlike the discovery fixture, initialize all storage through functional
  // ports so the independent RTL oracle never needs hierarchy deposits.
  private val source = rawSource.replace("reg harnessOnly : UInt<1>, clock",
    "reg harnessOnly : UInt<1>, clock with :\n      reset => (reset, UInt<1>(0))")
  private def lower(src: String): CircuitState =
    new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(src), ChirrtlForm)).copy(form = LowForm)
  private def generate(src: String = source): GeneratedStateBankAccess.Result = {
    val state = lower(src)
    GeneratedStateBankAccess.generate(state, SemanticStateInventory.discover(state, state.circuit.main))
  }
  behavior.of("Generated lowered-state bank endpoint")

  it should "specialize repeated instances and bind every actual register and memory exactly once" in {
    val result = generate()
    assert(result.entries.size == 9)
    val doc = parse(result.binding)
    val entries = (doc \ "state").asInstanceOf[JArray].arr
    assert(entries.size == 9)
    assert(entries.map(e => (e \ "storage_hierarchy").values).distinct.size == 9)
    assert(result.state.circuit.modules.size == 4)
    assert(result.state.annotations.exists(_.isInstanceOf[firrtl.transforms.CombinationalPath]))
    val leaves = result.state.circuit.modules.collect { case m: firrtl.ir.Module
      if m.body.serialize.contains("reg saved") => m.name }
    assert(leaves.size == 2 && leaves.distinct.size == 2)
    assert(result.state.circuit.serialize.contains("reader => ckpt_read"))
    assert(result.state.circuit.serialize.contains("ckpt_freeze"))
    // Both inventory namespaces may bind the same concrete lowered storage,
    // but their StateIDs and exact manifest hashes remain independent.
    val state = lower(source)
    val simulator = GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state))
    assert(simulator.entries.size == 9)
    assert(simulator.entries.map(_.stateID).toSet.intersect(result.entries.map(_.stateID).toSet).isEmpty)
    val emitted = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(result.state.copy(annotations = Seq(
        firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter]))))
    val sv = emitted.annotations.collectFirst { case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value }.get
    assert(sv.contains("ckpt_resp_valid") && sv.contains("ckpt_saved_data"))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      def save(name: String, value: String): Unit = java.nio.file.Files.write(
        java.nio.file.Paths.get(dir, name), value.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      save("generated-bank-input.fir", state.circuit.serialize)
      save("generated-bank.sv", sv)
      save("generated-bank.fir", result.state.circuit.serialize)
      save("generated-bank-binding.json", result.binding)
      save("generated-bank-semantic.json", SemanticStateInventory.discover(state, state.circuit.main))
      save("generated-bank-simulator-binding.json", simulator.binding)
      save("generated-bank-simulator.json", SimulatorStateInventory.discover(state))
    }
  }

  it should "gate every functional register reset/update and memory write behind the common cut" in {
    val result = generate()
    val text = result.state.circuit.serialize
    assert(text.contains("mux(ckpt_freeze, saved"))
    assert(text.contains("mux(ckpt_freeze, counter"))
    assert(text.contains("not(ckpt_freeze)"))
    // The endpoint response storage is outside functional freeze and latches
    // complete data/error at the accepted request, not when the response drains.
    assert(text.contains("ckpt_saved_data <= mux(ckpt_accept"))
    assert(text.contains("ckpt_saved_error <= mux(ckpt_accept"))
    assert(text.contains("ckpt_resp_data <= ckpt_saved_data"))
  }

  it should "reject stale inventories and incomplete reordered or duplicate selectors" in {
    val state = lower(source)
    val manifest = SemanticStateInventory.discover(state, state.circuit.main)
    val valid = generate().entries
    for (bindings <- Seq(valid.drop(1), valid.reverse,
                         valid.updated(1, valid(1).copy(bank = valid.head.bank, slot = valid.head.slot)))) {
      intercept[IllegalArgumentException](GeneratedStateBankAccess.generate(state, manifest, Some(bindings)))
    }
    intercept[IllegalArgumentException](GeneratedStateBankAccess.generate(
      lower(source.replace("UInt<9>(0)", "UInt<9>(1)")), manifest))
  }

  it should "include synchronous and readwrite pipeline owners and reject wide unchunked state" in {
    val sync = lower(source.replace("read-latency => 0", "read-latency => 1"))
    val manifest = SimulatorStateInventory.discover(sync)
    assert(GeneratedStateBankAccess.generate(sync, manifest).entries.size == 10)
    val rwSource = source.replace("reader => r", "readwriter => r")
      .replace("read-latency => 0", "read-latency => 1")
      .replace("read-under-write => undefined", "read-under-write => old")
      .replace("sram.r.en <= UInt<1>(1)", "sram.r.en <= UInt<1>(1)\n    sram.r.wmode <= UInt<1>(0)\n    sram.r.wmask <= UInt<1>(1)\n    sram.r.wdata <= data")
      .replace("sram.r.data", "sram.r.rdata")
    val rw = lower(rwSource)
    assert(GeneratedStateBankAccess.generate(rw, SimulatorStateInventory.discover(rw)).entries.size == 10)
    val wide = source.replace("reg harnessOnly : UInt<1>", "reg harnessOnly : UInt<65>")
    assert(intercept[IllegalArgumentException](generate(wide)).getMessage.contains("chunking"))
  }

  it should "translate initialization and reject unknown annotations and independently clocked children" in {
    val state = lower(source)
    val manifest = SemanticStateInventory.discover(state, state.circuit.main)
    val target = firrtl.annotations.ReferenceTarget("Harness", "InventoryDUT", Nil, "sram", Nil)
    val initialized = state.copy(annotations = Seq(
      firrtl.annotations.MemoryFileInlineAnnotation(target, "fixture-state.hex")))
    val result = GeneratedStateBankAccess.generate(initialized, manifest)
    val annotations = result.state.annotations.collect { case a: firrtl.annotations.MemoryFileInlineAnnotation => a }
    assert(annotations.size == 1 && annotations.head.target.module != target.module)
    assert(annotations.head.filename == "fixture-state.hex")
    val unknown = state.copy(annotations = Seq(firrtl.options.TargetDirAnnotation("unknown-output")))
    assert(intercept[IllegalArgumentException](GeneratedStateBankAccess.generate(unknown, manifest))
      .getMessage.contains("annotations require explicit clone/rename translation"))
    val clocks = source.replace("  module Harness :\n    input clock : Clock",
      "  module Harness :\n    input clock : Clock\n    input otherClock : Clock")
      .replace("dut.clock <= clock", "dut.clock <= otherClock")
    intercept[IllegalArgumentException](generate(clocks))
  }

  it should "reject side effects that have no frozen-owner contract" in {
    for (effect <- Seq("printf(clock, wen, \"side effect %d\", data)",
                        "stop(clock, wen, 1)",
                        "assert(clock, UInt<1>(1), wen, \"side effect assertion\")")) {
      val effects = source.replace("    result <= dut.ram", s"    $effect\n    result <= dut.ram")
      assert(intercept[IllegalArgumentException](generate(effects)).getMessage.contains("ungated side effect"))
    }
    val analog = source.replace("  module Harness :\n    input clock : Clock",
      "  module Harness :\n    input clock : Clock\n    input analogPin : Analog<1>")
    assert(intercept[IllegalArgumentException](generate(analog)).getMessage.contains("analog attachment"))
  }

  it should "reject reserved names and derived clocks without returning partial hardware" in {
    assert(intercept[IllegalArgumentException](generate(source.replace("harnessOnly", "ckpt_owner")))
      .getMessage.contains("reserved ckpt_"))
    intercept[IllegalArgumentException](generate(source.replace("reg harnessOnly : UInt<1>, clock",
      "reg harnessOnly : UInt<1>, asClock(asUInt(reset))")))
  }
}
