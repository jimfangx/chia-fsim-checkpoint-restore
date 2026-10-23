// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser, WDefInstance}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class MultipleFunctionalClocksSpec extends AnyFlatSpec {
  private val source = """circuit MultiClockStorage :
  module DomainStorage :
    input a : Clock
    input b : Clock
    input reset_a : UInt<1>
    input reset_b : UInt<1>
    input data : UInt<17>
    input address : UInt<2>
    input wen : UInt<1>
    output result_a : UInt<9>
    output result_b : UInt<17>
    output result_ram : UInt<13>
    reg saved_a : UInt<9>, a with :
      reset => (reset_a, UInt<9>(0))
    reg saved_b : UInt<17>, b with :
      reset => (reset_b, UInt<17>(0))
    mem ram :
      data-type => UInt<13>
      depth => 4
      read-latency => 1
      write-latency => 1
      reader => r
      writer => w
      read-under-write => old
    ram.r.clk <= a
    ram.r.en <= UInt<1>(1)
    ram.r.addr <= address
    ram.w.clk <= b
    ram.w.en <= wen
    ram.w.addr <= address
    ram.w.data <= bits(data, 12, 0)
    ram.w.mask <= UInt<1>(1)
    saved_a <= bits(data, 8, 0)
    saved_b <= data
    result_a <= saved_a
    result_b <= saved_b
    result_ram <= ram.r.data
  module MultiClockStorage :
    input clock_a : Clock
    input clock_b : Clock
    input reset_a : UInt<1>
    input reset_b : UInt<1>
    input data : UInt<17>
    input address : UInt<2>
    input wen : UInt<1>
    output result : UInt<9>
    output left_a : UInt<9>
    output left_b : UInt<17>
    output left_ram : UInt<13>
    output right_a : UInt<9>
    output right_b : UInt<17>
    output right_ram : UInt<13>
    reg saved : UInt<9>, clock_a with :
      reset => (reset_a, UInt<9>(0))
    saved <= bits(data, 8, 0)
    result <= saved
    inst left of DomainStorage
    inst right of DomainStorage
    left.a <= clock_a
    left.b <= clock_b
    right.a <= clock_b
    right.b <= clock_a
    left.reset_a <= reset_a
    left.reset_b <= reset_b
    right.reset_a <= reset_b
    right.reset_b <= reset_a
    left.data <= data
    right.data <= data
    left.address <= address
    right.address <= address
    left.wen <= wen
    right.wen <= wen
    left_a <= left.result_a
    left_b <= left.result_b
    left_ram <= left.result_ram
    right_a <= right.result_a
    right_b <= right.result_b
    right_ram <= right.result_ram
"""
  private def lower(text: String) = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(text), ChirrtlForm)).copy(form = LowForm)
  private def emit(state: CircuitState): String = new Compiler(
    Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(
      state.copy(annotations = state.annotations ++ Seq(
        firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation,
        firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))).getEmittedCircuit.value

  behavior.of("Explicit multiple direct functional clock domains")
  it should "preserve all clock owners and swapped repeated-child domains with independent transport" in {
    val state = lower(source)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    assert(result.entries.size == 9 && result.entries.map(_.depth).sum == 15)
    val domains = (parse(result.binding) \ "functional_clock_domains").asInstanceOf[JArray].arr
    assert(domains.size == 6)
    assert(domains.exists(row => (row \ "clock_hierarchy") == JString("MultiClockStorage.right_.a") &&
      (row \ "root_clock") == JString("clock_b")), domains.toString)
    val reference = state.copy(circuit = state.circuit.copy(main = "OriginalMultiClockStorage",
      modules = state.circuit.modules.map {
        case m: Module =>
          def rename(s: Statement): Statement = s match {
            case i: WDefInstance => i.copy(module = "Original" + i.module)
            case x => x.mapStmt(rename)
          }
          m.copy(name = "Original" + m.name, body = rename(m.body))
        case _ => fail("opaque reference")
      }))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("multiple-clocks"); Files.createDirectories(out)
      Seq("generated-bank-input.fir" -> state.circuit.serialize,
        "generated-bank.fir" -> result.state.circuit.serialize,
        "generated-bank.sv" -> emit(result.state), "reference.sv" -> emit(reference),
        "generated-bank-manifest.json" -> manifest, "generated-bank-binding.json" -> result.binding
      ).foreach { case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8)) }
    }
  }
  it should "reject missing reordered changed and transport-owned functional domain bindings" in {
    val state = lower(source)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    val doc = parse(result.binding)
    val domains = (doc \ "functional_clock_domains").asInstanceOf[JArray].arr
    for (bad <- Seq(JNothing, JArray(domains.tail), JArray(domains.reverse),
      JArray(domains.map(_.transformField { case ("root_clock", _) => "root_clock" -> JString("ckpt_clock") })))) {
      val binding = doc.transformField { case ("functional_clock_domains", _) => "functional_clock_domains" -> bad }
      assert(intercept[IllegalArgumentException] {
        midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(
          binding = SemanticStateInventory.canonical(binding)), manifest)
      }.getMessage.contains("functional clock domains"))
    }
  }
  it should "reject implicit transport selection derived clocks and derived memory clocks" in {
    val state = lower(source)
    intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state))
    }
    for (bad <- Seq(source.replace("right.a <= clock_b", "right.a <= asClock(bits(data, 0, 0))"),
      source.replace("reg saved_b : UInt<17>, b", "reg saved_b : UInt<17>, asClock(bits(data, 0, 0))"),
      source.replace("ram.w.clk <= b", "ram.w.clk <= asClock(bits(data, 0, 0))"))) {
      val input = lower(bad)
      intercept[IllegalArgumentException] {
        GeneratedStateBankAccess.generate(input, SimulatorStateInventory.discover(input), separateAccessClock = true)
      }
    }
  }
}
