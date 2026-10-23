// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations.{CircuitName, ModuleName}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.transforms.BlackBoxInlineAnno
import midas.passes.DefineAbstractClockGate
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class RegisteredClockGateBankSpec extends AnyFlatSpec {
  private val source = """circuit GateStorage :
  extmodule AbstractClockGate :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    defname = AbstractClockGate
  module GateStorage :
    input clock : Clock
    input reset : UInt<1>
    input ce : UInt<1>
    input data : UInt<17>
    input address : UInt<2>
    input wen : UInt<1>
    output result : UInt<17>
    output ram_result : UInt<13>
    inst gate of AbstractClockGate
    gate.I <= clock
    gate.CE <= ce
    reg saved : UInt<17>, gate.O with :
      reset => (reset, UInt<17>(0))
    saved <= data
    mem ram :
      data-type => UInt<13>
      depth => 4
      read-latency => 1
      write-latency => 1
      reader => r
      writer => w
      read-under-write => old
    node read_clock = validif(wen, gate.O)
    ram.r.clk <= read_clock
    ram.r.en <= mux(wen, UInt<1>(1), UInt<1>(0))
    ram.r.addr <= address
    ram.w.clk <= validif(wen, gate.O)
    ram.w.en <= wen
    ram.w.addr <= address
    ram.w.mask <= UInt<1>(1)
    ram.w.data <= bits(data, 12, 0)
    result <= saved
    ram_result <= ram.r.data
"""
  private def lower(text: String): CircuitState = {
    val inline = BlackBoxInlineAnno(ModuleName("AbstractClockGate", CircuitName("GateStorage")),
      "AbstractClockGate.v", DefineAbstractClockGate.impl)
    new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(text), ChirrtlForm,
      Seq(inline, firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  }
  private def emit(state: CircuitState): String = new Compiler(
    Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(
      state.copy(annotations = state.annotations ++ Seq(
        firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation,
        firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))).getEmittedCircuit.value

  behavior.of("Exact registered-enable clock-gate bank adapter")
  it should "capture the real enable owner without replacing or ungating downstream clocks" in {
    val state = lower(source)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    assert(result.entries.size == 4 && result.entries.map(_.depth).sum == 7)
    val doc = parse(result.binding)
    assert((doc \ "clock_gate_adapters") == GeneratedClockGateAdapter.binding(Set("GateStorage.gate")))
    assert((doc \ "functional_clock_domains").asInstanceOf[JArray].arr.exists(row =>
      (row \ "clock_hierarchy") == JString("GateStorage.gate.O") &&
      (row \ "gate_chain") == JArray(List(JString("GateStorage.gate")))))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("registered-gate"); Files.createDirectories(out)
      // Ordinary original RTL is emitted with the unmodified original gate model.
      // Rename only the top; the concrete generated gate is a specialized module.
      val reference = state.copy(circuit = state.circuit.copy(main = "OriginalGateStorage",
        modules = state.circuit.modules.map {
          case m: Module if m.name == "GateStorage" => m.copy(name = "OriginalGateStorage")
          case m => m
        }), annotations = Nil)
      Seq("generated-bank-input.fir" -> state.circuit.serialize,
        "generated-bank.fir" -> result.state.circuit.serialize,
        "generated-bank.sv" -> emit(result.state), "reference.sv" -> emit(reference),
        "AbstractClockGate.v" -> DefineAbstractClockGate.impl,
        "generated-bank-manifest.json" -> manifest, "generated-bank-binding.json" -> result.binding
      ).foreach { case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8)) }
    }
  }
  it should "reject missing altered duplicated and foreign-circuit implementations" in {
    val state = lower(source)
    val manifest = SimulatorStateInventory.discover(state)
    val inline = state.annotations.collectFirst { case a: BlackBoxInlineAnno => a }.get
    val rest = state.annotations.filterNot(_.isInstanceOf[BlackBoxInlineAnno])
    for (bad <- Seq(rest, rest ++ Seq(inline, inline), rest :+ inline.copy(text =
      inline.text.replace("posedge I", "negedge I")), rest :+ inline.copy(target = inline.target.copy(circuit = CircuitName("Other"))))) {
      intercept[IllegalArgumentException] {
        GeneratedStateBankAccess.generate(state.copy(annotations = bad), manifest, separateAccessClock = true)
      }
    }
  }
  it should "reject removed or forged gate provenance and altered emitted gate behavior" in {
    val state = lower(source)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    val doc = parse(result.binding)
    for (bad <- Seq(doc.removeField { case (key, _) => key == "clock_gate_adapters" },
      doc.transformField { case ("source_sha256", _) => "source_sha256" -> JString("0" * 64) },
      doc.transformField { case ("gate_chain", _) => "gate_chain" -> JArray(Nil) })) {
      intercept[IllegalArgumentException] {
        midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(
          binding = SemanticStateInventory.canonical(bad)), manifest)
      }
    }
    val altered = result.state.circuit.copy(modules = result.state.circuit.modules.map {
      case m: Module if m.ports.exists(_.name == "I") =>
        def rewrite(s: Statement): Statement = s match {
          case c: Connect if c.loc.serialize == "enable" => c.copy(expr = firrtl.WRef("CE", UIntType(IntWidth(1))))
          case x => x.mapStmt(rewrite)
        }
        m.copy(body = rewrite(m.body))
      case m => m
    })
    // Rehashing an altered circuit cannot bypass the independent gate checker.
    val binding = doc.transformField { case ("instrumented_sha256", _) =>
      "instrumented_sha256" -> JString(SemanticStateInventory.sha256(altered.serialize)) }
    assert(intercept[IllegalArgumentException] {
      midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(
        state = result.state.copy(circuit = altered), binding = SemanticStateInventory.canonical(binding)), manifest)
    }.getMessage.contains("clock-gate behavior"))
  }

  it should "preserve repeated and cascaded gate owners with ordered phase provenance" in {
    val repeated = source.replace("    reg saved :", """    inst second of AbstractClockGate
    second.I <= gate.O
    second.CE <= wen
    reg saved :""").replace("reg saved : UInt<17>, gate.O", "reg saved : UInt<17>, second.O")
    val state = lower(repeated)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    assert(result.entries.size == 5 && result.entries.map(_.depth).sum == 8)
    val doc = parse(result.binding)
    assert((doc \ "clock_gate_adapters") == GeneratedClockGateAdapter.binding(Set("GateStorage.gate", "GateStorage.second")))
    assert((doc \ "functional_clock_domains").asInstanceOf[JArray].arr.exists(row =>
      (row \ "clock_hierarchy") == JString("GateStorage.second.O") &&
      (row \ "gate_chain") == JArray(List(JString("GateStorage.gate"), JString("GateStorage.second")))))
  }

  it should "reject mismatched conditional memory clocks and guarded register clocks" in {
    for (bad <- Seq(source.replace("validif(wen, gate.O)", "validif(ce, gate.O)"),
      source.replace("reg saved : UInt<17>, gate.O", "reg saved : UInt<17>, validif(wen, gate.O)"))) {
      val state = lower(bad)
      intercept[IllegalArgumentException] {
        GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state), separateAccessClock = true)
      }
    }
  }

  it should "instrument the retained exact Rocket FASED timing model including its registered gate" in {
    val filename = sys.props.getOrElse("checkpoint.rocket.reference", fail("retained Rocket reference required"))
    val gateFile = sys.props.getOrElse("checkpoint.gate.reference", fail("retained gate implementation required"))
    val gateSource = new String(Files.readAllBytes(Paths.get(gateFile)), UTF_8)
    assert(gateSource == DefineAbstractClockGate.impl, "retained build gate source differs from the exact adapter")
    val file = scala.io.Source.fromFile(filename)
    val circuit = try Parser.parse(file.mkString) finally file.close()
    val modules = circuit.modules.map(m => m.name -> m).toMap
    val reachable = scala.collection.mutable.Set.empty[String]
    def walk(name: String): Unit = if (reachable.add(name)) modules(name) match {
      case m: Module =>
        def visit(s: Statement): Unit = {
          s match { case i: DefInstance => walk(i.module); case _ => () }; s.foreachStmt(visit)
        }; visit(m.body)
      case m: ExtModule => assert(m.defname == "AbstractClockGate")
    }
    walk("FASEDMemoryTimingModel")
    val selected = circuit.copy(main = "FASEDMemoryTimingModel", modules = circuit.modules.filter(m => reachable(m.name)))
    val inline = selected.modules.collect { case m: ExtModule =>
      BlackBoxInlineAnno(ModuleName(m.name, CircuitName(selected.main)), "AbstractClockGate.v", gateSource) }
    val state = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(selected.serialize), ChirrtlForm,
      inline :+ firrtl.transforms.NoCircuitDedupAnnotation)).copy(form = LowForm)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    val doc = parse(result.binding)
    assert((doc \ "clock_gate_adapters") == GeneratedClockGateAdapter.binding(Set("FASEDMemoryTimingModel.gate")))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("rocket-fased"); Files.createDirectories(out)
      Seq("generated-bank-input.fir" -> state.circuit.serialize,
        "generated-bank.fir" -> result.state.circuit.serialize,
        "generated-bank.sv" -> emit(result.state), "generated-bank-manifest.json" -> manifest,
        "generated-bank-binding.json" -> result.binding
      ).foreach { case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8)) }
      println(s"ROCKET_FASED_BANK entries=${result.entries.size} words=${result.entries.map(_.depth.toLong).sum}")
    }
  }
}
