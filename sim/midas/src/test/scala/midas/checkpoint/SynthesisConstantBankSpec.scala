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
import org.scalatest.flatspec.AnyFlatSpec

class SynthesisConstantBankSpec extends AnyFlatSpec {
  private val modelFile = sys.props.getOrElse("checkpoint.plusarg.reference",
    throw new IllegalArgumentException("exact retained plusarg model required"))
  private val model = new String(Files.readAllBytes(Paths.get(modelFile)), UTF_8)
  private val source = """circuit SynthesisConstantStorage :
  extmodule Plus :
    output out : UInt<9>
    defname = plusarg_reader
    parameter WIDTH = 9
    parameter DEFAULT = 341
    parameter FORMAT = "checkpoint_constant=%d"
  module SynthesisConstantStorage :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<17>
    output result : UInt<32>
    output fixed : UInt<9>
    inst a of Plus
    inst b of Plus
    reg count : UInt<32>, clock with :
      reset => (reset, UInt<32>(0))
    count <= add(count, add(data, a.out))
    result <= count
    fixed <= b.out
"""
  private def lower(text: String = source, implementation: String = model,
                    annotationCircuit: String = "SynthesisConstantStorage"): CircuitState = {
    val circuit = Parser.parse(text)
    val inline = circuit.modules.collect { case m: ExtModule if m.defname == "plusarg_reader" =>
      BlackBoxInlineAnno(ModuleName(m.name, CircuitName(annotationCircuit)), "plusarg_reader.v", implementation) }
    new Compiler(Forms.LowForm).execute(CircuitState(circuit, ChirrtlForm,
      inline :+ firrtl.transforms.NoCircuitDedupAnnotation)).copy(form = LowForm)
  }
  private def emit(s: CircuitState): String = new Compiler(
    Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(
      s.copy(annotations = s.annotations :+ firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
      .getEmittedCircuit.value

  behavior.of("Explicit FPGA SYNTHESIS plusarg constants")
  it should "retain metasim init-only owners unless explicitly prepared for synthesis" in {
    assert(SemanticStateInventory.sha256(model) == SimulatorStateInventory.plusargModelSHA256)
    val original = lower()
    val oldManifest = SimulatorStateInventory.discover(original)
    assert(oldManifest.contains("metasim-plusarg-v1") && oldManifest.contains("myplus"))
    intercept[IllegalArgumentException] { GeneratedStateBankAccess.generate(original, oldManifest) }
    val state = GeneratedSynthesisConstantAdapter.prepare(original)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    assert(result.entries.size == 1 && result.entries.head.width == 32)
    val annotations = result.state.annotations.collect { case a: SynthesisPlusArgConstantAnnotation => a }
    assert(annotations.size == 2 && annotations.map(_.target.module).distinct.size == 2)
    assert(annotations.forall(a => a.default == 341 && a.originalCircuitSHA256 ==
      SemanticStateInventory.sha256(original.circuit.serialize)))
    assert(!manifest.contains("myplus"))
    intercept[IllegalArgumentException] { GeneratedSynthesisConstantAdapter.prepare(state) }
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("synthesis-constant"); Files.createDirectories(out)
      // The independent model is retained explicitly below. Do not ask the
      // emitter to spill its BlackBoxInlineAnno resource into the sbt cwd.
      val reference = emit(original.copy(annotations = original.annotations.filterNot(_.isInstanceOf[BlackBoxInlineAnno])))
        .replace("module SynthesisConstantStorage(", "module OriginalSynthesisConstantStorage(")
      Seq("generated-bank-input.fir" -> state.circuit.serialize, "generated-bank.fir" -> result.state.circuit.serialize,
        "generated-bank.sv" -> emit(result.state), "generated-bank-manifest.json" -> manifest,
        "generated-bank-binding.json" -> result.binding, "reference.sv" -> reference,
        "plusarg_reader.v" -> model, "original-input.fir" -> original.circuit.serialize,
        "original-annotations.json" -> firrtl.annotations.JsonProtocol.serialize(original.annotations),
        "prepared-annotations.json" -> firrtl.annotations.JsonProtocol.serialize(state.annotations)
      ).foreach { case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8)) }
    }
  }

  it should "reject changed missing duplicate foreign and malformed source contracts" in {
    for (state <- Seq(lower(implementation = model + "\n"),
      lower(annotationCircuit = "Other"), lower(source.replace("parameter DEFAULT = 341", "parameter DEFAULT = 512")),
      lower(source.replace("checkpoint_constant=%d", "invalid format")),
      lower(source.replace("parameter WIDTH = 9", "parameter WIDTH = 8")),
      lower(source.replace("parameter WIDTH = 9", "parameter WIDTH = 9\n    parameter EXTRA = 0")))) {
      intercept[IllegalArgumentException] { GeneratedSynthesisConstantAdapter.prepare(state) }
    }
    val state = lower()
    intercept[IllegalArgumentException] { GeneratedSynthesisConstantAdapter.prepare(state.copy(
      annotations = state.annotations.filterNot(_.isInstanceOf[BlackBoxInlineAnno]))) }
    val anno = state.annotations.collectFirst { case a: BlackBoxInlineAnno => a }.get
    intercept[IllegalArgumentException] { GeneratedSynthesisConstantAdapter.prepare(state.copy(
      annotations = state.annotations :+ anno)) }
  }

  it should "recheck actual constants and complete specialized provenance during descriptor construction" in {
    val state = GeneratedSynthesisConstantAdapter.prepare(lower())
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest)
    def reject(s: CircuitState): Unit = intercept[IllegalArgumentException] {
      midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(state = s), manifest)
    }
    reject(result.state.copy(annotations = result.state.annotations.filterNot(_.isInstanceOf[SynthesisPlusArgConstantAnnotation])))
    reject(result.state.copy(annotations = result.state.annotations.map {
      case a: SynthesisPlusArgConstantAnnotation => a.copy(default = 340)
      case a => a
    }))
    val target = result.state.annotations.collectFirst { case a: SynthesisPlusArgConstantAnnotation => a.target.module }.get
    reject(result.state.copy(circuit = result.state.circuit.copy(modules = result.state.circuit.modules.map {
      case m: Module if m.name == target =>
        def change(s: Statement): Statement = s match {
          case c: Connect if c.loc.serialize == "out" => c.copy(expr = UIntLiteral(340, IntWidth(9)))
          case s => s.mapStmt(change)
        }
        m.copy(body = change(m.body))
      case m => m
    })))
  }

}

/** Explicit diagnostic entry point; a failed whole-root probe is never a passing component test. */
object RetainedRocketBankProbe {
  private def emit(s: CircuitState): String = new Compiler(
    Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(
      s.copy(annotations = s.annotations :+ firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
      .getEmittedCircuit.value
  def main(args: Array[String]): Unit = {
    val model = new String(Files.readAllBytes(Paths.get(sys.props("checkpoint.plusarg.reference"))), UTF_8)
      val path = Paths.get(sys.props("checkpoint.rocket.reference"))
      val parsed = Parser.parse(new String(Files.readAllBytes(path), UTF_8))
      val modules = parsed.modules.map(m => m.name -> m).toMap
      val reachable = scala.collection.mutable.Set.empty[String]
      def walk(name: String): Unit = if (reachable.add(name)) modules(name) match {
        case m: Module =>
          def visit(s: Statement): Unit = { s match { case i: DefInstance => walk(i.module); case _ => () }; s.foreachStmt(visit) }
          visit(m.body)
        case _ => ()
      }
      walk(parsed.main)
      val circuit = parsed.copy(modules = parsed.modules.filter(m => reachable(m.name)))
      val gate = new String(Files.readAllBytes(Paths.get(sys.props("checkpoint.gate.reference"))), UTF_8)
      val inline = circuit.modules.collect { case m: ExtModule =>
        val source = if (m.defname == "AbstractClockGate") gate else { assert(m.defname == "plusarg_reader"); model }
        BlackBoxInlineAnno(ModuleName(m.name, CircuitName(circuit.main)), m.defname + ".v", source) }
      val original = new Compiler(Forms.LowForm).execute(CircuitState(circuit, ChirrtlForm,
        inline :+ firrtl.transforms.NoCircuitDedupAnnotation)).copy(form = LowForm)
      val state = GeneratedSynthesisConstantAdapter.prepare(original)
      val manifest = SimulatorStateInventory.discover(state)
      val inventoryOut = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("rocket-complete-probe")
      Files.createDirectories(inventoryOut)
      Seq("original-input.fir" -> original.circuit.serialize,
        "original-annotations.json" -> firrtl.annotations.JsonProtocol.serialize(original.annotations),
        "prepared-input.fir" -> state.circuit.serialize, "manifest.json" -> manifest,
        "prepared-annotations.json" -> firrtl.annotations.JsonProtocol.serialize(state.annotations)
      ).foreach { case (name, text) => Files.write(inventoryOut.resolve(name), text.getBytes(UTF_8)) }
      val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
      midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
      val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("rocket-complete"); Files.createDirectories(out)
      Seq("generated-bank-input.fir" -> state.circuit.serialize, "generated-bank.fir" -> result.state.circuit.serialize,
        "generated-bank.sv" -> emit(result.state), "generated-bank-manifest.json" -> manifest,
        "generated-bank-binding.json" -> result.binding,
        "original-annotations.json" -> firrtl.annotations.JsonProtocol.serialize(original.annotations),
        "prepared-annotations.json" -> firrtl.annotations.JsonProtocol.serialize(state.annotations)
      ).foreach { case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8)) }
      println(s"ROCKET_COMPLETE_BANK entries=${result.entries.size} words=${result.entries.map(_.depth.toLong).sum}")
  }
}
