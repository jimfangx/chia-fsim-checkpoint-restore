// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.transforms.DontTouchAnnotation
import midas.widgets.CheckpointCaptureDescriptors
import midas.InternalXDCAnnotation
import midas.passes.{WriteXDCFile, XDCOutputAnnotation}
import midas.targetutils.xdc.{RAMStyleHint, RAMStyles, XDCAnnotation, XDCFiles, XDCPathToCircuitAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

class InitializedStateBankSpec extends AnyFlatSpec {
  private val out = sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").map(Paths.get(_).resolve("initialized"))
    .getOrElse(Files.createTempDirectory("checkpoint-initialized-"))
  Files.createDirectories(out)
  // Keep filenames relative and self-contained for transfer to EDA/native synthesis.
  // The emitter embeds these names in $readmemh/$readmemb; callers run in out.
  private val source = """circuit InitializedStorage :
  module Leaf :
    input clock : Clock
    input addr : UInt<3>
    input data : UInt<9>
    input wen : UInt<1>
    output value : UInt<9>
    mem ram :
      data-type => UInt<9>
      depth => 8
      reader => r
      writer => w
      read-latency => 0
      write-latency => 1
      read-under-write => undefined
    ram.r.clk <= clock
    ram.r.en <= UInt<1>(1)
    ram.r.addr <= addr
    ram.w.clk <= clock
    ram.w.en <= wen
    ram.w.addr <= addr
    ram.w.mask <= UInt<1>(1)
    ram.w.data <= data
    value <= ram.r.data
  module InitializedStorage :
    input clock : Clock
    input addr : UInt<3>
    input data : UInt<9>
    input wen : UInt<1>
    output left : UInt<9>
    output right : UInt<9>
    inst a of Leaf
    inst b of Leaf
    a.clock <= clock
    b.clock <= clock
    a.addr <= addr
    b.addr <= addr
    a.data <= data
    b.data <= xor(data, UInt<9>(511))
    a.wen <= wen
    b.wen <= wen
    left <= a.value
    right <= b.value
"""
  private def lower(text: String = source): CircuitState = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(text), ChirrtlForm)).copy(form = LowForm)
  private val state = lower()
  private val root = state.circuit.main
  private val local = ReferenceTarget(root, "Leaf", Nil, "ram", Nil)
  private def child(name: String): ReferenceTarget = ReferenceTarget(root, root,
    Seq(TargetToken.Instance(name) -> TargetToken.OfModule("Leaf")), "ram", Nil)
  private def generate(annotations: Seq[Annotation], input: CircuitState = state): GeneratedStateBankAccess.Result =
    GeneratedStateBankAccess.generate(input.copy(annotations = annotations),
      SemanticStateInventory.discover(input, input.circuit.main))
  private def save(name: String, value: String): Unit = Files.write(out.resolve(name), value.getBytes(UTF_8))
  private def emit(s: CircuitState): String = {
    val emitted = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(s.copy(annotations = s.annotations :+
        firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
    emitted.annotations.collectFirst { case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value }.get
  }
  behavior.of("Initialized and preserved generated storage")

  it should "clone local annotations to every repeated instance without sharing physical storage" in {
    val result = generate(Seq(MemoryFileInlineAnnotation(local, "shared.hex"), DontTouchAnnotation(local)))
    val init = result.state.annotations.collect { case a: MemoryFileInlineAnnotation => a }
    val keep = result.state.annotations.collect { case a: DontTouchAnnotation => a }
    assert(init.size == 2 && keep.size == 2 && init.map(_.target).toSet == keep.map(_.target).toSet)
    assert(init.map(_.target.module).distinct.size == 2 && init.forall(_.target.path.isEmpty))
    assert(CheckpointCaptureDescriptors.fromGenerated(result, SemanticStateInventory.discover(state, root)).entries.size == 2)
    val bad = result.copy(state = result.state.copy(annotations = result.state.annotations.filterNot(
      _.isInstanceOf[MemoryFileInlineAnnotation])))
    intercept[IllegalArgumentException] { CheckpointCaptureDescriptors.fromGenerated(bad,
      SemanticStateInventory.discover(state, root)) }
    val changed = result.copy(state = result.state.copy(annotations = result.state.annotations.map {
      case a: MemoryFileInlineAnnotation => a.copy(filename = "changed.hex")
      case a => a
    }))
    intercept[IllegalArgumentException] { CheckpointCaptureDescriptors.fromGenerated(changed,
      SemanticStateInventory.discover(state, root)) }
    val extra = result.copy(state = result.state.copy(annotations = result.state.annotations :+
      firrtl.options.TargetDirAnnotation("unsupported")))
    intercept[IllegalArgumentException] { CheckpointCaptureDescriptors.fromGenerated(extra,
      SemanticStateInventory.discover(state, root)) }
  }

  it should "preserve separately targeted hex and binary initialization through emission" in {
    val annotations = Seq(MemorySynthInit, MemoryFileInlineAnnotation(child("a"), "left.hex"),
      MemoryFileInlineAnnotation(child("b"), "right.bin", MemoryLoadFileType.Binary),
      DontTouchAnnotation(child("a")), DontTouchAnnotation(child("b")),
      XDCAnnotation(XDCFiles.Synthesis, RAMStyleHint.propertyTemplate(RAMStyles.DISTRIBUTED), child("a")),
      InternalXDCAnnotation(XDCFiles.Synthesis, RAMStyleHint.propertyTemplate(RAMStyles.DISTRIBUTED), child("b")))
    val result = generate(annotations)
    val manifest = SemanticStateInventory.discover(state, root)
    val simulator = SimulatorStateInventory.discover(state)
    val simResult = GeneratedStateBankAccess.generate(state.copy(annotations = annotations), simulator)
    assert(result.entries.map(_.stateID).toSet.intersect(simResult.entries.map(_.stateID).toSet).isEmpty)
    val rtl = emit(result.state)
    assert(rtl.contains("$readmemh(\"left.hex\"") && rtl.contains("$readmemb(\"right.bin\""))
    // Use the same independent original RTL, but name its single top distinctly.
    // Standard FIRRTL path resolution is independent of our bank specialization.
    val referenceState = new firrtl.annotations.transforms.EliminateTargetPaths().runTransform(
      state.copy(annotations = annotations :+ firrtl.annotations.transforms.ResolvePaths(Seq(child("a"), child("b")))))
    val reference = emit(referenceState).replace("module InitializedStorage(",
      "module ReferenceInitializedStorage(")
    save("generated-bank-input.fir", state.circuit.serialize)
    save("generated-bank.fir", result.state.circuit.serialize)
    save("generated-bank.sv", rtl)
    save("reference.sv", reference)
    save("reference.fir", referenceState.circuit.serialize)
    save("generated-bank-semantic.json", manifest)
    save("generated-bank-simulator.json", simulator)
    save("generated-bank-binding.json", result.binding)
    save("generated-bank-simulator-binding.json", simResult.binding)
    save("generated-bank-simulator.fir", simResult.state.circuit.serialize)
    save("source-annotations.json", GeneratedStateBankAnnotations.annotationJSON(annotations))
    save("translated-annotations.json", GeneratedStateBankAnnotations.annotationJSON(result.state.annotations))
    val xdc = WriteXDCFile.execute(result.state.copy(annotations = result.state.annotations :+ XDCPathToCircuitAnnotation(None, None)))
    xdc.annotations.collect { case a: XDCOutputAnnotation => a }.foreach(a => save("ram-style" + a.suffix.get, a.fileBody))
    save("left.hex", (0 until 8).map(i => f"${17 + i * 29}%03x").mkString("\n") + "\n")
    save("right.bin", (0 until 8).map(i => ("0" * 9 + (401 - i * 31).toBinaryString).takeRight(9)).mkString("\n") + "\n")
  }

  it should "reject overlapping initializers, dead declarations and malformed instance paths" in {
    val bad = Seq(
      Seq(MemoryFileInlineAnnotation(local, "shared.hex"), MemoryFileInlineAnnotation(child("a"), "left.hex")),
      Seq(MemoryFileInlineAnnotation(local.copy(ref = "clock"), "wrong.hex")),
      Seq(MemoryFileInlineAnnotation(child("missing"), "wrong.hex")),
      Seq(DontTouchAnnotation(local.copy(module = "Missing"))),
      Seq(DontTouchAnnotation(local.copy(circuit = "Wrong"))),
      Seq(DontTouchAnnotation(local.field("r"))),
      Seq(MemorySynthInit, MemoryNoSynthInit),
      Seq(DontTouchAnnotation(child("a").copy(path = Seq(TargetToken.Instance("a") -> TargetToken.OfModule("Wrong"))))))
    bad.foreach(a => intercept[IllegalArgumentException] { generate(a) })
  }

  it should "translate synchronous RAM initialization while retaining its separate pipeline owners" in {
    val sync = lower(source.replace("read-latency => 0", "read-latency => 1"))
    val result = generate(Seq(MemoryFileInlineAnnotation(local, "shared.hex")), sync)
    assert(result.entries.size == 4)
    assert(result.state.annotations.count(_.isInstanceOf[MemoryFileInlineAnnotation]) == 2)
    assert(emit(result.state).sliding("$readmemh".length).count(_ == "$readmemh") == 2)
  }
}
