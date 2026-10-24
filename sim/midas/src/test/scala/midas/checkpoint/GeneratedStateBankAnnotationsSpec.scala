// See LICENSE for license details.
package midas.checkpoint

import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations._
import firrtl.passes.InlineAnnotation
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.transforms.NoDedupAnnotation
import midas.widgets.CheckpointCaptureDescriptors
import org.scalatest.flatspec.AnyFlatSpec
import midas.InternalXDCAnnotation
import midas.passes.{WriteXDCFile, XDCOutputAnnotation}
import midas.targetutils.FirrtlMemModelAnnotation
import midas.targetutils.xdc.{XDCAnnotation, XDCFiles, XDCPathToCircuitAnnotation}

class GeneratedStateBankAnnotationsSpec extends AnyFlatSpec {
  private val source = """circuit Top :
  module Leaf :
    input clock : Clock
    input data : UInt<8>
    output value : UInt<8>
    reg stored : UInt<8>, clock
    stored <= data
    value <= stored
  module Parent :
    input clock : Clock
    input data : UInt<8>
    output value : UInt<8>
    inst child of Leaf
    child.clock <= clock
    child.data <= data
    value <= child.value
  module Top :
    input clock : Clock
    input data : UInt<8>
    output value : UInt<8>
    inst left of Parent
    inst right of Parent
    left.clock <= clock
    right.clock <= clock
    left.data <= data
    right.data <= add(data, UInt<8>(1))
    value <= add(left.value, right.value)
"""
  private val state = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(source), ChirrtlForm)).copy(form = LowForm)
  private val circuit = CircuitName("Top")
  private val parent = ModuleName("Parent", circuit)
  private val leaf = ModuleName("Leaf", circuit)
  private val manifest = SemanticStateInventory.discover(state, "Top")
  private def generate(annotations: Seq[Annotation]): GeneratedStateBankAccess.Result =
    GeneratedStateBankAccess.generate(state.copy(annotations = annotations), manifest)
  private def moduleNames(result: GeneratedStateBankAccess.Result): Set[String] =
    result.state.circuit.modules.map(_.name).toSet

  behavior.of("Generated bank hierarchy annotation contracts")

  it should "preserve no-dedup intent for every specialized repeated owner and bind the contract" in {
    val result = generate(Seq(NoDedupAnnotation(ModuleTarget("Top", "Leaf"))))
    val copies = result.state.annotations.collect { case a: NoDedupAnnotation => a }
    assert(copies.size == 2)
    assert(copies.map(_.target.module).distinct.size == 2)
    assert(copies.forall(a => a.target.circuit == "Top" && moduleNames(result)(a.target.module)))
    assert(result.state.annotations.contains(firrtl.transforms.NoCircuitDedupAnnotation))
    CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    intercept[IllegalArgumentException] {
      CheckpointCaptureDescriptors.fromGenerated(result.copy(state = result.state.copy(
        annotations = result.state.annotations.filterNot(_.isInstanceOf[NoDedupAnnotation]))), manifest)
    }
  }

  it should "rename module and exact-instance inline requests without consuming or flattening them" in {
    val requests = Seq(InlineAnnotation(leaf), InlineAnnotation(ComponentName("child", parent)),
      InlineAnnotation(circuit))
    val result = generate(requests)
    val inline = result.state.annotations.collect { case a: InlineAnnotation => a }
    assert(inline.size == 5)
    assert(inline.count(_.target.isInstanceOf[ModuleName]) == 2)
    assert(inline.count(_.target.isInstanceOf[ComponentName]) == 2)
    assert(inline.contains(InlineAnnotation(circuit)))
    inline.foreach(_.target match {
      case m: ModuleName => assert(m.circuit == circuit && moduleNames(result)(m.name))
      case i: ComponentName =>
        assert(i.name == "child" && i.module.circuit == circuit && moduleNames(result)(i.module.name))
      case c: CircuitName => assert(c == circuit)
      case target => fail(s"unexpected inline target: ${target.serialize}")
    })
    assert(result.state.circuit.modules.size == 5)
    CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    val emitted = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(result.state.copy(annotations = result.state.annotations :+
        firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
    assert(emitted.annotations.count(_.isInstanceOf[InlineAnnotation]) == 5)
    assert(emitted.getEmittedCircuit.value.contains("module CheckpointStorage_"))
    assert(GeneratedStateBankAnnotations.translatedHash(result.state) !=
      GeneratedStateBankAnnotations.translatedHash(result.state.copy(annotations =
        result.state.annotations.filterNot(_.isInstanceOf[InlineAnnotation]))))
  }

  it should "preserve correlated physical constraints under each repeated root through real XDC emission" in {
    val clock = ReferenceTarget("Top", "Parent", Nil, "clock", Nil)
    val leafValue = ModuleTarget("Top", "Parent").instOf("child", "Leaf").ref("stored")
    val connector = ReferenceTarget("Top", "Parent", Nil, "child", Nil).field("value")
    val constraints = Seq(
      InternalXDCAnnotation(XDCFiles.Implementation, "set_max_delay 7 -from [get_pins {}] -to [get_pins {}]", clock, leafValue),
      InternalXDCAnnotation(XDCFiles.Synthesis, "set_property DONT_TOUCH true [get_nets {}]", connector),
      InternalXDCAnnotation(XDCFiles.Synthesis, "set_property DONT_TOUCH true [get_cells {}]",
        ReferenceTarget("Top", "Parent", Nil, "child", Nil)),
      InternalXDCAnnotation(XDCFiles.Synthesis, "set_param project.enableReportConfiguration 1"))
    val result = generate(constraints)
    val copies = result.state.annotations.collect { case a: InternalXDCAnnotation => a }
    assert(copies.size == 7) // two correlated copies per targeted snippet, one global snippet
    assert(copies.filter(_.argumentList.nonEmpty).forall(a => a.argumentList.map(_.module).distinct.size == 1))
    assert(copies.filter(_.argumentList.size == 2).forall(a =>
      a.argumentList(1).path.head._2.value != "Leaf"))
    def emitted(st: CircuitState): Map[String, String] = {
      val output = WriteXDCFile.execute(st.copy(annotations = st.annotations :+
        XDCPathToCircuitAnnotation(Some("shell/pre"), Some("shell/post"))))
      output.annotations.collect { case XDCOutputAnnotation(body, Some(suffix)) =>
        suffix -> body.split("\n").sorted.mkString("\n")
      }.toMap
    }
    val original = emitted(state.copy(annotations = constraints))
    val renamed = emitted(result.state)
    assert(original == renamed)
    assert(renamed(".implementation.xdc").contains("shell/post/left/clock"))
    assert(renamed(".implementation.xdc").contains("shell/post/right/child/stored"))
    assert(!renamed(".implementation.xdc").contains("left/clock] -to [get_pins shell/post/right"))
    CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    copies.foreach { removed =>
      intercept[IllegalArgumentException] {
        CheckpointCaptureDescriptors.fromGenerated(result.copy(state = result.state.copy(
          annotations = result.state.annotations.filterNot(_ == removed))), manifest)
      }
    }
    val changed = result.state.copy(annotations = result.state.annotations.map {
      case a: InternalXDCAnnotation if a.argumentList.size == 2 => InternalXDCAnnotation(a.destinationFile, a.formatString.replace(" 7 ", " 8 "), a.argumentList: _*)
      case a => a
    })
    intercept[IllegalArgumentException] {
      CheckpointCaptureDescriptors.fromGenerated(result.copy(state = changed), manifest)
    }
  }

  it should "translate external XDC with the same exact path contract without treating it as emitted internal XDC" in {
    val rt = ModuleTarget("Top", "Parent").instOf("child", "Leaf").ref("stored")
    val input = XDCAnnotation(XDCFiles.Implementation, "set_property KEEP true [get_nets {}]", rt)
    val result = generate(Seq(input))
    val copies = result.state.annotations.collect { case a: XDCAnnotation => a }
    assert(copies.size == 2 && copies.map(_.argumentList.head.module).distinct.size == 2)
    assert(copies.forall(_.argumentList.head.path.head._2.value != "Leaf"))
    assert(!result.state.annotations.exists(_.isInstanceOf[InternalXDCAnnotation]))
    CheckpointCaptureDescriptors.fromGenerated(result, manifest)
  }

  it should "reject malformed, cross-root, foreign, opaque, stale and aggregate XDC targets before specialization" in {
    val clock = ReferenceTarget("Top", "Parent", Nil, "clock", Nil)
    val bad = Seq(
      InternalXDCAnnotation(XDCFiles.Synthesis, "two {} {}", clock),
      InternalXDCAnnotation(XDCFiles.Synthesis, "none", clock),
      InternalXDCAnnotation(XDCFiles.Synthesis, "two {} {}", clock, clock.copy(module = "Leaf")),
      InternalXDCAnnotation(XDCFiles.Synthesis, "one {}", clock.copy(circuit = "Other")),
      InternalXDCAnnotation(XDCFiles.Synthesis, "one {}", clock.copy(module = "Dead")),
      InternalXDCAnnotation(XDCFiles.Synthesis, "one {}", clock.copy(ref = "missing")),
      InternalXDCAnnotation(XDCFiles.Synthesis, "one {}", clock.field("bad")),
      InternalXDCAnnotation(XDCFiles.Synthesis, "one {}", clock.copy(ref = "child").field("bad")),
      InternalXDCAnnotation(XDCFiles.Synthesis, "one {}", clock.copy(ref = "child").field("value").index(0)),
      InternalXDCAnnotation(XDCFiles.Synthesis, "one {}", clock.copy(path =
        Seq(TargetToken.Instance("child") -> TargetToken.OfModule("Parent")))))
    bad.foreach(a => intercept[IllegalArgumentException] { generate(Seq(a)) })
    val opaque = state.copy(circuit = state.circuit.copy(modules = state.circuit.modules.map {
      case m if m.name == "Leaf" => firrtl.ir.ExtModule(m.info, m.name, m.ports, "Leaf", Nil)
      case m => m
    }))
    intercept[IllegalArgumentException] {
      GeneratedStateBankAnnotations.preflight(opaque.copy(annotations = Seq(
        InternalXDCAnnotation(XDCFiles.Synthesis, "one {}", clock.copy(ref = "child").field("value")))))
    }
  }

  it should "clone memory-model labels only onto exact retained logical memories and bind their ownership" in {
    val memorySource = source.replace("reg stored : UInt<8>, clock", """mem ram :
      data-type => UInt<8>
      depth => 4
      read-latency => 0
      write-latency => 1
      reader => r
    ram.r.addr <= UInt<2>(0)
    ram.r.en <= UInt<1>(1)
    ram.r.clk <= clock
    reg stored : UInt<8>, clock""").replace("stored <= data", "stored <= add(data, ram.r.data)")
    val memoryState = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(memorySource), ChirrtlForm))
      .copy(form = LowForm)
    val memoryManifest = SemanticStateInventory.discover(memoryState, "Top")
    val label = FirrtlMemModelAnnotation(ReferenceTarget("Top", "Leaf", Nil, "ram", Nil))
    def generateMemory(a: FirrtlMemModelAnnotation) =
      GeneratedStateBankAccess.generate(memoryState.copy(annotations = Seq(a)), memoryManifest)
    val result = generateMemory(label)
    val copies = result.state.annotations.collect { case a: FirrtlMemModelAnnotation => a }
    assert(copies.size == 2 && copies.map(_.target.module).distinct.size == 2)
    assert(copies.forall(a => a.target.ref == "ram" && a.target.path.isEmpty))
    CheckpointCaptureDescriptors.fromGenerated(result, memoryManifest)
    intercept[IllegalArgumentException] {
      CheckpointCaptureDescriptors.fromGenerated(result.copy(state = result.state.copy(
        annotations = result.state.annotations.filterNot(_.isInstanceOf[FirrtlMemModelAnnotation]))), memoryManifest)
    }
    Seq(label.copy(target = label.target.copy(ref = "stored")),
      label.copy(target = label.target.copy(circuit = "Other")),
      label.copy(target = label.target.copy(module = "Missing")),
      label.copy(target = label.target.field("r"))).foreach { a =>
      intercept[IllegalArgumentException] { generateMemory(a) }
    }
  }

  it should "reject foreign, unreachable, opaque and non-instance hierarchy contracts before compilation" in {
    val invalid: Seq[Annotation] = Seq(
      NoDedupAnnotation(ModuleTarget("Other", "Leaf")),
      NoDedupAnnotation(ModuleTarget("Top", "Missing")),
      InlineAnnotation(CircuitName("Other")),
      InlineAnnotation(ModuleName("Missing", circuit)),
      InlineAnnotation(ComponentName("stored", leaf)),
      InlineAnnotation(ComponentName("child.value", parent)),
      InlineAnnotation(ComponentName("missing", parent)),
      InlineAnnotation(ModuleTarget("Top", "Leaf")),
      InlineAnnotation(ModuleName("Leaf", CircuitName("Other"))))
    invalid.foreach(a => intercept[IllegalArgumentException] { generate(Seq(a)) })
    val opaque = state.copy(circuit = state.circuit.copy(modules = state.circuit.modules.map {
      case m if m.name == "Leaf" => firrtl.ir.ExtModule(m.info, m.name, m.ports, "Leaf", Nil)
      case m => m
    }))
    Seq(NoDedupAnnotation(ModuleTarget("Top", "Leaf")), InlineAnnotation(leaf),
      InlineAnnotation(ComponentName("child", parent))).foreach { a =>
      intercept[IllegalArgumentException] {
        GeneratedStateBankAnnotations.preflight(opaque.copy(annotations = Seq(a)))
      }
    }
    intercept[IllegalArgumentException] {
      generate(Seq(UnrecognizedAnnotation(org.json4s.JObject(List("class" -> org.json4s.JString("unknown"))))))
    }
  }
}
