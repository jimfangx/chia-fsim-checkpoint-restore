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
