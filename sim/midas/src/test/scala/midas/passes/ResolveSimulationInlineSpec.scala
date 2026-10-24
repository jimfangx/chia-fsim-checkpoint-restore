// See LICENSE for license details.
package midas.passes

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations._
import firrtl.options.Dependency
import firrtl.passes.{InlineAnnotation, InlineInstances}
import firrtl.stage.{Forms, RunFirrtlTransformAnnotation}
import firrtl.stage.transforms.Compiler
import midas.checkpoint.SemanticStateInventory.{canonical, sha256}
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class ResolveSimulationInlineSpec extends AnyFlatSpec {
  private val source = """circuit InlineFixture :
  module Leaf :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<8>
    input addr : UInt<2>
    input wen : UInt<1>
    output value : UInt<8>
    output read : UInt<8>
    reg time : UInt<8>, clock with :
      reset => (reset, UInt<8>(7))
    time <= add(time, data)
    value <= time
    mem ram :
      data-type => UInt<8>
      depth => 4
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
    ram.w.data <= data
    ram.w.mask <= UInt<1>(1)
    read <= ram.r.data
  module InlineFixture :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<8>
    input addr : UInt<2>
    input wen : UInt<1>
    output value : UInt<8>
    output read : UInt<8>
    inst left of Leaf
    inst right of Leaf
    left.clock <= clock
    right.clock <= clock
    left.reset <= reset
    right.reset <= reset
    left.data <= data
    right.data <= add(data, UInt<8>(3))
    left.addr <= addr
    right.addr <= addr
    left.wen <= wen
    right.wen <= wen
    value <= add(left.value, right.value)
    read <= xor(left.read, right.read)
"""
  private def lower = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(source), ChirrtlForm,
    Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  private val request = RunFirrtlTransformAnnotation(new InlineInstances)
  private val target = InlineAnnotation(ModuleName("Leaf", CircuitName("InlineFixture")))
  private def input = lower.copy(annotations = lower.annotations ++ Seq(request, target,
    firrtl.transforms.DontTouchAnnotation(ReferenceTarget("InlineFixture", "Leaf", Nil, "time", Nil))))
  private def save(name: String, value: String): Unit = sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
    val out = Paths.get(dir).resolve("inline"); Files.createDirectories(out)
    Files.write(out.resolve(name), value.getBytes(UTF_8))
  }
  private def emit(state: CircuitState): String = {
    val emitter = new firrtl.SystemVerilogEmitter
    new Compiler(Seq(Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(state.copy(annotations = state.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
      .getEmittedCircuit.value
  }

  behavior.of("Post-mapping simulator inline request consumption")

  it should "execute the native pass before banking with complete repeated-instance register and memory mappings" in {
    val original = input
    val result = ResolveSimulationInline(original)
    assert(!result.annotations.exists(_.isInstanceOf[InlineAnnotation]))
    assert(!result.annotations.exists(_.isInstanceOf[RunFirrtlTransformAnnotation]))
    val receipt = result.annotations.collect { case a: SimulationInlineResultAnnotation => a }.head
    val doc = ResolveSimulationInline.context(receipt)
    assert((doc \ "input_sha256") == JString(sha256(original.circuit.serialize)))
    assert((doc \ "output_sha256") == JString(sha256(result.circuit.serialize)))
    assert((doc \ "renames").asInstanceOf[JString].s.nonEmpty)
    val rows = (doc \ "storage").asInstanceOf[JArray].arr
    assert(rows.size == 4)
    assert(rows.map(_ \ "to").toSet == Set("left_time", "left_ram", "right_time", "right_ram")
      .map(s => JString(s"~InlineFixture|InlineFixture>$s")))
    val preserved = result.annotations.collect { case a: firrtl.transforms.DontTouchAnnotation => a.target.ref }.toSet
    assert(preserved == Set("left_time", "right_time"))
    assert(ResolveSimulationInline(result) == result)
    save("input.fir", original.circuit.serialize)
    save("output.fir", result.circuit.serialize)
    save("input-annotations.json", JsonProtocol.serialize(original.annotations))
    save("output-annotations.json", JsonProtocol.serialize(result.annotations.filterNot(_ == receipt)))
    save("receipt.json", receipt.body)
    save("reference.sv", emit(original.copy(annotations = original.annotations.filterNot(a =>
      a.isInstanceOf[RunFirrtlTransformAnnotation] || a.isInstanceOf[InlineAnnotation]))))
    save("inlined.sv", emit(result))
  }

  it should "preserve unrelated transform requests and reject duplicate missing and stale inline requests" in {
    val extra = RunFirrtlTransformAnnotation(new firrtl.transforms.ConstantPropagation)
    assert(ResolveSimulationInline(input.copy(annotations = input.annotations :+ extra)).annotations.contains(extra))
    val unselected = lower
    assert(ResolveSimulationInline(unselected) == unselected)
    Seq(input.copy(form = ChirrtlForm), input.copy(annotations = input.annotations :+ request),
      input.copy(annotations = input.annotations :+ target),
      input.copy(annotations = input.annotations.filterNot(_.isInstanceOf[InlineAnnotation])),
      input.copy(annotations = input.annotations.filterNot(_.isInstanceOf[InlineAnnotation]) :+
        InlineAnnotation(ModuleName("Absent", CircuitName("InlineFixture"))))).foreach { bad =>
      intercept[Exception](ResolveSimulationInline(bad))
    }
    val result = ResolveSimulationInline(input)
    intercept[IllegalArgumentException](ResolveSimulationInline(result.copy(annotations =
      result.annotations ++ Seq(request, target))))
  }

  it should "reject altered request identity shape target aliases hashes and receipt schema" in {
    val receipt = ResolveSimulationInline(input).annotations.collectFirst {
      case a: SimulationInlineResultAnnotation => a
    }.get
    val doc = parse(receipt.body)
    val rows = (doc \ "storage").asInstanceOf[JArray].arr
    def replace(key: String, value: JValue): JValue = doc transformField { case (`key`, _) => key -> value }
    val invalid = Seq(replace("version", JInt(2)), replace("input_sha256", JString("bad")),
      replace("producer", JString("unknown")), replace("request", JArray(Nil)),
      replace("inline_annotations", JArray(Nil)), replace("renames", JArray(Nil)), replace("renames", JString("")),
      replace("storage", JArray(rows :+ rows.head)), replace("shape", JString("opaque")),
      replace("from", JString("~Other|Other>reg")))
    invalid.foreach(d => intercept[IllegalArgumentException](
      ResolveSimulationInline.context(SimulationInlineResultAnnotation(canonical(d)))))
    intercept[IllegalArgumentException](ResolveSimulationInline.context(receipt.copy(body = receipt.body + " ")))
  }
}
