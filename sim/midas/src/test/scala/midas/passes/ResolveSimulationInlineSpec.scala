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
import firrtl.transforms.NoDedupAnnotation
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
    NoDedupAnnotation(ModuleTarget("InlineFixture", "Leaf")),
    NoDedupAnnotation(ModuleTarget("InlineFixture", "InlineFixture")),
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

  it should "normalize regional inline output inside its producer while preserving default v2 text storage and annotation order" in {
    val original = input
    val ordinary = ResolveSimulationInline(original)
    val regional = ResolveSimulationInline.forCheckpointRegions(original)
    val ordinaryReceipt = ordinary.annotations.collectFirst { case a: SimulationInlineResultAnnotation => a }.get
    val regionalReceipt = regional.annotations.collectFirst { case a: SimulationInlineResultAnnotation => a }.get
    val v2 = ResolveSimulationInline.context(ordinaryReceipt)
    val v3 = ResolveSimulationInline.context(regionalReceipt)
    assert((v2 \ "version") == JInt(2) && (v2 \ "regional_normalization") == JNothing)
    assert((v3 \ "version") == JInt(3))
    assert(ordinary.circuit.serialize == regional.circuit.serialize)
    val expected = firrtl.passes.ResolveFlows.run(firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(
      firrtl.passes.ToWorkingIR.run(ordinary.circuit))))
    assert(regional.circuit == expected)
    assert(ordinary.annotations.filterNot(_ == ordinaryReceipt).toVector ==
      regional.annotations.filterNot(_ == regionalReceipt).toVector)
    Seq("storage", "no_dedup", "renames", "request", "inline_annotations", "input_sha256",
      "input_annotations_sha256", "output_sha256", "output_annotations_sha256").foreach(k => assert((v2 \ k) == (v3 \ k)))
    val normalization = v3 \ "regional_normalization"
    assert((normalization \ "input_sha256") == (normalization \ "output_sha256"))
    assert((normalization \ "output_sha256") == (v3 \ "output_sha256"))
    val changes = (normalization \ "changed_modules").asInstanceOf[JArray].arr
    assert(changes.nonEmpty)
    assert(changes.exists(r => (r \ "input_top_level_statements") != (r \ "output_top_level_statements")))
    assert(ResolveSimulationInline.forCheckpointRegions(regional) eq regional)
    save("regional-v3-output.fir", regional.circuit.serialize)
    save("regional-v3-output-annotations.json", JsonProtocol.serialize(regional.annotations.filterNot(_ == regionalReceipt)))
    save("regional-v3-receipt.json", regionalReceipt.body)
  }

  it should "reject mixed-version malformed or false regional normalization receipts without loosening v2" in {
    val regional = ResolveSimulationInline.forCheckpointRegions(input)
    val receipt = regional.annotations.collectFirst { case a: SimulationInlineResultAnnotation => a }.get
    val doc = parse(receipt.body)
    val normalization = doc \ "regional_normalization"
    val changes = (normalization \ "changed_modules").asInstanceOf[JArray].arr
    def normalized(value: JValue): JValue = doc transformField {
      case ("regional_normalization", _) => "regional_normalization" -> value
    }
    def changed(key: String, value: JValue): JValue =
      JObject(normalization.asInstanceOf[JObject].obj.filterNot(_._1 == key) :+ (key -> value))
    val invalidNormalizations = Seq(
      changed("normalization_version", JInt(2)),
      changed("scope", JString("complete_capture")),
      changed("input_sha256", JString("a" * 64)),
      changed("input_annotations_sha256", JString("b" * 64)),
      changed("passes", JArray(Nil)),
      changed("complete_fpga_capture", JBool(true)),
      changed("extra", JBool(false)),
      changed("changed_modules", JArray(changes :+ changes.head)),
      changed("changed_modules", JArray(List(changes.head merge JObject(List("input_top_level_statements" -> JInt(-1)))))),
      changed("changed_modules", JArray(List(changes.head merge JObject(List("module" -> JString("~Other|Module")))))),
      changed("changed_modules", JArray(List(changes.head merge JObject(List("serialized_sha256" -> JString("bad")))))))
    val mixed = Seq(doc transformField { case ("version", _) => "version" -> JInt(2) },
      JObject(doc.asInstanceOf[JObject].obj.filterNot(_._1 == "regional_normalization")))
    (invalidNormalizations.map(normalized) ++ mixed).zipWithIndex.foreach { case (value, index) =>
      withClue(s"regional mutation $index: ") {
        intercept[IllegalArgumentException](ResolveSimulationInline.context(SimulationInlineResultAnnotation(canonical(value))))
      }
    }
    val ordinary = parse(ResolveSimulationInline(input).annotations.collectFirst {
      case a: SimulationInlineResultAnnotation => a.body
    }.get)
    intercept[IllegalArgumentException](ResolveSimulationInline.context(SimulationInlineResultAnnotation(canonical(
      ordinary transformField { case ("version", _) => "version" -> JInt(3) }))))
  }

  it should "retain native storage and ownership rejection before regional normalization" in {
    val partial = input.copy(annotations = input.annotations.filterNot(_.isInstanceOf[InlineAnnotation]) :+
      InlineAnnotation(ComponentName("left", ModuleName("InlineFixture", CircuitName("InlineFixture")))))
    assert(intercept[IllegalArgumentException](ResolveSimulationInline.forCheckpointRegions(partial))
      .getMessage.contains("missing or reshaped inline storage"))
    Seq(input.copy(form = ChirrtlForm), input.copy(annotations = input.annotations :+ request),
      input.copy(annotations = input.annotations :+ NoDedupAnnotation(ModuleTarget("Other", "Leaf")))).foreach(bad =>
      intercept[IllegalArgumentException](ResolveSimulationInline.forCheckpointRegions(bad)))
  }

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
    assert(result.annotations.collect { case a: NoDedupAnnotation => a.target.module } == Seq("InlineFixture"))
    val protection = (doc \ "no_dedup").asInstanceOf[JArray].arr
    assert(protection.size == 2)
    assert(protection.map(r => (r \ "target", r \ "action")).toSet == Set(
      JString("~InlineFixture|Leaf") -> JString("retired"),
      JString("~InlineFixture|InlineFixture") -> JString("preserved")))
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

  it should "preserve NoDedup on partially inlined definitions and retire circuit-wide selections at the real pass" in {
    val partial = input.copy(annotations = input.annotations.filterNot(_.isInstanceOf[InlineAnnotation]) :+
      InlineAnnotation(ComponentName("left", ModuleName("InlineFixture", CircuitName("InlineFixture")))))
    // Native FIRRTL 1.6 aliases a surviving repeated Leaf's tracking target
    // onto the promoted copy. Keep the complete state audit fail-closed.
    val error = intercept[IllegalArgumentException](ResolveSimulationInline(partial))
    assert(error.getMessage.contains("missing or reshaped inline storage"))
    val combinational = """circuit InlineFixture :
  module Leaf :
    input data : UInt<8>
    output value : UInt<8>
    value <= data
  module InlineFixture :
    input data : UInt<8>
    output value : UInt<8>
    inst left of Leaf
    inst right of Leaf
    left.data <= data
    right.data <= left.value
    value <= right.value
"""
    val stateless = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(combinational), ChirrtlForm,
      Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
    val result = ResolveSimulationInline(stateless.copy(annotations = stateless.annotations ++
      partial.annotations.filter(a => a.isInstanceOf[NoDedupAnnotation] ||
        a.isInstanceOf[InlineAnnotation] || a.isInstanceOf[RunFirrtlTransformAnnotation])))
    assert(result.circuit.modules.exists(_.name == "Leaf"))
    assert(result.annotations.collect { case a: NoDedupAnnotation => a }.toSet ==
      partial.annotations.collect { case a: NoDedupAnnotation => a }.toSet)
    val doc = ResolveSimulationInline.context(result.annotations.collectFirst {
      case a: SimulationInlineResultAnnotation => a }.get)
    assert((doc \ "no_dedup").asInstanceOf[JArray].arr.forall(r => (r \ "action") == JString("preserved")))
    val all = input.copy(annotations = input.annotations.filterNot(_.isInstanceOf[InlineAnnotation]) :+
      InlineAnnotation(CircuitName("InlineFixture")))
    val flat = ResolveSimulationInline(all)
    assert(flat.circuit.modules.map(_.name) == Seq("InlineFixture"))
    assert(flat.annotations.collect { case a: NoDedupAnnotation => a.target.module } == Seq("InlineFixture"))
  }

  it should "reject stale foreign duplicate or already unreachable NoDedup constraints before inline consumption" in {
    val leaf = NoDedupAnnotation(ModuleTarget("InlineFixture", "Leaf"))
    val dead = lower.circuit.modules.head.asInstanceOf[firrtl.ir.Module].copy(name = "Dead")
    Seq(input.copy(annotations = input.annotations :+ leaf),
      input.copy(annotations = input.annotations :+ NoDedupAnnotation(ModuleTarget("Other", "Leaf"))),
      input.copy(annotations = input.annotations :+ NoDedupAnnotation(ModuleTarget("InlineFixture", "Absent"))),
      input.copy(circuit = input.circuit.copy(modules = input.circuit.modules :+ dead),
        annotations = input.annotations :+ NoDedupAnnotation(ModuleTarget("InlineFixture", "Dead"))))
      .foreach(bad => intercept[IllegalArgumentException](ResolveSimulationInline(bad)))
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
    val invalid = Seq(replace("version", JInt(1)), replace("input_sha256", JString("bad")),
      replace("producer", JString("unknown")), replace("request", JArray(Nil)),
      replace("inline_annotations", JArray(Nil)), replace("renames", JArray(Nil)), replace("renames", JString("")),
      replace("storage", JArray(rows :+ rows.head)), replace("shape", JString("opaque")),
      replace("from", JString("~Other|Other>reg")), replace("no_dedup", JNothing),
      replace("no_dedup", JString("bad")), replace("action", JString("retired")),
      replace("definition_sha256", JString("bad")), replace("target", JString("~Other|Leaf")))
    invalid.foreach(d => intercept[IllegalArgumentException](
      ResolveSimulationInline.context(SimulationInlineResultAnnotation(canonical(d)))))
    intercept[IllegalArgumentException](ResolveSimulationInline.context(receipt.copy(body = receipt.body + " ")))
  }
}
