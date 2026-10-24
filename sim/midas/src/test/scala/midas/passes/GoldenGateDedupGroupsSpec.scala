// See LICENSE for license details.
package midas.passes

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{AnnotationSeq, CircuitState, ChirrtlForm, LowForm, Parser}
import firrtl.annotations._
import firrtl.transforms.{DedupGroupAnnotation, NoCircuitDedupAnnotation, NoDedupAnnotation}
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.checkpoint.SemanticStateInventory.{canonical, sha256}
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class GoldenGateDedupGroupsSpec extends AnyFlatSpec {
  private val leaves = Seq("A", "A2", "B", "B2", "C", "C2", "N")
  private def leaf(n: String) = s"""  module $n :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<8>
    output value : UInt<8>
    reg stored : UInt<8>, clock with :
      reset => (reset, UInt<8>(7))
    stored <= add(stored, data)
    value <= stored
"""
  private val source = "circuit GroupFixture :\n" + leaves.map(leaf).mkString + """  module GroupFixture :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<8>
""" + leaves.map(n => s"    output out$n : UInt<8>\n").mkString + leaves.zipWithIndex.map { case (n, i) =>
    s"""    inst inst$n of $n
    inst$n.clock <= clock
    inst$n.reset <= reset
    inst$n.data <= add(data, UInt<8>($i))
    out$n <= inst$n.value
"""
  }.mkString
  private def anno(n: String, g: String) = DedupGroupAnnotation(ModuleTarget("GroupFixture", n), g)
  private def input(extra: AnnotationSeq = Nil) = GoldenGateDedupGroups.prepare(
    CircuitState(Parser.parse(source), ChirrtlForm,
      Seq(anno("A", "alpha"), anno("A2", "alpha"), anno("B", "beta"), anno("B2", "beta"),
        NoDedupAnnotation(ModuleTarget("GroupFixture", "N"))) ++ extra))
  private def lower(state: CircuitState) = new Compiler(Forms.LowForm).execute(state).copy(form = LowForm)
  private def run(state: CircuitState) = GoldenGateDedupGroups.run(state)
  private def result(state: CircuitState) = state.annotations.collectFirst { case a: DedupGroupResultAnnotation => a }.get
  private def save(name: String, value: String): Unit = sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
    val out = Paths.get(dir).resolve("dedup"); Files.createDirectories(out)
    Files.write(out.resolve(name), value.getBytes(UTF_8))
  }
  private def emit(state: CircuitState): String = {
    val emitter = new firrtl.SystemVerilogEmitter
    new Compiler(Seq(Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(state.copy(annotations = state.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
      .getEmittedCircuit.value
  }

  behavior.of("Phase-correct Golden Gate dedup group enforcement")

  it should "deserialize the exact current Chisel wire format into a typed renamable constraint" in {
    val raw = """[{"class":"firrtl.transforms.DedupGroupAnnotation","target":"~GroupFixture|A","group":"alpha"}]"""
    assert(JsonProtocol.deserialize(raw) == Seq(anno("A", "alpha")))
    val renames = firrtl.RenameMap()
    renames.record(ModuleTarget("GroupFixture", "A"), ModuleTarget("Linked", "Renamed"))
    assert(anno("A", "alpha").update(renames) == Seq(DedupGroupAnnotation(ModuleTarget("Linked", "Renamed"), "alpha")))
  }

  it should "suppress unaware early dedup and merge only same-group modules in the actual forced pass" in {
    val original = lower(input())
    assert(original.circuit.modules.map(_.name).toSet == (leaves :+ "GroupFixture").toSet)
    val changed = run(original)
    assert(changed.circuit.modules.map(_.name).toSet == Set("A", "B", "C", "N", "GroupFixture"))
    val doc = GoldenGateDedupGroups.context(result(changed))
    val pairs = (doc \ "module_renames").asInstanceOf[JArray].arr.map(r =>
      (r \ "from").asInstanceOf[JString].s -> (r \ "to").asInstanceOf[JString].s).toMap
    assert(pairs == Map("A" -> "A", "A2" -> "A", "B" -> "B", "B2" -> "B", "C" -> "C", "C2" -> "C",
      "N" -> "N", "GroupFixture" -> "GroupFixture"))
    assert(changed.annotations.contains(NoCircuitDedupAnnotation))
    assert((new firrtl.transforms.DedupModules).runTransform(changed).circuit == changed.circuit)
    assert(changed.annotations.collect { case a: DedupGroupAnnotation => a }.toSet == Set(anno("A", "alpha"), anno("B", "beta")))
    val retired = GoldenGateDedupGroups.retire(changed)
    val production = (new EnableAndRunDedupOnce).runTransform(original)
    assert(production.circuit == retired.circuit)
    GoldenGateDedupGroups.validateFinal(production.annotations)
    assert(!retired.annotations.exists(_.isInstanceOf[DedupGroupAnnotation]))
    intercept[IllegalArgumentException](run(retired))
    save("input.fir", original.circuit.serialize); save("output.fir", changed.circuit.serialize)
    save("input-annotations.json", JsonProtocol.serialize(original.annotations))
    save("output-annotations.json", JsonProtocol.serialize(changed.annotations.filterNot(_ == result(changed))))
    save("policy.json", original.annotations.collectFirst { case a: DedupGroupPolicyAnnotation => a.body }.get)
    save("result.json", result(changed).body)
    save("retirement.json", retired.annotations.collectFirst { case a: DedupGroupRetirementAnnotation => a.body }.get)
    save("reference.sv", emit(original)); save("deduped.sv", emit(retired))
  }

  it should "keep grouped and ungrouped identical parents apart when children belong to different groups" in {
    val src = "circuit Top :\n" + Seq("A", "B").map(leaf).mkString + Seq("P" -> "A", "Q" -> "B").map {
      case (n, child) => s"""  module $n :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<8>
    output value : UInt<8>
    inst child of $child
    child.clock <= clock
    child.reset <= reset
    child.data <= data
    value <= child.value
"""
    }.mkString + """  module Top :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<8>
    output x : UInt<8>
    output y : UInt<8>
    inst p of P
    inst q of Q
    p.clock <= clock
    q.clock <= clock
    p.reset <= reset
    q.reset <= reset
    p.data <= data
    q.data <= data
    x <= p.value
    y <= q.value
"""
    val state = GoldenGateDedupGroups.prepare(CircuitState(Parser.parse(src), ChirrtlForm, Seq(
      DedupGroupAnnotation(ModuleTarget("Top", "A"), "alpha"), DedupGroupAnnotation(ModuleTarget("Top", "B"), "beta"))))
    val output = run(lower(state))
    assert(output.circuit.modules.map(_.name).toSet == Set("A", "B", "P", "Q", "Top"))
  }

  it should "retain native forced dedup behavior for inputs without group constraints" in {
    val state = lower(CircuitState(Parser.parse(source), ChirrtlForm, Seq(NoCircuitDedupAnnotation)))
    val output = run(state)
    assert(output.circuit.modules.size == 2)
    assert(output.annotations.contains(NoCircuitDedupAnnotation))
    assert(!output.annotations.exists(_.isInstanceOf[DedupGroupResultAnnotation]))
  }

  it should "reject foreign missing duplicate conflicting or malformed constraints before early optimization" in {
    Seq(anno("A", "alpha"), anno("A", "conflict"), anno("Absent", "alpha"), anno("B", ""), anno("B", "x\u0000"),
      anno("B", null), anno("B", "alpha").copy(target = ModuleTarget("Foreign", "B"))).foreach { bad =>
      intercept[IllegalArgumentException](input(Seq(bad)))
    }
    intercept[IllegalArgumentException](GoldenGateDedupGroups.prepare(input()))
    intercept[IllegalArgumentException](input(Seq(NoCircuitDedupAnnotation, NoCircuitDedupAnnotation)))
  }

  it should "reject missing policy suppression and repeated passes instead of falling back to unaware dedup" in {
    val state = lower(input())
    Seq(state.copy(annotations = state.annotations.filterNot(_ == NoCircuitDedupAnnotation)),
      state.copy(annotations = state.annotations.filterNot(_.isInstanceOf[DedupGroupPolicyAnnotation]))).foreach { bad =>
      intercept[IllegalArgumentException](run(bad))
    }
    intercept[IllegalArgumentException](run(run(state)))
    intercept[IllegalArgumentException](GoldenGateDedupGroups.retire(state))
    intercept[IllegalArgumentException](GoldenGateDedupGroups.validatePreserved(state.annotations,
      state.annotations.filterNot(_ == NoCircuitDedupAnnotation)))
  }

  it should "reject rehashed cross-group mappings malformed receipts and changed policy bindings" in {
    val output = run(lower(input()))
    val receipt = result(output)
    val doc = parse(receipt.body)
    val mappings = (doc \ "module_renames").asInstanceOf[JArray].arr
    Seq(JArray(mappings.tail), JArray(mappings :+ mappings.head), JArray(mappings.map { r =>
      if ((r \ "from") == JString("B2")) JObject(List("from" -> JString("B2"), "to" -> JString("A"))) else r
    })).foreach { rows =>
      val changed = JObject(doc.asInstanceOf[JObject].obj.map {
        case ("module_renames", _) => "module_renames" -> rows
        case pair => pair
      })
      intercept[IllegalArgumentException](GoldenGateDedupGroups.context(receipt.copy(body = canonical(changed))))
    }
    intercept[IllegalArgumentException](GoldenGateDedupGroups.context(receipt.copy(body = receipt.body + " ")))
    val changed = output.copy(annotations = output.annotations.map {
      case a: DedupGroupPolicyAnnotation => a.copy(body = canonical(JObject(parse(a.body).asInstanceOf[JObject].obj.map {
        case ("input_sha256", _) => "input_sha256" -> JString(sha256("changed"))
        case pair => pair
      })))
      case a => a
    })
    intercept[IllegalArgumentException](GoldenGateDedupGroups.retire(changed))
  }

  it should "bind the complete consumed history and suppression at both real bank emission boundaries" in {
    val retired = GoldenGateDedupGroups.retire(run(lower(input())))
    val bank = midas.checkpoint.SimulatorBankProbe(retired.copy(annotations = retired.annotations :+
      midas.checkpoint.SimulatorBankProbeAnnotation))
    val xdc = midas.targetutils.xdc.XDCFiles.allFiles.map(f => XDCOutputAnnotation("", Some(f.fileSuffix)))
    assert(midas.checkpoint.SimulatorBankProbe.emissionBinding(bank, "fixture SV", bank.annotations ++ xdc).nonEmpty)
    val history = bank.annotations.filter {
      case _: DedupGroupPolicyAnnotation | _: DedupGroupResultAnnotation | _: DedupGroupRetirementAnnotation => true
      case _ => false
    }
    (history :+ NoCircuitDedupAnnotation).foreach { missing =>
      val changed = bank.annotations.filterNot(_ == missing)
      Seq((bank.copy(annotations = changed), bank.annotations ++ xdc), (bank, changed ++ xdc),
        (bank.copy(annotations = changed), changed ++ xdc)).foreach { case (state, emitted) =>
        intercept[IllegalArgumentException](midas.checkpoint.SimulatorBankProbe.emissionBinding(state, "fixture SV", emitted))
      }
    }
    history.foreach { duplicate =>
      intercept[IllegalArgumentException](GoldenGateDedupGroups.validateFinal(bank.annotations :+ duplicate))
    }
    intercept[IllegalArgumentException](GoldenGateDedupGroups.validateFinal(bank.annotations :+ anno("A", "alpha")))
  }
}
