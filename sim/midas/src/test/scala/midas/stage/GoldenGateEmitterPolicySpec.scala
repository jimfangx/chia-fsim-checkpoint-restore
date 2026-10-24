// See LICENSE for license details.
package midas.stage

import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations.{Annotation, DeletedAnnotation, JsonProtocol, NoTargetAnnotation}
import firrtl.options.Dependency
import firrtl.stage.{Forms, RunFirrtlTransformAnnotation}
import firrtl.stage.transforms.Compiler
import midas.checkpoint.{SemanticCleanEmitter, SemanticStateInventory}
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

private case object EmitterPolicyMarker extends NoTargetAnnotation
private class UnapprovedVerilogEmitter extends firrtl.VerilogEmitter

class GoldenGateEmitterPolicySpec extends AnyFlatSpec {
  private def request = RunFirrtlTransformAnnotation(new firrtl.VerilogEmitter)
  private def consumed = GoldenGateEmitterPolicy.consumeEmitterRequest(Seq(request))
  private def receipt = consumed.head.asInstanceOf[GoldenGateEmitterPolicyAnnotation]
  private val source = Parser.parse("""circuit Top :
  module Top :
    input clock : Clock
    input data : UInt<8>
    output value : UInt<8>
    reg stored : UInt<8>, clock
    stored <= data
    value <= stored
""")
  private def emit(observed: Boolean): (CircuitState, firrtl.Transform) = {
    val emitter = if (observed) new SemanticCleanEmitter else new firrtl.SystemVerilogEmitter
    val low = new Compiler(Forms.LowForm).execute(CircuitState(source, ChirrtlForm)).copy(form = LowForm)
    val output = new Compiler(Seq(Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter)).execute(
      low.copy(annotations = consumed :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
    (output, emitter)
  }

  behavior.of("Golden Gate default emitter request policy")

  it should "resolve the default request supplied by the actual runtime AddDefaults after fixed stage emission selection" in {
    val stage = new firrtl.stage.phases.AddDefaults().transform(Seq(
      firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
    val result = GoldenGateEmitterPolicy.consumeEmitterRequest(stage)
    assert(result.count(_.isInstanceOf[GoldenGateEmitterPolicyAnnotation]) == 1)
    assert(result.contains(firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
    assert(!result.exists(_.isInstanceOf[RunFirrtlTransformAnnotation]))
  }

  it should "consume the exact default request at its original position and retain its full immutable input" in {
    val input = request
    val result = GoldenGateEmitterPolicy.consumeEmitterRequest(Seq(EmitterPolicyMarker, input))
    assert(result.head == EmitterPolicyMarker && result.size == 2)
    assert(!result.exists(_.isInstanceOf[RunFirrtlTransformAnnotation]))
    val h = result.last.asInstanceOf[GoldenGateEmitterPolicyAnnotation]
    assert(h.requestJSON == JsonProtocol.serialize(Seq(input)))
    val row = parse(GoldenGateEmitterPolicy.contextJSON(h))
    assert((row \ "request") == parse(h.requestJSON))
    assert((row \ "request_sha256") == JString(SemanticStateInventory.sha256(h.requestJSON)))
    assert(JsonProtocol.deserialize(JsonProtocol.serialize(Seq(h))) == Seq(h))
  }

  it should "reject absent duplicate foreign subclass and previously consumed stage requests" in {
    Seq[Seq[Annotation]](Nil, Seq(request, request), Seq(request, receipt), Seq(receipt),
      Seq(RunFirrtlTransformAnnotation(new firrtl.SystemVerilogEmitter)),
      Seq(RunFirrtlTransformAnnotation(new UnapprovedVerilogEmitter)),
      Seq(RunFirrtlTransformAnnotation(new firrtl.transforms.DeadCodeElimination)),
      Seq(DeletedAnnotation("firrtl.stage.phases.AddDefaults", request))).foreach { values =>
      intercept[IllegalArgumentException] { GoldenGateEmitterPolicy.consumeEmitterRequest(values) }
    }
  }

  it should "bind the actual plain and observed emitter to their sole actual SystemVerilog output" in {
    Seq(false, true).foreach { observed =>
      val (output, emitter) = emit(observed)
      val result = GoldenGateEmitterPolicy.emissionBinding(consumed, output, emitter, observed).head
      assert(result.fileSuffix == ".golden-gate-emitter.json")
      val binding = parse(result.body)
      assert((binding \ "actual_emitter") == JString(emitter.getClass.getName))
      assert((binding \ "observed_emission") == JBool(observed))
      assert((binding \ "emitted_verilog_sha256") == JString(SemanticStateInventory.sha256(output.getEmittedCircuit.value)))
      assert((binding \ "consumed_policy") == parse(GoldenGateEmitterPolicy.contextJSON(receipt)))
      assert((binding \ "consumed_policy_sha256") == JString(SemanticStateInventory.sha256(GoldenGateEmitterPolicy.contextJSON(receipt))))
    }
  }

  it should "reject missing duplicated changed or revived policy history at the final boundary" in {
    val (output, emitter) = emit(true)
    val missing = output.annotations.filterNot(_.isInstanceOf[GoldenGateEmitterPolicyAnnotation])
    val badReceipts = Seq(receipt.copy(policy = "other"), receipt.copy(requestJSON = "[]"),
      receipt.copy(requestJSON = receipt.requestJSON + " "))
    val changes = Seq(missing, output.annotations :+ receipt, output.annotations :+ request) ++
      badReceipts.map(h => missing :+ h)
    changes.foreach { annotations =>
      intercept[IllegalArgumentException] {
        GoldenGateEmitterPolicy.emissionBinding(consumed, output.copy(annotations = annotations), emitter, true)
      }
    }
  }

  it should "retain consumed policy through bank specialization and reject changed history at both emission boundaries" in {
    val low = new Compiler(Forms.LowForm).execute(CircuitState(source, ChirrtlForm)).copy(form = LowForm)
    val bank = midas.checkpoint.SimulatorBankProbe(low.copy(annotations =
      consumed :+ midas.checkpoint.SimulatorBankProbeAnnotation))
    val xdc = midas.targetutils.xdc.XDCFiles.allFiles.map(f => midas.passes.XDCOutputAnnotation("", Some(f.fileSuffix)))
    midas.checkpoint.SimulatorBankProbe.emissionBinding(bank, "fixture RTL", bank.annotations ++ xdc)
    val context = bank.annotations.collectFirst {
      case GoldenGateOutputFileAnnotation(body, ".bank-probe-context.json") => parse(body)
    }.get
    val rows = (context \ "annotations").asInstanceOf[JArray].arr
    assert(rows.size == 1 && (rows.head \ "kind") == JString("golden_gate_emitter_policy"))
    assert((rows.head \ "value") == parse(GoldenGateEmitterPolicy.contextJSON(receipt)))
    val without = bank.annotations.filterNot(_.isInstanceOf[GoldenGateEmitterPolicyAnnotation])
    Seq(without, bank.annotations :+ receipt, without :+ receipt.copy(policy = "foreign"),
      without :+ receipt.copy(requestJSON = "[]")).foreach { annotations =>
      Seq((bank.copy(annotations = annotations), bank.annotations ++ xdc),
        (bank, annotations ++ xdc)).foreach { case (state, emitted) =>
        intercept[IllegalArgumentException] {
          midas.checkpoint.SimulatorBankProbe.emissionBinding(state, "fixture RTL", emitted)
        }
      }
    }
    intercept[IllegalArgumentException] {
      midas.checkpoint.SimulatorBankProbe(low.copy(annotations = Seq(request,
        midas.checkpoint.SimulatorBankProbeAnnotation)))
    }
  }

  it should "leave generated non-emitter requests live for their own contract and reject them at bank preflight" in {
    val (_, elaborated) = midas.ElaborateChiselSubCircuit(new junctions.Nasti2AXI4IdentityModule(
      freechips.rocketchip.amba.axi4.AXI4BundleParameters(32, 64, 4)))
    val generatedRequests = elaborated.collect { case a: RunFirrtlTransformAnnotation => a }
    assert(generatedRequests.size == 1)
    val generated = generatedRequests.head
    assert(generated.transform.getClass == classOf[firrtl.passes.InlineInstances])
    assert(elaborated.exists(_.isInstanceOf[firrtl.passes.InlineAnnotation]))
    val annotations = consumed :+ generated
    GoldenGateEmitterPolicy.validatePreserved(consumed, annotations)
    assert(annotations.contains(generated))
    val low = new Compiler(Forms.LowForm).execute(CircuitState(source, ChirrtlForm)).copy(form = LowForm)
    val failure = intercept[IllegalArgumentException] {
      midas.checkpoint.SimulatorBankProbe(low.copy(annotations = annotations :+
        midas.checkpoint.SimulatorBankProbeAnnotation))
    }
    assert(failure.getMessage.contains("RunFirrtlTransformAnnotation"))
    assert(annotations.contains(generated))
    // Even after final output exists, emitter-policy validation does not
    // reinterpret another transform request as consumed emitter history.
    val (output, emitter) = emit(true)
    val actual = output.copy(annotations = output.annotations :+ generated)
    GoldenGateEmitterPolicy.emissionBinding(consumed, actual, emitter, true)
    assert(actual.annotations.contains(generated))
  }

  it should "reject every revived emitter implementation while reporting the exact pending class" in {
    Seq[firrtl.Transform](new firrtl.VerilogEmitter, new firrtl.SystemVerilogEmitter,
      new SemanticCleanEmitter, new UnapprovedVerilogEmitter).foreach { emitter =>
      val failure = intercept[IllegalArgumentException] {
        GoldenGateEmitterPolicy.validatePreserved(consumed,
          consumed :+ RunFirrtlTransformAnnotation(emitter))
      }
      assert(failure.getMessage.contains("unconsumed emitter request"))
      assert(failure.getMessage.contains(emitter.getClass.getName))
    }
  }

  it should "reject policy selection mismatches extra results missing results and wrong result formats" in {
    val (output, emitter) = emit(true)
    val actual = output.annotations.collectFirst { case a: firrtl.EmittedVerilogCircuitAnnotation => a }.get
    val rest = output.annotations.filterNot(_.isInstanceOf[firrtl.EmittedCircuitAnnotation[_]])
    val bad = Seq(rest, output.annotations :+ actual,
      rest :+ actual.copy(value = actual.value.copy(outputSuffix = ".v")),
      rest :+ actual.copy(value = actual.value.copy(name = "Foreign")),
      rest :+ actual.copy(value = actual.value.copy(value = "")),
      rest :+ firrtl.EmittedFirrtlCircuitAnnotation(firrtl.EmittedFirrtlCircuit("Top", "circuit Top :", ".fir")))
    bad.foreach { annotations =>
      intercept[IllegalArgumentException] {
        GoldenGateEmitterPolicy.emissionBinding(consumed, output.copy(annotations = annotations), emitter, true)
      }
    }
    intercept[IllegalArgumentException] { GoldenGateEmitterPolicy.emissionBinding(consumed, output, emitter, false) }
    intercept[IllegalArgumentException] {
      GoldenGateEmitterPolicy.emissionBinding(consumed, output, new firrtl.VerilogEmitter, true)
    }
  }
}
