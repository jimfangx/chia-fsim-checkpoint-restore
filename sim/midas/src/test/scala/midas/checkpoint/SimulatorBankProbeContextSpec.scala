// See LICENSE for license details.
package midas.checkpoint

import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations._
import firrtl.stage.{AllowUnrecognizedAnnotations, FirrtlCircuitAnnotation, InfoModeAnnotation, OutputFileAnnotation, Forms}
import firrtl.stage.transforms.Compiler
import chisel3.experimental.EnumAnnotations.{EnumComponentAnnotation, EnumDefAnnotation}
import midas.stage.{ConfigStringAnnotation, GoldenGateOutputFileAnnotation}
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

private case object UnknownCompilerContext extends NoTargetAnnotation

class SimulatorBankProbeContextSpec extends AnyFlatSpec {
  private val state = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse("""circuit Top :
  module Top :
    input clock : Clock
    input data : UInt<8>
    output value : UInt<8>
    reg stored : UInt<8>, clock
    stored <= data
    value <= stored
"""), ChirrtlForm)).copy(form = LowForm)
  private def probe(extra: Seq[Annotation]) = SimulatorBankProbe(state.copy(annotations =
    Seq(SimulatorBankProbeAnnotation) ++ extra))
  private def context(extra: Seq[Annotation]) = parse(probe(extra).annotations.collectFirst {
    case GoldenGateOutputFileAnnotation(body, ".bank-probe-context.json") => body
  }.get)

  behavior.of("Explicit post-lowering compiler context")

  it should "preserve every literal value and bind both actual circuit representations" in {
    val annotations = Seq(InfoModeAnnotation("ignore"), ConfigStringAnnotation("A_B"),
      EnumDefAnnotation("Mode", Map("wide" -> (BigInt(1) << 120), "zero" -> BigInt(0))))
    val result = probe(annotations)
    annotations.foreach(a => assert(result.annotations.contains(a)))
    val c = context(annotations)
    assert((c \ "reference_sha256") == JString(SemanticStateInventory.sha256(state.circuit.serialize)))
    assert((c \ "instrumented_sha256") == JString(SemanticStateInventory.sha256(result.circuit.serialize)))
    assert((c \ "annotations").asInstanceOf[JArray].arr.size == 3)
    assert((c \ "annotations").asInstanceOf[JArray].arr.exists(r =>
      (r \ "value" \ "values" \ "wide") == JInt(BigInt(1) << 120)))
    assert(c == context(annotations.reverse))
    assert(c != context(annotations.updated(1, ConfigStringAnnotation("B_A"))))
  }

  it should "leave the instrumented storage and behavior independent of inert context" in {
    assert(probe(Nil).circuit == probe(Seq(OutputFileAnnotation("other.fir"))).circuit)
  }

  it should "reject duplicate compiler options and conflicting enum definitions" in {
    Seq(Seq(InfoModeAnnotation("use"), InfoModeAnnotation("ignore")),
      Seq(ConfigStringAnnotation("A"), ConfigStringAnnotation("A")),
      Seq(EnumDefAnnotation("Mode", Map("a" -> BigInt(0))),
          EnumDefAnnotation("Mode", Map("a" -> BigInt(1))))).foreach { annotations =>
      val e = intercept[IllegalArgumentException](probe(annotations))
      assert(e.getMessage.contains("duplicate compiler option") || e.getMessage.contains("conflicting enum"))
    }
  }

  it should "retain input-parser permissiveness without admitting opaque annotations" in {
    val unknown = UnrecognizedAnnotation(JObject(List("class" -> JString("hidden.Owner"),
      "target" -> JString("~Top|Top>stored"))))
    val e = intercept[IllegalArgumentException](probe(Seq(AllowUnrecognizedAnnotations, unknown)))
    assert(e.getMessage.contains("UnrecognizedAnnotation"))
  }

  it should "reject pass requests, stale circuits, targeted enums and arbitrary output payloads" in {
    val targeted = EnumComponentAnnotation(ComponentName("stored", ModuleName("Top", CircuitName("Top"))), "Mode")
    val pending = firrtl.stage.RunFirrtlTransformAnnotation(new firrtl.transforms.DeadCodeElimination)
    Seq[Annotation](pending, FirrtlCircuitAnnotation(state.circuit), targeted,
      GoldenGateOutputFileAnnotation("stale hierarchy", ".hidden.json"), UnknownCompilerContext).foreach { a =>
      assert(!SimulatorBankProbeContext.accepts(a))
      val e = intercept[IllegalArgumentException](probe(Seq(a)))
      assert(e.getMessage.contains(a.getClass.getName))
    }
  }

  it should "reject unknown values in the context serializer itself" in {
    intercept[IllegalArgumentException] {
      SimulatorBankProbeContext.snapshot(Seq(UnknownCompilerContext), "reference", "instrumented")
    }
  }
}
