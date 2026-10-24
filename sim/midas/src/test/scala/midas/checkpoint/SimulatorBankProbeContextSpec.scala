// See LICENSE for license details.
package midas.checkpoint

import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations._
import firrtl.stage.{AllowUnrecognizedAnnotations, FirrtlCircuitAnnotation, FirrtlFileAnnotation, InfoModeAnnotation, OutputFileAnnotation, Forms}
import firrtl.stage.transforms.Compiler
import chisel3.experimental.EnumAnnotations.{EnumComponentAnnotation, EnumDefAnnotation}
import midas.stage.{ConfigStringAnnotation, GoldenGateOutputFileAnnotation}
import midas.passes.ChannelClockInfoAnnotation
import firesim.lib.bridgeutils.RationalClock
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

  it should "preserve only exact consumed input-file events without reopening their historical paths" in {
    val source = DeletedAnnotation("firrtl.stage.phases.AddCircuit", FirrtlFileAnnotation("absent-original.fir"))
    val include = DeletedAnnotation("firrtl.options.phases.GetIncludes",
      firrtl.options.InputAnnotationFileAnnotation("absent-original.anno.json"))
    val annotations = Seq(source, include)
    val result = probe(annotations)
    annotations.foreach(a => assert(result.annotations.contains(a)))
    assert(result.circuit == probe(Nil).circuit)
    val rows = (context(annotations) \ "annotations").asInstanceOf[JArray].arr
    assert(rows.size == 2 && rows.forall(r => (r \ "kind") == JString("deleted_input")))
    assert(rows.map(_ \ "value" \ "annotation_class").toSet == Set(
      JString("firrtl.stage.FirrtlFileAnnotation"), JString("firrtl.options.InputAnnotationFileAnnotation")))
    assert(context(annotations) == context(annotations.reverse))
    assert(context(annotations) != context(Seq(source.copy(anno = FirrtlFileAnnotation("other.fir")), include)))
    assert(intercept[IllegalArgumentException](probe(Seq(source, source))).getMessage.contains("duplicate deleted"))
  }

  it should "reject deleted owners nested wrappers unknown producers mismatched events and live inputs" in {
    val pending = firrtl.stage.RunFirrtlTransformAnnotation(new firrtl.transforms.DeadCodeElimination)
    val owner = firrtl.transforms.DontTouchAnnotation(ModuleTarget("Top", "Top").ref("stored"))
    val file = FirrtlFileAnnotation("old.fir")
    Seq[Annotation](
      DeletedAnnotation("firrtl.stage.phases.AddCircuit", owner),
      DeletedAnnotation("firrtl.stage.phases.AddCircuit", pending),
      DeletedAnnotation("unknown.Producer", file),
      DeletedAnnotation("firrtl.options.phases.GetIncludes", file),
      DeletedAnnotation("firrtl.stage.phases.AddCircuit", firrtl.options.InputAnnotationFileAnnotation("old.json")),
      DeletedAnnotation("firrtl.stage.phases.AddCircuit", DeletedAnnotation("firrtl.stage.phases.AddCircuit", file)),
      DeletedAnnotation("firrtl.stage.phases.AddCircuit", UnknownCompilerContext),
      file, firrtl.options.InputAnnotationFileAnnotation("old.json")).foreach { a =>
      assert(!SimulatorBankProbeContext.accepts(a))
      assert(intercept[IllegalArgumentException](probe(Seq(a))).getMessage.contains(a.getClass.getName))
    }
    Seq("", "bad" + 0.toChar + ".fir").foreach { name =>
      assert(intercept[IllegalArgumentException] {
        probe(Seq(DeletedAnnotation("firrtl.stage.phases.AddCircuit", FirrtlFileAnnotation(name))))
      }.getMessage.contains("deleted input filename"))
    }
  }

  it should "bind every global channel clock label and exact ratio without changing storage" in {
    val clocks = ChannelClockInfoAnnotation(Map(
      "request" -> RationalClock("core", 2, 4), "response" -> RationalClock("uncore", 1, 1)))
    val result = probe(Seq(clocks))
    assert(result.annotations.contains(clocks) && result.circuit == probe(Nil).circuit)
    val row = (context(Seq(clocks)) \ "annotations").asInstanceOf[JArray].arr.head
    assert((row \ "kind") == JString("channel_clock_info"))
    assert((row \ "value" \ "request" \ "multiplier") == JInt(2))
    assert((row \ "value" \ "request" \ "divisor") == JInt(4))
    val reversed = clocks.copy(infoMap = clocks.infoMap.toSeq.reverse.toMap)
    assert(context(Seq(clocks)) == context(Seq(reversed)))
    Seq(clocks.copy(infoMap = clocks.infoMap - "response"),
      clocks.copy(infoMap = clocks.infoMap.updated("request", RationalClock("core", 1, 2))),
      clocks.copy(infoMap = clocks.infoMap.updated("request", RationalClock("other", 2, 4))),
      clocks.copy(infoMap = clocks.infoMap.updated("request", RationalClock("core", 2, 5)))).foreach { changed =>
      assert(context(Seq(clocks)) != context(Seq(changed)))
    }
    assert(intercept[IllegalArgumentException](probe(Seq(clocks, reversed))).getMessage.contains("duplicate compiler"))
  }

  it should "reject empty malformed or nonpositive channel clock metadata" in {
    val invalid = Seq(Map.empty[String, RationalClock], Map("" -> RationalClock("core", 1, 1)),
      Map("request" -> RationalClock("", 1, 1))) ++ Seq((0, 1), (-1, 1), (1, 0), (1, -1)).map {
      case (m, d) => Map("request" -> RationalClock("core", m, d))
    }
    invalid.foreach { info =>
      assert(intercept[IllegalArgumentException](probe(Seq(ChannelClockInfoAnnotation(info))))
        .getMessage.contains("invalid channel clock"))
    }
  }

  it should "reject context changes in either the bank state or final emitted annotations" in {
    val deleted = DeletedAnnotation("firrtl.stage.phases.AddCircuit", FirrtlFileAnnotation("old.fir"))
    val clock = ChannelClockInfoAnnotation(Map("request" -> RationalClock("core", 2, 4)))
    val original = probe(Seq(deleted, clock))
    val xdc = midas.targetutils.xdc.XDCFiles.allFiles.map(f => midas.passes.XDCOutputAnnotation("", Some(f.fileSuffix)))
    SimulatorBankProbe.emissionBinding(original, "fixture RTL", original.annotations ++ xdc)
    val changes = Seq(
      original.annotations.filterNot(_ == deleted),
      original.annotations.filterNot(_ == clock),
      original.annotations.map { case `deleted` => deleted.copy(anno = FirrtlFileAnnotation("other.fir")); case a => a },
      original.annotations.map { case `clock` => clock.copy(infoMap = Map("request" -> RationalClock("core", 1, 2))); case a => a })
    changes.foreach { annotations =>
      val stateError = intercept[IllegalArgumentException] {
        SimulatorBankProbe.emissionBinding(original.copy(annotations = annotations), "fixture RTL", original.annotations ++ xdc)
      }
      val emittedError = intercept[IllegalArgumentException] {
        SimulatorBankProbe.emissionBinding(original, "fixture RTL", annotations ++ xdc)
      }
      assert(stateError.getMessage.contains("compiler context changed"))
      assert(emittedError.getMessage.contains("compiler context changed"))
    }
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
