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

  private def consumedCircuit = midas.stage.GoldenGateCompilerPhase.consumeInputCircuit(
    Seq(FirrtlCircuitAnnotation(state.circuit)))._2.head.asInstanceOf[midas.stage.GoldenGateInputCircuitAnnotation]

  it should "retain the consumed stage circuit identity independently of the specialized hierarchy" in {
    val h = consumedCircuit
    val result = probe(Seq(h))
    assert(result.annotations.contains(h) && result.circuit == probe(Nil).circuit)
    val row = (context(Seq(h)) \ "annotations").asInstanceOf[JArray].arr.head
    assert((row \ "kind") == JString("golden_gate_input_circuit"))
    assert((row \ "value" \ "circuit_sha256") == JString(h.circuitSHA256))
    assert((row \ "value" \ "modules") == JArray(List(JString("Top"))))
    assert(intercept[IllegalArgumentException](probe(Seq(h, h))).getMessage.contains("duplicate compiler option"))
  }

  it should "reject malformed consumed circuit hashes module identities and missing roots" in {
    val h = consumedCircuit
    Seq(h.copy(circuit = ""), h.copy(circuitSHA256 = "bad"), h.copy(circuitSHA256 = "A" * 64),
      h.copy(modules = Nil), h.copy(modules = Seq("Other")), h.copy(modules = Seq("Top", "Top")),
      h.copy(modules = Seq("Top", "bad.name"))).foreach { a =>
      assert(intercept[IllegalArgumentException](probe(Seq(a))).getMessage.contains("invalid consumed circuit"))
    }
  }

  it should "reject dropped altered or reordered consumed circuit history at both final emission boundaries" in {
    val h = consumedCircuit.copy(modules = Seq("Top", "HistoricalWorker"))
    val original = probe(Seq(h))
    val xdc = midas.targetutils.xdc.XDCFiles.allFiles.map(f => midas.passes.XDCOutputAnnotation("", Some(f.fileSuffix)))
    SimulatorBankProbe.emissionBinding(original, "fixture RTL", original.annotations ++ xdc)
    Seq(None, Some(h.copy(circuitSHA256 = "1" * 64)), Some(h.copy(modules = h.modules.reverse)),
      Some(h.copy(modules = h.modules :+ "Other")),
      Some(h.copy(circuit = "Other", modules = Seq("Other")))).foreach { change =>
      val annotations = original.annotations.filterNot(_ == h) ++ change.toSeq
      Seq((original.copy(annotations = annotations), original.annotations ++ xdc),
        (original, annotations ++ xdc), (original.copy(annotations = annotations), annotations ++ xdc)).foreach {
        case (state, emitted) => assert(intercept[IllegalArgumentException] {
          SimulatorBankProbe.emissionBinding(state, "fixture RTL", emitted)
        }.getMessage.contains("compiler context changed"))
      }
    }
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

  it should "preserve consumed pre-link history independently of the current hierarchy" in {
    val h = midas.passes.PreLinkRenamingTestFixture.history
    val result = probe(Seq(h))
    assert(result.annotations.contains(h) && result.circuit == probe(Nil).circuit)
    val row = (context(Seq(h)) \ "annotations").asInstanceOf[JArray].arr.head
    assert((row \ "kind") == JString("pre_link_renaming"))
    assert((row \ "value" \ "circuit") == JString("HistoricalWrapper"))
    assert((row \ "value" \ "input_sha256") == JString(h.inputSHA256))
    assert((row \ "value" \ "output_sha256") == JString(h.outputSHA256))
    assert((row \ "value" \ "module_renames").asInstanceOf[JArray].arr.size == 3)
    assert(intercept[IllegalArgumentException](probe(Seq(h, h))).getMessage.contains("duplicate consumed pre-link"))
  }

  it should "reject malformed consumed pre-link identities hashes and nonbijective mappings" in {
    val h = midas.passes.PreLinkRenamingTestFixture.history
    Seq(h.copy(circuit = ""), h.copy(inputSHA256 = "bad"), h.copy(outputSHA256 = "A" * 64),
      h.copy(moduleRenames = Nil), h.copy(moduleRenames = h.moduleRenames.dropRight(1)),
      h.copy(moduleRenames = h.moduleRenames :+ h.moduleRenames.head),
      h.copy(moduleRenames = h.moduleRenames.updated(0, "" -> "Shared_0")),
      h.copy(moduleRenames = h.moduleRenames.updated(0, "Shared" -> "External_0"))).foreach { a =>
      assert(intercept[IllegalArgumentException](probe(Seq(a))).getMessage.contains("invalid consumed pre-link"))
    }
  }

  it should "reject dropped or altered pre-link history at both final-emission boundaries" in {
    val h = midas.passes.PreLinkRenamingTestFixture.history
    val original = probe(Seq(h))
    val xdc = midas.targetutils.xdc.XDCFiles.allFiles.map(f => midas.passes.XDCOutputAnnotation("", Some(f.fileSuffix)))
    SimulatorBankProbe.emissionBinding(original, "fixture RTL", original.annotations ++ xdc)
    val changes = Seq(None, Some(h.copy(inputSHA256 = "0" * 64)), Some(h.copy(outputSHA256 = "1" * 64)),
      Some(h.copy(moduleRenames = h.moduleRenames.updated(0, "Shared" -> "OtherShared"))))
    changes.foreach { change =>
      val annotations = original.annotations.filterNot(_ == h) ++ change.toSeq
      Seq((original.copy(annotations = annotations), original.annotations ++ xdc),
        (original, annotations ++ xdc), (original.copy(annotations = annotations), annotations ++ xdc)).foreach {
        case (state, emitted) => assert(intercept[IllegalArgumentException] {
          SimulatorBankProbe.emissionBinding(state, "fixture RTL", emitted)
        }.getMessage.contains("compiler context changed"))
      }
    }
  }

  it should "preserve consumed extraction history without retargeting the removed wrapper" in {
    val h = midas.passes.BridgeExtractionTestFixture.history
    val result = probe(Seq(h))
    assert(result.annotations.contains(h) && result.circuit == probe(Nil).circuit)
    val row = (context(Seq(h)) \ "annotations").asInstanceOf[JArray].arr.head
    assert((row \ "kind") == JString("bridge_extraction"))
    assert((row \ "value" \ "wrapper") == JString(h.wrapper))
    assert((row \ "value" \ "bridge_instances") == JArray(h.bridgeInstances.map(JString(_)).toList))
    assert((row \ "value" \ "connectivity_sha256") == JString(h.connectivitySHA256))
    Seq(h, h.copy(inputSHA256 = "1" * 64)).foreach { other =>
      assert(intercept[IllegalArgumentException](probe(Seq(h, other))).getMessage.contains("duplicate consumed bridge"))
    }
  }

  it should "reject malformed extraction names hashes instances and wrappers" in {
    val h = midas.passes.BridgeExtractionTestFixture.history
    val first = Target.deserialize(h.bridgeInstances.head).asInstanceOf[InstanceTarget]
    Seq(h.copy(circuit = ""), h.copy(wrapper = h.circuit), h.copy(inputSHA256 = "bad"),
      h.copy(promotedSHA256 = "A" * 64), h.copy(outputSHA256 = ""), h.copy(connectivitySHA256 = "no"),
      h.copy(bridgeInstances = Nil), h.copy(bridgeInstances = h.bridgeInstances :+ h.bridgeInstances.head),
      h.copy(bridgeInstances = Seq(first.copy(circuit = "Other").serialize)),
      h.copy(bridgeInstances = Seq(first.copy(module = h.circuit).serialize)),
      h.copy(bridgeInstances = Seq(first.instOf("nested", "Other").serialize)),
      h.copy(bridgeInstances = Seq(ModuleTarget(h.wrapper, h.wrapper).serialize))).foreach { a =>
      assert(intercept[IllegalArgumentException](probe(Seq(a))).getMessage.contains("invalid consumed bridge"))
    }
  }

  it should "reject dropped or altered extraction history at both final-emission boundaries" in {
    val h = midas.passes.BridgeExtractionTestFixture.history
    val original = probe(Seq(h))
    val xdc = midas.targetutils.xdc.XDCFiles.allFiles.map(f => midas.passes.XDCOutputAnnotation("", Some(f.fileSuffix)))
    SimulatorBankProbe.emissionBinding(original, "fixture RTL", original.annotations ++ xdc)
    Seq(None, Some(h.copy(inputSHA256 = "0" * 64)), Some(h.copy(promotedSHA256 = "1" * 64)),
      Some(h.copy(outputSHA256 = "2" * 64)), Some(h.copy(connectivitySHA256 = "3" * 64)),
      Some(h.copy(bridgeInstances = h.bridgeInstances.reverse))).foreach { change =>
      val annotations = original.annotations.filterNot(_ == h) ++ change.toSeq
      Seq((original.copy(annotations = annotations), original.annotations ++ xdc),
        (original, annotations ++ xdc), (original.copy(annotations = annotations), annotations ++ xdc)).foreach {
        case (state, emitted) => assert(intercept[IllegalArgumentException] {
          SimulatorBankProbe.emissionBinding(state, "fixture RTL", emitted)
        }.getMessage.contains("compiler context changed"))
      }
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

  it should "reject changed or dropped deduplication history at either final-emission boundary" in {
    val history = firrtl.transforms.DedupedResult(ModuleTarget("Historical", "Original"),
      Some(ModuleTarget("Top", "Top")), 0.25)
    val original = probe(Seq(history))
    val xdc = midas.targetutils.xdc.XDCFiles.allFiles.map(f => midas.passes.XDCOutputAnnotation("", Some(f.fileSuffix)))
    SimulatorBankProbe.emissionBinding(original, "fixture RTL", original.annotations ++ xdc)
    val changed = Seq(original.annotations.filterNot(_.isInstanceOf[firrtl.transforms.DedupedResult]),
      original.annotations.map {
        case a: firrtl.transforms.DedupedResult => a.copy(index = 0.5)
        case a => a
      })
    changed.foreach { annotations =>
      Seq((original.copy(annotations = annotations), original.annotations ++ xdc),
          (original, annotations ++ xdc),
          (original.copy(annotations = annotations), annotations ++ xdc)).foreach { case (state, emitted) =>
        assert(intercept[IllegalArgumentException] {
          SimulatorBankProbe.emissionBinding(state, "fixture RTL", emitted)
        }.getMessage.contains("deduplication history changed"))
      }
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
