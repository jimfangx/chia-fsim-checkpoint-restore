// See LICENSE for license details.
package midas.stage

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser, Transform}
import firrtl.annotations.{NoTargetAnnotation, ReferenceTarget}
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.HostTransforms
import midas.checkpoint.{SemanticStateInventory, SimulatorCheckpointInventoryAnnotation, SimulatorStateInventory}
import midas.passes.{HostClockSink, HostClockSource}
import midas.stage.phases.ConfigParametersAnnotation
import midas.targetutils.xdc.XDCPathToCircuitAnnotation
import org.chipsalliance.cde.config.Parameters
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

private case object HostPipelineMarker extends NoTargetAnnotation

/** Exercises the exact post-mapping entry used by GoldenGateCompilerPhase.
  * This transform changes observable behavior and adds real storage. A marker
  * annotation alone would miss the old discarded-CircuitState bug.
  */
private class InjectHostState extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm
  def execute(state: CircuitState): CircuitState = {
    val fragment = Parser.parse("""circuit Fragment :
  module Fragment :
    input clock : Clock
    input reset : UInt<1>
    output injected : UInt<8>
    reg host_owner : UInt<8>, clock with :
      reset => (reset, UInt<8>(19))
    host_owner <= add(host_owner, UInt<8>(3))
    injected <= host_owner
""").modules.head.asInstanceOf[Module].body
    def removeOutput(s: Statement): Statement = s match {
      case c: Connect if c.loc.serialize == "injected" => EmptyStmt
      case _ => s.mapStmt(removeOutput)
    }
    state.copy(circuit = state.circuit.copy(modules = state.circuit.modules.map {
      case m: Module if m.name == state.circuit.main =>
        m.copy(body = Block(Seq(removeOutput(m.body), fragment)))
      case m => m
    }), annotations = state.annotations :+ HostPipelineMarker)
  }
}

private class RejectHostState extends InjectHostState {
  override def execute(state: CircuitState): CircuitState =
    throw new IllegalArgumentException("host owner binding rejected")
}

private class InjectBankHostState extends InjectHostState {
  override def execute(state: CircuitState): CircuitState = {
    val changed = super.execute(state)
    changed.copy(annotations = changed.annotations.filterNot(_ == HostPipelineMarker))
  }
}

class GoldenGateHostPipelineSpec extends AnyFlatSpec {
  private val source = """circuit HostPipeline :
  module HostConsumer :
    input clock : Clock
    input reset : UInt<1>
    output count : UInt<8>
    reg always : UInt<8>, clock with :
      reset => (reset, UInt<8>(0))
    always <= add(always, UInt<8>(1))
    count <= always
  module HostPipeline :
    input clock : Clock
    input otherClock : Clock
    input reset : UInt<1>
    output count : UInt<8>
    output injected : UInt<8>
    inst consumer of HostConsumer
    consumer.clock <= otherClock
    consumer.reset <= reset
    count <= consumer.count
    injected <= UInt<8>(0)
"""
  private def lower = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(source), ChirrtlForm)).copy(form = LowForm)

  private def compile(inventory: Boolean, transforms: Seq[Transform] = Nil,
                      extra: firrtl.AnnotationSeq = Nil): firrtl.AnnotationSeq = {
    implicit val p: Parameters = Parameters.empty.alterPartial {
      case HostTransforms => transforms.map(Dependency.fromTransform(_))
    }
    val annos = Seq(ConfigParametersAnnotation(p),
      HostClockSource(ReferenceTarget("HostPipeline", "HostPipeline", Nil, "clock", Nil)),
      HostClockSink(ReferenceTarget("HostPipeline", "HostPipeline", Nil, "consumer", Nil).field("clock")),
      XDCPathToCircuitAnnotation(None, None),
      firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])) ++
      (if (inventory) Seq(SimulatorCheckpointInventoryAnnotation("HostPipeline")) else Nil) ++ extra
    new GoldenGateCompilerPhase().compileSimulator(lower.copy(annotations = annos))
  }
  private def verilog(a: firrtl.AnnotationSeq): String = a.collectFirst {
    case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value
  }.get
  private def file(a: firrtl.AnnotationSeq, suffix: String): String = a.collectFirst {
    case GoldenGateOutputFileAnnotation(body, `suffix`) => body
  }.get
  private def save(name: String, value: String): Unit = sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
    val out = Paths.get(dir).resolve("host-pipeline"); Files.createDirectories(out)
    Files.write(out.resolve(name), value.getBytes(UTF_8))
  }

  behavior.of("Golden Gate production host-to-FPGA pipeline")

  it should "consume exactly the stage circuit and retain immutable complete input identity" in {
    val input = Parser.parse(source)
    val request = firrtl.stage.FirrtlCircuitAnnotation(input)
    val (selected, annotations) = GoldenGateCompilerPhase.consumeInputCircuit(Seq(HostPipelineMarker, request))
    assert(selected == input && annotations.head == HostPipelineMarker)
    assert(!annotations.exists(_.isInstanceOf[firrtl.stage.FirrtlCircuitAnnotation]))
    val h = annotations.collect { case a: GoldenGateInputCircuitAnnotation => a }.head
    assert(h.circuit == input.main && h.modules == input.modules.map(_.name))
    assert(h.circuitSHA256 == SemanticStateInventory.sha256(input.serialize))
    val changed = input.copy(modules = input.modules.reverse)
    val (_, altered) = GoldenGateCompilerPhase.consumeInputCircuit(Seq(firrtl.stage.FirrtlCircuitAnnotation(changed)))
    assert(!altered.contains(h))
    Seq(Nil, Seq(request, request), annotations :+ request).foreach { invalid =>
      intercept[IllegalArgumentException] { GoldenGateCompilerPhase.consumeInputCircuit(invalid) }
    }
  }

  it should "emit functional generated banks through the real backend and bind every probe input" in {
    val injection = new InjectBankHostState
    val policies = Seq(firrtl.passes.memlib.InferReadWriteAnnotation,
      firrtl.passes.memlib.DefaultReadFirstAnnotation,
      firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation)
    val historicalInput = Parser.parse("""circuit HistoricalInput :
  module Worker :
    input clock : Clock
    output count : UInt<8>
    reg count_reg : UInt<8>, clock
    count_reg <= add(count_reg, UInt<8>(1))
    count <= count_reg
  module HistoricalInput :
    input clock : Clock
    output count : UInt<8>
    inst worker of Worker
    worker.clock <= clock
    count <= worker.count
""")
    val (_, consumedInput) = GoldenGateCompilerPhase.consumeInputCircuit(
      Seq(firrtl.stage.FirrtlCircuitAnnotation(historicalInput)))
    val context = consumedInput ++ Seq(
      midas.passes.PreLinkRenamingTestFixture.history,
      midas.passes.BridgeExtractionTestFixture.history,
      firrtl.stage.InfoModeAnnotation("use"),
      firrtl.stage.OutputFileAnnotation("fixture.fir"),
      firrtl.options.TargetDirAnnotation("checkpoint-fixture"),
      ConfigPackageAnnotation("fixture.config"), ConfigStringAnnotation("FixtureConfig"),
      OutputBaseFilenameAnnotation("fixture"), firrtl.stage.AllowUnrecognizedAnnotations,
      chisel3.experimental.EnumAnnotations.EnumDefAnnotation("fixture.Mode", Map("idle" -> BigInt(0), "busy" -> BigInt(3))),
      firrtl.annotations.DeletedAnnotation("firrtl.stage.phases.AddCircuit",
        firrtl.stage.FirrtlFileAnnotation("historical-fixture.fir")),
      firrtl.annotations.DeletedAnnotation("firrtl.options.phases.GetIncludes",
        firrtl.options.InputAnnotationFileAnnotation("historical-fixture.anno.json")),
      midas.passes.ChannelClockInfoAnnotation(Map(
        "fixture_request" -> firesim.lib.bridgeutils.RationalClock("fixture_core", 2, 4),
        "fixture_response" -> firesim.lib.bridgeutils.RationalClock("fixture_uncore", 1, 1))))
    val constraints = Seq(
      midas.InternalXDCAnnotation(midas.targetutils.xdc.XDCFiles.Synthesis,
        "set_property DONT_TOUCH true [get_cells {}]",
        ReferenceTarget("HostPipeline", "HostPipeline", Nil, "consumer", Nil)),
      midas.InternalXDCAnnotation(midas.targetutils.xdc.XDCFiles.Implementation,
        "set_false_path -from [get_ports {}]",
        ReferenceTarget("HostPipeline", "HostPipeline", Nil, "reset", Nil)))
    val a = compile(inventory = true, Seq(injection),
      Seq(midas.checkpoint.SimulatorBankProbeAnnotation,
        midas.core.TargetBoxAnnotation(firrtl.annotations.ModuleTarget("HostPipeline", "HostPipeline")
          .instOf("consumer", "HostConsumer")),
        firrtl.transforms.DedupedResult(firrtl.annotations.ModuleTarget("Historical", "OriginalConsumer"),
          Some(firrtl.annotations.ModuleTarget("HostPipeline", "HostPipeline").instOf("consumer", "HostConsumer")), 0.25),
        firrtl.transforms.DedupedResult(firrtl.annotations.ModuleTarget("Historical", "Deleted"), None, -1)) ++
        constraints ++ context ++ policies)
    context.foreach(c => assert(a.contains(c)))
    save("compiler-input.fir", historicalInput.serialize)
    val historicalLink = midas.passes.PreLinkRenamingTestFixture.run()
    save("pre-link-input.fir", midas.passes.PreLinkRenamingTestFixture.input.serialize)
    save("pre-link-output.fir", historicalLink.circuit.serialize)
    val extraction = midas.passes.BridgeExtractionTestFixture.run()
    save("bridge-extraction-input.fir", midas.passes.BridgeExtractionTestFixture.input.circuit.serialize)
    save("bridge-extraction-promoted.fir", extraction.promoted.circuit.serialize)
    save("bridge-extraction-output.fir", midas.passes.BridgeExtractionTestFixture.result().circuit.serialize)
    save("bridge-extraction-connectivity.json", firrtl.annotations.JsonProtocol.serialize(
      midas.passes.BridgeExtractionTestFixture.connectivity(extraction.commuted)))
    policies.foreach(c => assert(a.contains(c)))
    val sv = verilog(a)
    assert(sv.contains("ckpt_freeze") && sv.contains("ckpt_clock") && sv.contains("ckpt_saved_data"))
    val names = Seq(".bank-probe-reference.fir", ".bank-probe-state.json", ".bank-probe-instrumented.fir",
      ".bank-probe-binding.json", ".bank-probe-descriptors.json", ".bank-probe-context.json", ".bank-probe-emission.json")
    val binding = parse(file(a, names.last))
    assert((binding \ "deployable_capture_image") == JBool(false))
    assert((binding \ "emitted_verilog_sha256") == JString(SemanticStateInventory.sha256(sv)))
    names.dropRight(1).foreach { n =>
      assert((binding \ "inputs" \ n) == JString(SemanticStateInventory.sha256(file(a, n))))
    }
    val compilerContext = parse(file(a, ".bank-probe-context.json"))
    assert((compilerContext \ "reference_sha256") == JString(SemanticStateInventory.sha256(file(a, names.head))))
    assert((compilerContext \ "instrumented_sha256") == JString(SemanticStateInventory.sha256(file(a, names(2)))))
    assert((compilerContext \ "annotations").asInstanceOf[JArray].arr.size == context.size)
    val rows = (parse(file(a, ".bank-probe-state.json")) \ "state").asInstanceOf[JArray].arr
    assert(rows.size == 2 && rows.exists(r => (r \ "hierarchy") == JString("HostPipeline.host_owner")))
    assert(rows.exists(r => (r \ "hierarchy") == JString("HostPipeline.consumer.always")))
    // Final emitted-state inventory additionally owns the bank response latch.
    assert((parse(file(a, ".simulator-state.json")) \ "state").asInstanceOf[JArray].arr.size == 5)
    val xdc = a.collect { case midas.passes.XDCOutputAnnotation(body, Some(suffix)) => suffix -> body }.toMap
    assert(xdc.size == 2)
    xdc.foreach { case (suffix, body) =>
      assert((binding \ "emitted_xdc_sha256" \ suffix) == JString(SemanticStateInventory.sha256(body)))
      save("bank-probe" + suffix, body)
    }
    assert(xdc(".synthesis.xdc").contains("set_property DONT_TOUCH true [get_cells consumer]"))
    assert(xdc(".implementation.xdc").contains("set_false_path -from [get_ports reset]"))
    val noXDC = intercept[IllegalArgumentException] {
      midas.checkpoint.SimulatorBankProbe.emissionBinding(lower.copy(annotations =
        Seq(midas.checkpoint.SimulatorBankProbeAnnotation) ++ a.collect {
          case f: GoldenGateOutputFileAnnotation if names.dropRight(1).contains(f.fileSuffix) => f
        }), sv, a.filterNot(_.isInstanceOf[midas.passes.XDCOutputAnnotation]))
    }
    assert(noXDC.getMessage.contains("constraint files"))
    save("bank-probe.sv", sv)
    names.foreach(n => save(n.drop(1), file(a, n)))
  }

  it should "reject unknown annotation ownership before bank specialization" in {
    val e = intercept[IllegalArgumentException] {
      compile(inventory = true, Seq(new InjectHostState), Seq(midas.checkpoint.SimulatorBankProbeAnnotation))
    }
    assert(e.getMessage.contains("HostPipelineMarker"))
  }

  it should "reject duplicate bank probes and a partial inventory root" in {
    val probe = midas.checkpoint.SimulatorBankProbeAnnotation
    val duplicate = intercept[IllegalArgumentException] { compile(inventory = false, extra = Seq(probe, probe)) }
    assert(duplicate.getMessage.contains("exactly one"))
    val partial = intercept[IllegalArgumentException] {
      compile(inventory = false, extra = Seq(probe, SimulatorCheckpointInventoryAnnotation("HostConsumer")))
    }
    assert(partial.getMessage.contains("complete circuit root"))
  }

  it should "retain actual host-injected storage, behavior and annotations through final emission" in {
    val injection = new InjectHostState
    val a = compile(inventory = true, Seq(injection))
    val sv = verilog(a)
    val manifest = file(a, ".simulator-state.json")
    val reference = file(a, ".simulator-reference.fir")
    val rows = (parse(manifest) \ "state").asInstanceOf[JArray].arr
    assert(a.count(_ == HostPipelineMarker) == 1)
    assert(rows.size == 2 && rows.exists(r => (r \ "hierarchy") == JString("HostPipeline.host_owner")))
    assert(sv.contains("host_owner") && sv.contains("'h13") && sv.contains("'h3"))
    // A Verilog keyword is renamed by the emitter. Both its exact name and
    // the injected owner must be inventoried from that same internal state.
    assert(!rows.exists(r => (r \ "hierarchy") == JString("HostPipeline.consumer.always")))
    // VerilogPrep uses instance connectors whose diagnostic serialization is
    // not parser input. Check emitted storage independently instead of parsing
    // that internal IR back into a different circuit representation.
    assert("(?m)^  reg \\[7:0\\]".r.findAllIn(sv).size == rows.size)
    assert(rows.exists(r => (r \ "hierarchy") == JString("HostPipeline.consumer.always_")))
    assert((parse(manifest) \ "reference_sha256") == JString(SemanticStateInventory.sha256(reference)))
    assert((parse(manifest) \ "external_runtime_required") == JBool(true))
    val binding = file(a, ".simulator-emission.json")
    val owners = file(a, ".simulator-external-owners.json")
    assert((parse(owners) \ "reachable_external_instances") == JArray(Nil))
    assert((parse(owners) \ "reference_sha256") == JString(SemanticStateInventory.sha256(reference)))
    val identity = parse(binding)
    for ((key, text) <- Seq("emission_reference_sha256" -> reference,
      "simulator_manifest_sha256" -> manifest, "external_owners_sha256" -> owners, "emitted_verilog_sha256" -> sv)) {
      assert((identity \ key) == JString(SemanticStateInventory.sha256(text)))
    }
    assert((identity \ "host_pipeline_version") == JInt(2))
    save("host-pipeline.sv", sv); save("host-pipeline.fir", reference)
    save("host-pipeline-manifest.json", manifest)
    save("host-pipeline-emission.json", binding)
    save("host-pipeline-external-owners.json", owners)
    save("original-input.fir", lower.circuit.serialize)
    val oldPath = new Compiler(Seq(Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(
      lower.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter]))))
    save("discarded-host.sv", oldPath.getEmittedCircuit.value)
  }

  it should "retain host clock wiring in the emitted FPGA RTL and its state inventory" in {
    val a = compile(inventory = true)
    val sv = verilog(a)
    assert(sv.contains("always @(posedge clock)"))
    assert(sv.contains("assign consumer_clock = clock;"))
    val rows = (parse(file(a, ".simulator-state.json")) \ "state").asInstanceOf[JArray].arr
    assert(rows.size == 1 && (rows.head \ "clock_expression") == JString("clock"))
    assert(!a.exists(_.isInstanceOf[HostClockSink]))
    save("clock-wired.sv", sv)
  }

  it should "emit the same RTL with inventory enabled and disabled" in {
    val enabled = compile(inventory = true, Seq(new InjectHostState))
    val disabled = compile(inventory = false, Seq(new InjectHostState))
    assert(verilog(enabled) == verilog(disabled))
    assert(!disabled.exists { case GoldenGateOutputFileAnnotation(_, s) => s.startsWith(".simulator-"); case _ => false })
  }

  it should "propagate a failed host transform before emitting any simulator image" in {
    val e = intercept[Exception](compile(inventory = true, Seq(new RejectHostState)))
    val causes = Iterator.iterate[Throwable](e)(_.getCause).takeWhile(_ != null).toSeq
    assert(causes.exists(e => Option(e.getMessage).exists(_.contains("host owner binding rejected"))))
  }
}
