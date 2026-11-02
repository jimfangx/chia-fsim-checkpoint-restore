// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import firrtl.{CircuitState, LowForm, WDefInstance, WRef, WSubField}
import firrtl.annotations.{JsonProtocol, ModuleTarget, TargetToken}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.passes.ResolveSimulationInline
import midas.stage.GoldenGateCompilerPhase
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

/** The registered Chisel FPGATop widget and its real inline history are reused;
  * an independently FAME-transformed execution supplies the observed roles.
  */
class FAMEClockCutTransportSelectionSpec extends AnyFlatSpec {
  import SemanticStateInventory.{canonical, sha256}
  private val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("fame-compiler-transport")
  Files.createDirectories(out)
  private lazy val registered = new NativeCheckpointRegionWidgetGraftSpec {
    override protected def fixtureOutput: Path = out.resolve("registered-widget")
  }
  private val request = FAMEClockCutTransportSelectionAnnotation("clockA", "fixture.functional")
  private lazy val source: CircuitState = {
    val widget = registered.compilerProbeBefore(Nil)
    // Merge ordinary lowered FIRRTL, before VerilogPrep introduces emitter-only
    // instance connectors and local pin wires. Those connectors cannot safely
    // be fed back through the normal compiler's metadata-resolution passes.
    val transformed = FAMEClockCutWitnessFixture.transform(true)
    val observed = new Compiler(Forms.LowForm).execute(transformed.copy(form = firrtl.HighForm,
      annotations = transformed.annotations.filter {
        case _: firrtl.transforms.DontTouchAnnotation | _: FAMEClockProgressSignalAnnotation |
             _: FAMEClockProgressModelAnnotation => true
        case _ => false
      })).copy(form = LowForm)
    val root = widget.circuit.modules.find(_.name == widget.circuit.main).get.asInstanceOf[Module]
    val observer = observed.circuit.modules.find(_.name == observed.circuit.main).get.asInstanceOf[Module]
    val special = Map("hostClock" -> "clock", "hostReset" -> "reset")
    val ports = observer.ports.filterNot(p => special.contains(p.name)).map(p => p.copy(name = "observed_" + p.name))
    val instance = "fameObserver"
    val connections = observer.ports.map { p =>
      val pin = WSubField(WRef(instance), p.name, p.tpe)
      val port = WRef(special.getOrElse(p.name, "observed_" + p.name), p.tpe)
      if (p.direction == Input) Connect(NoInfo, pin, port) else Connect(NoInfo, port, pin)
    }
    val combined = root.copy(ports = root.ports ++ ports, body = Block(Seq(root.body,
      WDefInstance(NoInfo, instance, observer.name, firrtl.Utils.module_type(observer))) ++ connections))
    val existing = widget.circuit.modules.map(_.name).toSet
    assert(observed.circuit.modules.forall(m => !existing(m.name)))
    val modules = widget.circuit.modules.map(m => if (m.name == root.name) combined else m) ++ observed.circuit.modules
    val circuit = firrtl.passes.ResolveFlows.run(firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(
      firrtl.passes.ToWorkingIR.run(widget.circuit.copy(modules = modules)))))
    val roles = observed.annotations.collect {
      case a: FAMEClockProgressModelAnnotation => a
      case a: FAMEClockProgressSignalAnnotation =>
        assert(a.target.module == observer.name)
        a.copy(target = a.target.copy(circuit = root.name, module = root.name,
          path = Seq(TargetToken.Instance(instance) -> TargetToken.OfModule(observer.name)) ++ a.target.path))
      case a: firrtl.transforms.DontTouchAnnotation => a.copy(target = a.target.copy(circuit = root.name))
    }
    val rootedWidget = widget.annotations.map {
      case a: NativeCheckpointWidgetExecutionAnnotation => a.copy(target =
        ModuleTarget(root.name, root.name).instOf("top", "FPGATop").instOf("sim", "SimWrapper"))
      case a => a
    }
    CircuitState(circuit, LowForm, rootedWidget ++ roles :+ request)
  }
  private def save(name: String, value: String): Unit = Files.write(out.resolve(name), value.getBytes(UTF_8))
  private def change(input: CircuitState)(f: Statement => Statement): CircuitState =
    input.copy(circuit = input.circuit.copy(modules = input.circuit.modules.map {
      case m: Module =>
        def walk(s: Statement): Statement = f(s.mapStmt(walk))
        m.copy(body = walk(m.body))
      case m => m
    }))

  behavior.of("Actual compiler existing-widget FAME witness selection")

  it should "select the actual registered widget after inline and preserve the independent image guard" in {
    val result = GoldenGateCompilerPhase.prepareCheckpointTransport(source)
    assert(!result.annotations.exists(_.isInstanceOf[FAMEClockCutTransportSelectionAnnotation]))
    val outputs = result.annotations.collect { case a: FAMEClockCutTransportSelectionBindingAnnotation => a }
    assert(outputs.size == 1)
    val binding = parse(outputs.head.body)
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound", "metasim_restore_supported")
      .foreach(k => assert((binding \ k) == JBool(false)))
    val after = ResolveSimulationInline(source)
    FAMEClockCutTransportSelection.validate(source, after, result)
    val peer = parse(NativeCheckpointWidgetPeerRouting.inspect(source, after))
    val JString(path) = peer \ "transport"
    val live = GeneratedExternalOwners.instances(after, after.circuit.main).toMap
    var target: firrtl.annotations.IsModule = ModuleTarget(after.circuit.main, after.circuit.main)
    val parts = path.split('.').toSeq
    parts.indices.drop(1).foreach { i => target = target.instOf(parts(i), live(parts.take(i + 1).mkString(".")).name) }
    val direct = FAMEClockCutTransportWiring.connect(after, target.asInstanceOf[firrtl.annotations.InstanceTarget],
      request.primaryDomain, Seq("fixture.functional", "fixture.host"), request.fameOwner)
    assert(result.circuit == direct.state.circuit)
    assert((binding \ "transport_binding") == parse(direct.binding))
    assert(result.annotations.filterNot(_.isInstanceOf[FAMEClockCutTransportSelectionBindingAnnotation]).toVector ==
      direct.state.annotations.filterNot(_.isInstanceOf[FAMEClockCutTransportSelectionAnnotation]).toVector)
    val changed = result.copy(circuit = result.circuit.copy(info = FileInfo(StringLit("changed"))))
    val rehashed = changed.copy(annotations = changed.annotations.map {
      case a: FAMEClockCutTransportSelectionBindingAnnotation => a.copy(body = canonical(parse(a.body).transformField {
        case ("wired_reference_sha256", _) => "wired_reference_sha256" -> JString(sha256(changed.circuit.serialize))
      }))
      case a => a
    })
    intercept[IllegalArgumentException](FAMEClockCutTransportSelection.validate(source, after, rehashed))
    val error = intercept[IllegalArgumentException](GoldenGateCompilerPhase.prepareCheckpointBoundary(source))
    assert(error.getMessage.contains("external transport state owner required before image emission"))
    intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(result.copy(annotations = Nil)))
    intercept[IllegalArgumentException](GoldenGateCompilerPhase.prepareCheckpointTransport(result))
    val emitter = new SemanticCleanEmitter
    val emitted = new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter)).execute(
      result.copy(annotations = result.annotations.filterNot(_.isInstanceOf[firrtl.EmitCircuitAnnotation]) :+
        firrtl.EmitCircuitAnnotation(emitter.getClass))).getEmittedCircuit.value
    val files = Seq("reference.fir" -> source.circuit.serialize, "reference.anno.json" -> JsonProtocol.serialize(source.annotations),
      "wired.fir" -> result.circuit.serialize, "wired.anno.json" -> JsonProtocol.serialize(result.annotations),
      "wired.sv" -> emitted, "binding.json" -> outputs.head.body, "transport-binding.json" -> direct.binding)
    files.foreach { case (name, value) => save(name, value) }
    save("emission.json", canonical(JObject(files.map { case (name, value) => name -> JString(sha256(value)) }.toList)))
  }

  it should "reject missing tied or aliased owners at the actual compiler boundary" in {
    val root = source.circuit.modules.find(_.name == source.circuit.main).get.asInstanceOf[Module]
    val frozenInputs = root.ports.filter(_.name.endsWith("_reservedFrozen"))
    val presentInputs = root.ports.filter(_.name.endsWith("_reservedPresent"))
    assert(frozenInputs.size == 1 && presentInputs.size == 1)
    val frozen = "top." + frozenInputs.head.name
    for (replacement <- Seq[Option[Expression]](None, Some(UIntLiteral(3, IntWidth(2))),
      Some(WRef(presentInputs.head.name, UIntType(IntWidth(2)))))) {
      var mutations = 0
      val bad = change(source) {
        case c: Connect if c.loc.serialize == frozen =>
          mutations += 1
          replacement.map(e => c.copy(expr = e)).getOrElse(EmptyStmt)
        case s => s
      }
      assert(mutations == 1 && bad.circuit != source.circuit,
        "negative case must mutate exactly one actual root owner connection")
      intercept[IllegalArgumentException](GoldenGateCompilerPhase.prepareCheckpointTransport(bad))
    }
  }

  it should "validate cardinality preservation rooted execution and authentic inline history before wiring" in {
    val empty = CircuitState(firrtl.Parser.parse("circuit Empty :\n  module Empty :\n    input clock : Clock\n"), LowForm)
    assert(GoldenGateCompilerPhase.prepareCheckpointTransport(empty) == empty)
    assert(GoldenGateCompilerPhase.prepareCheckpointBoundary(empty) == empty)
    val bad = Seq(request.copy(primaryDomain = ""), request.copy(fameOwner = ""), request.copy(primaryDomain = "bad:domain"))
    bad.foreach(a => intercept[IllegalArgumentException](FAMEClockCutTransportSelection.validateCardinality(Seq(a))))
    intercept[IllegalArgumentException](FAMEClockCutTransportSelection.validateCardinality(Seq(request, request)))
    intercept[IllegalArgumentException](FAMEClockCutTransportSelection.validatePreserved(Nil, Seq(request)))
    intercept[IllegalArgumentException](FAMEClockCutTransportSelection.validatePreserved(Seq(request), Nil))
    val after = ResolveSimulationInline(source)
    intercept[IllegalArgumentException](FAMEClockCutTransportSelection(source,
      after.copy(annotations = after.annotations.filterNot(_.isInstanceOf[FAMEClockCutTransportSelectionAnnotation]))))
    val changed = after.copy(circuit = after.circuit.copy(info = FileInfo(StringLit("changed"))))
    intercept[IllegalArgumentException](FAMEClockCutTransportSelection(source, changed))
    val unrooted = source.copy(annotations = source.annotations.map {
      case a: NativeCheckpointWidgetExecutionAnnotation => a.copy(target = ModuleTarget(source.circuit.main, "FPGATop").instOf("sim", "SimWrapper"))
      case a => a
    })
    intercept[IllegalArgumentException](GoldenGateCompilerPhase.prepareCheckpointTransport(unrooted))
    val absent = source.copy(annotations = source.annotations.map {
      case _: FAMEClockCutTransportSelectionAnnotation => request.copy(fameOwner = "missing")
      case a => a
    })
    intercept[IllegalArgumentException](GoldenGateCompilerPhase.prepareCheckpointTransport(absent))
  }
}
