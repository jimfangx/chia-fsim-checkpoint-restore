// See LICENSE for license details.
package midas.checkpoint

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import firrtl.{CircuitState, LowForm, WDefInstance, WRef, WSubField}
import firrtl.annotations.{JsonProtocol, ModuleTarget}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.transforms.BlackBoxInlineAnno
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import midas.widgets._
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

/** These are the real Chisel endpoint blackboxes, not a handwritten ABI. */
class FAMEExistingNativeTransportFixture extends chisel3.RawModule {
  val params = CheckpointNativeEndpointParams(130, 3, 3, 1, "a" * 64)
  val io = chisel3.IO(new CheckpointNativeCaptureTransportIO(params))
  val endpoint = chisel3.Module(new CheckpointNativeCaptureTransportBlackBox(params))
  io <> endpoint.io
}
class FAMEExistingPhysicalTransportFixture extends chisel3.RawModule {
  val cfg = CheckpointCaptureWidgetParams(4, "a" * 64, Seq("left", "fame", "right"), physicalClockGates = 3)
  val io = chisel3.IO(new CheckpointPhysicalCaptureTransportIO(3, 3))
  val endpoint = chisel3.Module(new CheckpointPhysicalCaptureTransportBlackBox(cfg))
  io <> endpoint.io
}

class FAMEClockCutTransportWiringSpec extends AnyFlatSpec {
  import SemanticStateInventory.{canonical, sha256}
  private val base = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("fame-transport")
  Files.createDirectories(base)
  private def fixture(native: Boolean, nested: Boolean = false): (CircuitState, firrtl.annotations.InstanceTarget) = {
    val out = base.resolve((if (native) "native" else "physical") + (if (nested) "-nested" else "-root"))
    Files.createDirectories(out)
    val emitted = new ChiselStage().execute(Array("-X", "low", "--target-dir", out.resolve("elaboration").toString),
      Seq(ChiselGeneratorAnnotation(() => if (native) new FAMEExistingNativeTransportFixture else new FAMEExistingPhysicalTransportFixture)))
    val circuit = emitted.collectFirst { case firrtl.stage.FirrtlCircuitAnnotation(c) => c }.get
    val ext = circuit.modules.collectFirst { case m: ExtModule if m.defname ==
      (if (native) "CheckpointNativeCaptureTransport" else "CheckpointPhysicalCaptureTransport") => m }.get
    val source = FAMEClockCutWitnessFixture.prepared()
    val root = source.circuit.modules.find(_.name == source.circuit.main).get.asInstanceOf[Module]
    val originalInputs = Seq(Port(NoInfo, "ownerPresent", Input, UIntType(IntWidth(3))),
      Port(NoInfo, "ownerFrozen", Input, UIntType(IntWidth(3))), Port(NoInfo, "ownerError", Input, UIntType(IntWidth(3))),
      Port(NoInfo, "oldCycle", Input, UIntType(IntWidth(64))))
    // Export the complete real transport ABI for runtime tests; only the
    // independent owner inputs, old cycle and raw clock have named aliases.
    val special = Map("owner_present" -> "ownerPresent", "owner_frozen" -> "ownerFrozen",
      "owner_error" -> "ownerError", "target_cycle" -> "oldCycle", "clock" -> "hostClock")
    val interface = originalInputs ++ ext.ports.filterNot(p => special.contains(p.name)).map(p => p.copy(name = "transport_" + p.name))
    def connect(p: Port): Statement = {
      val pin = WSubField(WRef("transport"), p.name, p.tpe)
      val r = WRef(special.getOrElse(p.name, "transport_" + p.name), p.tpe)
      if (p.direction == Input) Connect(NoInfo, pin, r) else Connect(NoInfo, r, pin)
    }
    val host = if (!nested) root.copy(ports = root.ports ++ interface,
      body = Block(Seq(root.body, WDefInstance(NoInfo, "transport", ext.name, firrtl.Utils.module_type(ext))) ++ ext.ports.map(connect)))
    else Module(NoInfo, "TransportHost", Seq(Port(NoInfo, "hostClock", Input, ClockType)) ++ interface,
      Block(Seq(WDefInstance(NoInfo, "transport", ext.name, firrtl.Utils.module_type(ext))) ++ ext.ports.map(connect)))
    val top = if (!nested) host else root.copy(ports = root.ports ++ interface,
      body = Block(Seq(root.body, WDefInstance(NoInfo, "peer", host.name, firrtl.Utils.module_type(host))) ++ host.ports.map { p =>
        val pin = WSubField(WRef("peer"), p.name, p.tpe); val r = WRef(p.name, p.tpe)
        if (p.direction == Input) Connect(NoInfo, pin, r) else Connect(NoInfo, r, pin)
      }))
    val modules = source.circuit.modules.filterNot(_.name == root.name) ++ Seq(top, ext) ++ (if (nested) Seq(host) else Nil)
    // Preserve VerilogPrep's connectors until the real witness reopens them
    // with fresh wire names and corresponding annotation renames.
    val c = source.circuit.copy(modules = modules)
    val resources = if (native) CheckpointNativeCaptureTransportBlackBox.resources else Seq(
      "CheckpointPhysicalCaptureTransport.sv", "CheckpointPhysicalClockCut.sv", "CheckpointCaptureTransport.sv", "CheckpointCaptureCut.sv")
    val inlines = resources.map { n =>
      val stream = getClass.getResourceAsStream("/" + n)
      val text = try new String(stream.readAllBytes(), UTF_8) finally stream.close()
      BlackBoxInlineAnno(firrtl.annotations.ModuleName(ext.name, firrtl.annotations.CircuitName(c.main)), n, text)
    }
    val state = source.copy(circuit = c, annotations = source.annotations ++ inlines)
    val t = ModuleTarget(c.main, c.main)
    val target = if (nested) t.instOf("peer", host.name).instOf("transport", ext.name) else t.instOf("transport", ext.name)
    (state, target)
  }
  private val owners = Seq("left", "fame", "right")
  private def all(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(s: Statement): Unit = { out += s; s.foreachStmt(visit) }; visit(s); out.toSeq
  }
  behavior.of("Existing transport FAME witness qualification")

  it should "reuse each exact real transport with independent mandatory acknowledgements and no new storage" in {
    for (native <- Seq(false, true); nested <- Seq(false, true)) {
      val (s, target) = fixture(native, nested)
      val r = FAMEClockCutTransportWiring.connect(s, target, "clockA", owners, "fame")
      FAMEClockCutTransportWiring.validate(s, target, "clockA", owners, "fame", r)
      def storage(x: CircuitState) = x.circuit.modules.collect { case m: Module =>
        m.name -> all(m.body).collect { case d: DefRegister => d.serialize; case d: DefMemory => d.serialize }
      }.toMap
      // The witness has already reopened VerilogPrep pin aliases. Attachment
      // must preserve that exact register/clock/reset IR without further edits.
      assert(storage(r.witness.state) == storage(r.state))
      assert(GeneratedExternalOwners.instances(s, s.circuit.main).map(_._1) ==
        GeneratedExternalOwners.instances(r.state, r.state.circuit.main).map(_._1))
      val binding = parse(r.binding)
      assert((binding \ "independent_owner_acknowledgements_required") == JBool(true))
      Seq("new_storage_bits", "new_collectors", "new_transports").foreach(k => assert((binding \ k) == JInt(0)))
      Seq("global_target_cut_proven", "production_freeze_owners_bound", "complete_fpga_capture",
        "deployable_capture_image", "metasim_restore_supported").foreach(k => assert((binding \ k) == JBool(false)))
      val text = r.state.circuit.serialize
      assert(text.contains("checkpoint_fame_transport_owner_frozen <= ownerFrozen"))
      assert(text.contains("bits(checkpoint_fame_transport_owner_frozen, 2, 2)"))
      assert(text.contains("bits(checkpoint_fame_transport_owner_frozen, 0, 0)"))
      assert(!text.contains("checkpoint_f2_adapter"))
      intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(r.state.copy(annotations = Nil)))
      val dir = base.resolve((if (native) "native" else "physical") + (if (nested) "-nested" else "-root"))
      val emitter = new SemanticCleanEmitter
      val rtl = new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
        .execute(r.state.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(emitter.getClass)))).getEmittedCircuit.value
      val files = Seq("reference.fir" -> s.circuit.serialize, "wired.fir" -> r.state.circuit.serialize,
        "wired.sv" -> rtl, "binding.json" -> r.binding, "witness-binding.json" -> r.witness.binding)
      files.foreach { case (n, v) => Files.write(dir.resolve(n), v.getBytes(UTF_8)) }
      Files.write(dir.resolve("emission.json"), canonical(JObject(files.map { case (n, v) => n -> JString(sha256(v)) }.toList)).getBytes(UTF_8))
    }
  }

  it should "reject stale targets geometry source overrides clocks and missing or aliased independent owners" in {
    val (s, target) = fixture(true)
    def reject(x: CircuitState) = intercept[IllegalArgumentException](
      FAMEClockCutTransportWiring.connect(x, target, "clockA", owners, "fame"))
    def change(f: Statement => Statement): CircuitState = s.copy(circuit = s.circuit.copy(modules = s.circuit.modules.map {
      case m: Module =>
        def walk(x: Statement): Statement = f(x.mapStmt(walk))
        m.copy(body = walk(m.body))
      case m => m
    }))
    reject(s.copy(form = firrtl.HighForm))
    intercept[IllegalArgumentException](FAMEClockCutTransportWiring.connect(s, target.copy(instance = "missing"), "clockA", owners, "fame"))
    intercept[IllegalArgumentException](FAMEClockCutTransportWiring.connect(s, target, "clockA", owners.reverse, "missing"))
    reject(s.copy(annotations = s.annotations.filterNot(_.isInstanceOf[BlackBoxInlineAnno])))
    reject(s.copy(annotations = s.annotations.map {
      case a: BlackBoxInlineAnno => a.copy(text = a.text + "\n")
      case a => a
    }))
    reject(s.copy(circuit = s.circuit.copy(modules = s.circuit.modules.map {
      case m: ExtModule if m.name == target.ofModule => m.copy(params = m.params.map {
        case p: IntParam if p.name == "OWNERS" => p.copy(value = 4)
        case p => p
      })
      case m => m
    })))
    for (pin <- Seq("owner_present", "owner_frozen", "owner_error")) {
      reject(change { case c: Connect if c.loc.serialize == "transport." + pin => c.copy(expr = UIntLiteral(7, IntWidth(3))); case x => x })
      reject(change { case c: Connect if c.loc.serialize == "transport." + pin => firrtl.ir.EmptyStmt; case x => x })
    }
    reject(change { case c: Connect if c.loc.serialize == "transport.owner_frozen" => c.copy(expr = WRef("ownerPresent", UIntType(IntWidth(3)))); case x => x })
    reject(change { case c: Connect if c.loc.serialize == "transport.clock" => c.copy(expr = WRef("clockA", ClockType)); case x => x })
    val r = FAMEClockCutTransportWiring.connect(s, target, "clockA", owners, "fame")
    val bad = r.state.copy(circuit = r.state.circuit.copy(modules = r.state.circuit.modules.map {
      case m: Module =>
        def walk(x: Statement): Statement = x match {
          case c: Connect if c.loc.serialize == "transport.target_cycle" => c.copy(expr = WRef("oldCycle", UIntType(IntWidth(64))))
          case x => x.mapStmt(walk)
        }
        m.copy(body = walk(m.body))
      case m => m
    }))
    val rehashed = canonical(parse(r.binding).transformField {
      case ("wired_reference_sha256", _) => "wired_reference_sha256" -> JString(sha256(bad.circuit.serialize))
    })
    intercept[IllegalArgumentException](FAMEClockCutTransportWiring.validate(s, target, "clockA", owners, "fame",
      r.copy(state = bad, binding = rehashed)))
    intercept[IllegalArgumentException](FAMEClockCutTransportWiring.connect(r.state, target, "clockA", owners, "fame"))
    val (nested, nt) = fixture(true, true)
    for (value <- Seq[Expression](UIntLiteral(7, IntWidth(3)), WRef("ownerPresent", UIntType(IntWidth(3))))) {
      val badAncestor = nested.copy(circuit = nested.circuit.copy(modules = nested.circuit.modules.map {
        case m: Module if m.name == nested.circuit.main =>
          def walk(x: Statement): Statement = x match {
            case c: Connect if c.loc.serialize == "peer.ownerFrozen" => c.copy(expr = value)
            case x => x.mapStmt(walk)
          }
          m.copy(body = walk(m.body))
        case m => m
      }))
      intercept[IllegalArgumentException](FAMEClockCutTransportWiring.connect(badAncestor, nt, "clockA", owners, "fame"))
    }
    val staleHop = nt.copy(path = nt.path.map { case (i, _) => i -> firrtl.annotations.TargetToken.OfModule("WrongHost") })
    intercept[IllegalArgumentException](FAMEClockCutTransportWiring.connect(nested, staleHop, "clockA", owners, "fame"))
    val registeredAck = s.copy(circuit = s.circuit.copy(modules = s.circuit.modules.map {
      case m: Module if m.name == s.circuit.main =>
        def walk(x: Statement): Statement = x match {
          case c: Connect if c.loc.serialize == "transport.owner_frozen" => c.copy(expr = WRef("registeredAck", UIntType(IntWidth(3))))
          case x => x.mapStmt(walk)
        }
        m.copy(body = Block(Seq(walk(m.body), DefRegister(NoInfo, "registeredAck", UIntType(IntWidth(3)),
          WRef("hostClock", ClockType), UIntLiteral(0, IntWidth(1)), WRef("registeredAck", UIntType(IntWidth(3)))),
          Connect(NoInfo, WRef("registeredAck", UIntType(IntWidth(3))), WRef("ownerFrozen", UIntType(IntWidth(3)))))))
      case m => m
    }))
    reject(registeredAck)
  }
}
