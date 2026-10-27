// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import chisel3._
import chisel3.stage.ChiselStage
import firrtl.{CircuitState, LowForm, Parser, WDefInstance, WRef, WSubField}
import firrtl.annotations.{CircuitName, JsonProtocol, ModuleTarget, ReferenceTarget}
import firrtl.ir.{Module => FirrtlModule, Input => FirrtlInput, Output => FirrtlOutput, _}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import freechips.rocketchip.diplomacy.LazyModule
import firesim.lib.bridgeutils.{BridgeIOAnnotation, RationalClock}
import midas.core._
import midas.passes.ResolveSimulationInline
import midas.passes.fame.{FAMEChannelConnectionAnnotation, TargetClockChannel}
import midas.stage.GoldenGateOutputFileAnnotation
import midas.widgets._
import org.chipsalliance.cde.config.Parameters
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

private case class UnsupportedRegionWidgetOwner(target: ReferenceTarget)
    extends firrtl.annotations.SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget) = copy(target = n)
}

class NativeCheckpointRegionWidgetGraftSpec extends AnyFlatSpec {
  private val out = Paths.get(sys.props.getOrElse("checkpoint.region.widget.out",
    Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("native-region-widget").toString))
  private def save(name: String, text: String): Unit = {
    Files.createDirectories(out.resolve(name).getParent); Files.write(out.resolve(name), text.getBytes(UTF_8))
  }
  private def resolved(c: Circuit): Circuit = firrtl.passes.ResolveFlows.run(
    firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(firrtl.passes.ToWorkingIR.run(c))))
  private class Binding extends NativeCheckpointWidgetReservationBinding(
      14, 4, Seq("fixture.functional", "fixture.host"), 2) {
    protected def connectOwners(top: FPGATopImp, capture: CheckpointCaptureSideband): Unit = {
      capture.targetCycle := IO(Input(UInt(64.W))).suggestName("reservedCycle")
      capture.ownerPresent := IO(Input(UInt(2.W))).suggestName("reservedPresent")
      capture.ownerFrozen := IO(Input(UInt(2.W))).suggestName("reservedFrozen")
      capture.ownerError := IO(Input(UInt(2.W))).suggestName("reservedError")
      IO(Output(Bool())).suggestName("reservedFreeze") := capture.freeze
    }
  }
  private lazy val before = {
    val ref = ReferenceTarget("Fixture", "Target", Nil, "clock_bits", Nil)
    val clock = FAMEChannelConnectionAnnotation("clock", TargetClockChannel(Seq(RationalClock("base", 1, 1)), Seq(1)),
      None, None, Some(Seq(ref)))
    val bridge = BridgeIOAnnotation(ReferenceTarget("Fixture", "Target", Nil, "clockbridge", Nil),
      Map.empty[String, String], None, classOf[CaptureTopFixtureClock].getName)
    val toCPU = new StreamAllocator; val fromCPU = new StreamAllocator
    implicit val p: Parameters = (new midas.F2Config).toInstance.alterPartial {
      case SimWrapperKey => SimWrapperConfig(Seq(clock, bridge), Map.empty)
      case CheckpointCaptureTopKey => Some(new Binding)
      case ToCPUStreamAllocatorKey => toCPU
      case FromCPUStreamAllocatorKey => fromCPU
    }
    val top = LazyModule(new FPGATop)
    val elaboration = out.resolve("elaboration"); Files.createDirectories(elaboration)
    val emitted = (new ChiselStage).execute(Array("-X", "low", "--target-dir", elaboration.toString),
      Seq(chisel3.stage.ChiselGeneratorAnnotation(() => top.module)))
    val circuit = emitted.collectFirst { case firrtl.stage.FirrtlCircuitAnnotation(c) => c }.get
    val annotations = JsonProtocol.deserialize(new String(Files.readAllBytes(elaboration.resolve("FPGATop.anno.json")), UTF_8))
    val header = new StringBuilder; top.genHeader(header)
    val native = NativeCheckpointInPlaceFixture.reference
    val storage = native.circuit.modules.find(_.name == "NativeStorage").get.asInstanceOf[FirrtlModule]
    val other = storage.copy(name = "OtherStorage")
    val oldWrapper = circuit.modules.find(_.name == "SimWrapper").get.asInstanceOf[FirrtlModule]
    def autonomous(instance: String, module: String, xor: Int): String = s"""    inst $instance of $module
    $instance.clock <= clock
    $instance.reset <= reset
    $instance.enable <= UInt<2>(3)
    $instance.address <= bits($instance.cycle, 2, 0)
    $instance.data <= pad(xor($instance.cycle, UInt<32>($xor)), 130)
    $instance.write <= UInt<1>(1)
"""
    val wrapper = Parser.parse("circuit SimWrapper :\n  module SimWrapper :\n" +
      oldWrapper.ports.map(p => "    " + p.serialize).mkString("\n") + "\n" +
      "    channelPorts_clock_ready <= UInt<1>(1)\n" + autonomous("network", "NativeStorage", 0))
      .modules.head.asInstanceOf[FirrtlModule].copy(ports = oldWrapper.ports)
    val topModule = circuit.modules.find(_.name == "FPGATop").get.asInstanceOf[FirrtlModule]
    val peerStatements = Parser.parse("circuit Peer :\n  module Peer :\n    input clock : Clock\n    input reset : UInt<1>\n" +
      autonomous("peer", "OtherStorage", 4660)).modules.head.asInstanceOf[FirrtlModule].body
    val modules = circuit.modules.map {
      case m: FirrtlModule if m.name == "SimWrapper" => wrapper
      case m: FirrtlModule if m.name == "FPGATop" => m.copy(body = Block(Seq(m.body, peerStatements)))
      case m => m
    } ++ native.circuit.modules :+ other
    val shell = FirrtlModule(NoInfo, "ReservationShell", topModule.ports,
      Block(Seq(WDefInstance(NoInfo, "top", "FPGATop", firrtl.Utils.module_type(topModule))) ++ topModule.ports.map { p =>
        val pin = WSubField(WRef("top"), p.name, p.tpe); val port = WRef(p.name, p.tpe)
        if (p.direction == FirrtlInput) Connect(NoInfo, pin, port) else Connect(NoInfo, port, pin)
      }))
    val renames = firrtl.RenameMap(Map(CircuitName(circuit.main) -> Seq(CircuitName("ReservationShell"))))
    val retained = annotations.filterNot(a => a.isInstanceOf[TargetBoxAnnotation] ||
      a.isInstanceOf[firrtl.stage.RunFirrtlTransformAnnotation] || a == firrtl.transforms.NoCircuitDedupAnnotation)
    val state = CircuitState(resolved(circuit.copy(main = "ReservationShell", modules = modules :+ shell)), LowForm,
      (retained :+ GoldenGateOutputFileAnnotation(header.toString, ".const.h")).flatMap(_.update(renames)) ++
        Seq(firrtl.transforms.NoCircuitDedupAnnotation,
          firrtl.stage.RunFirrtlTransformAnnotation(new firrtl.passes.InlineInstances)))
    NativeCheckpointWidgetReservation.inspect(state)
    state
  }
  private lazy val after = ResolveSimulationInline(before)
  private lazy val targets = {
    val top = ModuleTarget("ReservationShell", "ReservationShell").instOf("top", "FPGATop")
    Seq(top.instOf("sim", "SimWrapper").instOf("network", "NativeStorage"), top.instOf("peer", "OtherStorage"))
  }
  private lazy val inventory = ReservedCheckpointInventory.forReservedWidget(before, after)
  private lazy val source = NativeCheckpointRegions.generate(after.copy(
    annotations = Seq(firrtl.transforms.NoCircuitDedupAnnotation)), targets, Some(inventory))
  private lazy val connected = NativeCheckpointRegionWidgetGraft.connect(before, after, source)

  behavior.of("Existing widget connected to disjoint native regions")
  it should "route twenty signals across the common root and preserve admission guards and original execution" in {
    val result = connected
    assert(source.descriptors.entries.size == 14 && source.descriptors.physicalClockGates == 4)
    val doc = parse(result.binding)
    assert((doc \ "pins").asInstanceOf[JArray].arr.size == 20)
    assert((doc \ "routes").asInstanceOf[JArray].arr.size == 2)
    Seq("descriptor_identity_bound", "production_freeze_owners_bound", "complete_fpga_capture",
      "deployable_capture_image", "metasim_restore_supported").foreach(k => assert((doc \ k) == JBool(false)))
    assert(result.state.circuit.modules.find(_.name == "ReservationShell").get.ports ==
      after.circuit.modules.find(_.name == "ReservationShell").get.ports)
    val live = GeneratedExternalOwners.instances(result.state, result.state.circuit.main)
    val oldLive = GeneratedExternalOwners.instances(after, after.circuit.main)
    assert(oldLive.forall { case (p, _) => live.exists(_._1 == p) })
    assert(live.count { case (_, m: ExtModule) => m.defname == "CheckpointPhysicalCaptureTransport"; case _ => false } == 1)
    Seq(result.state, result.state.copy(annotations = Nil)).foreach { s =>
      intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(s))
      intercept[IllegalArgumentException](PhysicalClockGateContract.requireCaptureOwner(s))
    }
    Seq("before-reserved" -> before, "native-inlined" -> after, "connected" -> result.state,
      "host" -> source.host, "instrumented" -> source.state, "grafted" -> result.graft.state).foreach { case (n, s) =>
      save(n + ".fir", s.circuit.serialize); save(n + "-annotations.json", JsonProtocol.serialize(s.annotations))
    }
    save("connection.json", result.binding); save("binding.json", source.binding)
    save("annotation-projection.json", result.annotationProjection)
    save("reserved-inventory.json", inventory.binding)
    save("manifest.json", source.descriptors.manifest); save("descriptors.json", source.descriptors.manifest)
    save("descriptor-binding.json", source.descriptors.binding); save("routing.json", source.routing.binding)
    save("peer-routing.json", NativeCheckpointWidgetPeerRouting.inspect(before, after))
    source.regions.zipWithIndex.foreach { case (r, i) =>
      save(s"region-$i.fir", r.state.circuit.serialize); save(s"region-$i-binding.json", r.native.binding)
    }
    result.graft.files.foreach { f => save("graft" + f.fileSuffix, f.body); save("FireSim-generated" + f.fileSuffix, f.body) }
    before.annotations.collect { case a: GoldenGateOutputFileAnnotation if a.fileSuffix == ".const.h" => a }
      .foreach(a => save("reserved.h", a.body))
    before.annotations.collect { case a: firrtl.transforms.BlackBoxInlineAnno
      if Set("CheckpointPhysicalCaptureTransport.sv", "CheckpointPhysicalClockCut.sv", "CheckpointCaptureTransport.sv", "CheckpointCaptureCut.sv")(a.name) => a }
      .foreach(a => save(a.name, a.text))
    val emitter = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
    save("connected.sv", emitter.execute(result.state.copy(annotations = result.state.annotations.filterNot(
      _.isInstanceOf[firrtl.EmitCircuitAnnotation]) :+ firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter]))).getEmittedCircuit.value)
    Seq("instrumented" -> source.state, "grafted" -> result.graft.state).foreach { case (n, s) =>
      val xdc = midas.passes.WriteXDCFile.execute(s.copy(annotations = s.annotations :+
        midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, Some("shell/CL"))))
      xdc.annotations.collect { case a: midas.passes.XDCOutputAnnotation => a }
        .foreach(a => save(n + a.suffix.get, a.fileBody))
    }
    println(s"NATIVE_REGION_WIDGET out=$out entries=14 gates=4 routes=20 guards_retained=true")
  }
  it should "bind the combined identity to actual transport and host while keeping all image and restore guards" in {
    val bound = NativeCheckpointWidgetIdentity.bindRegions(before, after, source)
    assert(bound.connection.state.circuit.serialize == connected.state.circuit.serialize)
    assert(JsonProtocol.serialize(bound.connection.state.annotations) == JsonProtocol.serialize(connected.state.annotations))
    assert(bound.connection.binding == connected.binding)
    val doc = parse(bound.binding)
    assert((doc \ "descriptor_identity_bound") == JBool(true))
    assert((doc \ "descriptor_manifest_sha256") == JString(source.descriptors.manifestSha256))
    Seq("production_freeze_owners_bound", "complete_fpga_capture", "deployable_capture_image",
      "metasim_restore_supported", "production_annotations_translated").foreach(k => assert((doc \ k) == JBool(false)))
    val oldModules = connected.state.circuit.modules.map(m => m.name -> m).toMap
    val changed = bound.state.circuit.modules.filter(m => oldModules(m.name) != m)
    assert(changed.size == 1)
    val transport = changed.head.asInstanceOf[ExtModule]
    assert(transport.params.collect { case IntParam("MANIFEST_SHA256", n) => n } ==
      Seq(BigInt(source.descriptors.manifestSha256, 16)))
    assert(transport.copy(params = oldModules(transport.name).asInstanceOf[ExtModule].params) == oldModules(transport.name))
    val header = bound.state.annotations.collect {
      case a: GoldenGateOutputFileAnnotation if a.fileSuffix == ".const.h" => a.body
    }.head
    assert(header.contains(NativeCheckpointWidgetIdentity.incomplete) &&
      !header.contains("Unbound_checkpoint_widget_reservation"))
    source.descriptors.entries.foreach(e => assert(header.contains(e.stateID)))
    Seq(bound.state, bound.state.copy(annotations = Nil)).foreach { state =>
      intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(state))
      intercept[IllegalArgumentException](PhysicalClockGateContract.requireCaptureOwner(state))
    }
    // Keep the full unbound compiler output independently auditable. Only the
    // executable manifest parameter, registered header and its annotation change.
    val files = Files.list(out)
    try {
      val iterator = files.iterator()
      while (iterator.hasNext) {
        val file = iterator.next()
        if (Files.isRegularFile(file)) {
          save("identity/" + file.getFileName, new String(Files.readAllBytes(file), UTF_8))
          save("identity/unbound/" + file.getFileName, new String(Files.readAllBytes(file), UTF_8))
        }
      }
    } finally files.close()
    save("identity/connected.fir", bound.state.circuit.serialize)
    save("identity/connected-annotations.json", JsonProtocol.serialize(bound.state.annotations))
    save("identity/identity.json", bound.binding)
    save("identity/reserved.h", header)
    val emitter = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
    save("identity/connected.sv", emitter.execute(bound.state.copy(annotations = bound.state.annotations.filterNot(
      _.isInstanceOf[firrtl.EmitCircuitAnnotation]) :+ firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter]))).getEmittedCircuit.value)
    println(s"NATIVE_REGION_WIDGET_IDENTITY out=$out/identity entries=14 guards_retained=true")
  }
  it should "reconstruct regional descriptor identities instead of trusting rehashed sidecars" in {
    intercept[IllegalArgumentException](NativeCheckpointWidgetIdentity.bindRegions(before, after,
      source.copy(descriptors = NativeCheckpointInPlaceAccess.descriptors(source.regions.head))))
    intercept[IllegalArgumentException](NativeCheckpointWidgetIdentity.bindRegions(before, after,
      source.copy(descriptors = CheckpointCaptureDescriptors.fromNativeRegions(source.regions.reverse.map(NativeCheckpointInPlaceAccess.descriptors))._1)))
    intercept[IllegalArgumentException](NativeCheckpointWidgetIdentity.bindRegions(before, after,
      source.copy(targets = source.targets.reverse)))
    val header = after.annotations.collectFirst {
      case a: GoldenGateOutputFileAnnotation if a.fileSuffix == ".const.h" => a
    }.get
    intercept[IllegalArgumentException](NativeCheckpointWidgetIdentity.bindRegions(before,
      after.copy(annotations = after.annotations :+ header), source))
  }
  it should "reject changed host, source policy, metadata, reservation counts and reconstructed region ownership" in {
    intercept[IllegalArgumentException](NativeCheckpointRegionWidgetGraft.connect(before,
      after.copy(circuit = after.circuit.copy(info = FileInfo(StringLit("changed")))), source))
    intercept[IllegalArgumentException](NativeCheckpointRegionWidgetGraft.connect(before, after,
      source.copy(host = source.host.copy(circuit = source.host.circuit.copy(info = FileInfo(StringLit("changed")))))))
    intercept[IllegalArgumentException](NativeCheckpointRegionWidgetGraft.connect(before, after,
      source.copy(host = source.host.copy(annotations = Nil))))
    intercept[IllegalArgumentException](NativeCheckpointRegionWidgetGraft.connect(before, after,
      source.copy(routing = source.routing.copy(regions = source.routing.regions.reverse))))
    val badBefore = before.copy(annotations = before.annotations.map {
      case a: NativeCheckpointWidgetExecutionAnnotation => a.copy(entries = a.entries + 1)
      case a => a
    }, circuit = before.circuit.copy(modules = before.circuit.modules.map {
      case m: ExtModule if m.defname == "CheckpointPhysicalCaptureTransport" => m.copy(params = m.params.map {
        case IntParam("ENTRIES", n) => IntParam("ENTRIES", n + 1); case p => p
      })
      case m => m
    }))
    val badAfter = ResolveSimulationInline(badBefore)
    val badSource = NativeCheckpointRegions.generate(badAfter.copy(annotations = Seq(firrtl.transforms.NoCircuitDedupAnnotation)),
      targets, Some(ReservedCheckpointInventory.forReservedWidget(badBefore, badAfter)))
    intercept[IllegalArgumentException](NativeCheckpointRegionWidgetGraft.connect(badBefore, badAfter, badSource))
    val metadataBefore = before.copy(annotations = before.annotations :+
      UnsupportedRegionWidgetOwner(ModuleTarget("ReservationShell", "FPGATop").ref("reservedCycle")))
    val metadataAfter = ResolveSimulationInline(metadataBefore)
    val metadataSource = NativeCheckpointRegions.generate(metadataAfter.copy(
      annotations = Seq(firrtl.transforms.NoCircuitDedupAnnotation)), targets,
      Some(ReservedCheckpointInventory.forReservedWidget(metadataBefore, metadataAfter)))
    val error = intercept[IllegalArgumentException](NativeCheckpointRegionWidgetGraft.connect(
      metadataBefore, metadataAfter, metadataSource))
    assert(error.getMessage.contains("unknown host annotation targets changed definition"))
  }
  it should "keep strict default inventory and reject stale permission, changed ABI and changed transport sources" in {
    intercept[IllegalArgumentException](SimulatorStateInventory.discover(source.host))
    intercept[IllegalArgumentException](inventory.analysisFor(source.host.copy(annotations = Nil)))
    intercept[IllegalArgumentException](inventory.analysisFor(source.host.copy(circuit =
      source.host.circuit.copy(info = FileInfo(StringLit("changed"))))))
    intercept[IllegalArgumentException](inventory.validate(before, after.copy(annotations = Nil)))
    Seq(false, true).foreach { sourceChange =>
      val changed = if (sourceChange) before.copy(annotations = before.annotations.map {
        case a: firrtl.transforms.BlackBoxInlineAnno if a.name == "CheckpointPhysicalCaptureTransport.sv" => a.copy(text = a.text + "\n")
        case a => a
      }) else before.copy(circuit = before.circuit.copy(modules = before.circuit.modules.map {
        case m: ExtModule if m.defname == "CheckpointPhysicalCaptureTransport" => m.copy(ports = m.ports.filterNot(_.name == "dma_abort"))
        case m => m
      }))
      intercept[IllegalArgumentException](ReservedCheckpointInventory.forReservedWidget(changed, ResolveSimulationInline(changed)))
    }
    assert((parse(source.descriptors.manifest) \ "unsupported_external_owners").asInstanceOf[JArray].arr.nonEmpty)
    assert(source.state.circuit.modules.exists { case m: ExtModule => m.defname == "CheckpointPhysicalCaptureTransport"; case _ => false })
  }
}
