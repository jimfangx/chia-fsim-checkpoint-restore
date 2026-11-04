// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{WDefInstance}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class NativeCheckpointRegionCutSnapshotSpec extends AnyFlatSpec {
  import NativeCheckpointRegionsFixture._
  private lazy val regions = NativeCheckpointRegions.generate(source, targets)
  private val owners = Seq("fixture.external_writers")
  private lazy val snapshot = NativeCheckpointRegionCutSnapshot.generate(regions, owners)
  private def all(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  behavior.of("Compiler-attached atomic regional control observation")

  it should "preserve every original execution and raw-clock pending owner" in {
    val before = GeneratedExternalOwners.instances(snapshot.cut.state, snapshot.cut.state.circuit.main).toMap
    val after = GeneratedExternalOwners.instances(snapshot.state, snapshot.state.circuit.main).toMap
    before.foreach { case (p, m) => assert(after.get(p).exists(_.name == m.name)) }
    val root = snapshot.state.circuit.modules.find(_.name == "NativeHost").get.asInstanceOf[Module]
    val s = all(root.body)
    assert(s.collect { case i: WDefInstance if i.module == "CheckpointRegionCutSnapshot" => i.name } ==
      Seq("checkpoint_region_cut_snapshot"))
    assert(s.collectFirst { case r: DefRegister if r.name == "capture_cut_accepted_pending" =>
      r.clock.serialize }.contains("clock"))
    val c = s.collect { case x: Connect => x.loc.serialize -> x.expr.serialize }.toMap
    assert(c("checkpoint_region_cut_snapshot.clock") == "clock")
    assert(c("checkpoint_region_cut_snapshot.reset") == "capture_reset")
    assert(c("checkpoint_region_cut_snapshot.live_control_state") == "capture_cut_control_state")
    assert(c("checkpoint_region_cut_snapshot.source_pending") == "capture_cut_pending")
    assert(c("capture_cut_accepted_pending").contains("capture_snapshot_source_request_valid"))
    assert(c("checkpoint_region_clock_cut.freeze_request") ==
      "or(capture_snapshot_cut_freeze_request, capture_cut_accepted_pending)")
    NativeCheckpointRegionCutSnapshot.validate(regions, owners, snapshot)
  }

  it should "interpose all bank handshake directions without masking original readout clocks" in {
    val root = snapshot.state.circuit.modules.find(_.name == "NativeHost").get.asInstanceOf[Module]
    val c = all(root.body).collect { case x: Connect => x.loc.serialize -> x.expr.serialize }.toMap
    assert(c("checkpoint_region_cut_snapshot.request_valid") == "capture_request_valid")
    assert(c("checkpoint_region_cut_snapshot.response_ready") == "capture_response_ready")
    assert(c("capture_request_ready") == "checkpoint_region_cut_snapshot.request_ready")
    assert(c("capture_response_valid") == "checkpoint_region_cut_snapshot.response_valid")
    assert(c("capture_response_data") == "checkpoint_region_cut_snapshot.response_data")
    assert(c("capture_response_error") == "checkpoint_region_cut_snapshot.response_error")
    assert(c("checkpoint_region_cut_snapshot.source_request_ready") == "capture_snapshot_source_request_ready")
    assert(c("capture_snapshot_source_request_valid") == "checkpoint_region_cut_snapshot.source_request_valid")
    assert(c("capture_read_enable") == "checkpoint_region_clock_cut.read_enable")
    assert(root.ports.count(_.name.startsWith("capture_snapshot_")) == 12)
  }

  it should "expose separate owner widths without admitting a complete checkpoint" in {
    val b = parse(snapshot.binding)
    assert((b \ "control_state_bits") == JInt(75))
    assert((b \ "staging_owner_bits") == JInt(146))
    assert((b \ "controller_observation_atomically_staged") == JBool(true))
    Seq("readout_router_state_atomically_owned", "staging_owner_restore_supported", "complete_fpga_capture",
      "deployable_capture_image", "metasim_restore_supported").foreach(k => assert((b \ k) == JBool(false)))
    assert((parse(snapshot.cut.binding) \ "controller_state_atomically_owned") == JBool(false))
  }

  it should "reject rehashed circuit changes changed resources owners and reserved identifiers" in {
    val changed = snapshot.state.copy(circuit = snapshot.state.circuit.copy(info = FileInfo(StringLit("changed"))))
    val coherent = SemanticStateInventory.canonical(parse(snapshot.binding) merge JObject(List(
      "attached_sha256" -> JString(SemanticStateInventory.sha256(changed.circuit.serialize)))))
    intercept[IllegalArgumentException](NativeCheckpointRegionCutSnapshot.validate(regions, owners,
      snapshot.copy(state = changed, binding = coherent)))
    intercept[IllegalArgumentException](NativeCheckpointRegionCutSnapshot.validate(regions, owners,
      snapshot.copy(files = snapshot.files.map(f => f.copy(body = f.body + "\n")))))
    intercept[IllegalArgumentException](NativeCheckpointRegionCutSnapshot.validate(regions, Seq("other.owner"), snapshot))
    val collision = source.copy(circuit = source.circuit.copy(modules = source.circuit.modules.map {
      case m: Module if m.name == source.circuit.main => m.copy(ports = m.ports :+
        Port(NoInfo, "capture_snapshot_request", Input, UIntType(IntWidth(1))))
      case m => m
    }))
    val r = NativeCheckpointRegions.generate(collision, targets)
    assert(intercept[IllegalArgumentException](NativeCheckpointRegionCutSnapshot.generate(r, owners)).getMessage
      .contains("identifier collision"))
  }

  private def saveSnapshot(snapshot: NativeCheckpointRegionCutSnapshot.Result, name: String): Unit = {
    save(regions, name)
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve(name)
    def write(n: String, text: String): Unit = Files.write(out.resolve(n), text.getBytes(UTF_8))
    def emit(state: firrtl.CircuitState): String = {
      val placement = midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, Some("shell/CL"))
      val constrained = midas.passes.WriteXDCFile.execute(state.copy(annotations = state.annotations :+ placement))
      val emitter = new SemanticCleanEmitter
      new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
        .execute(constrained.copy(annotations = constrained.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
        .getEmittedCircuit.value
    }
    val cut = snapshot.cut
    write("grafted.fir", cut.graft.state.circuit.serialize)
    write("grafted-annotations.json", firrtl.annotations.JsonProtocol.serialize(cut.graft.state.annotations))
    write("grafted.sv", emit(cut.graft.state))
    cut.graft.files.foreach(f => write("FireSim-generated" + f.fileSuffix, f.body))
    write("attached.fir", cut.state.circuit.serialize)
    write("attached-annotations.json", firrtl.annotations.JsonProtocol.serialize(cut.state.annotations))
    write("cut-binding.json", cut.binding)
    write("graft-binding.json", cut.graft.binding)
    write("attached.sv", emit(cut.state))
    write("staged.fir", snapshot.state.circuit.serialize)
    write("staged-annotations.json", firrtl.annotations.JsonProtocol.serialize(snapshot.state.annotations))
    write("snapshot-binding.json", snapshot.binding)
    write("staged.sv", emit(snapshot.state))
    snapshot.files.filterNot(f => cut.graft.files.contains(f)).foreach(f => write("FireSim-generated" + f.fileSuffix, f.body))
    assert(Files.readString(out.resolve("staged.sv")).contains("CheckpointRegionCutSnapshot #("))
  }

  it should "emit the source-bound staged circuit beside its independently auditable original cut" in {
    saveSnapshot(snapshot, "native-regions-cut-snapshot")
  }

  it should "stage every shared and physical router bit at the same preedge without changing defaults" in {
    val result = NativeCheckpointRegionCutSnapshot.generate(regions, owners, includeRouterState = true)
    NativeCheckpointRegionCutSnapshot.validate(regions, owners, result)
    NativeCheckpointRegionClockCut.validate(regions, owners, result.cut)
    val b = parse(result.binding)
    assert((b \ "snapshot_version") == JInt(2))
    assert((b \ "controller_state_bits") == JInt(75))
    assert((b \ "router_state_bits") == JInt(7))
    assert((b \ "control_state_bits") == JInt(82))
    assert((b \ "staging_owner_bits") == JInt(153))
    assert((b \ "router_observation_atomically_staged") == JBool(true))
    val rows = (b \ "router_state_observation" \ "owners").asInstanceOf[JArray].arr
    assert(rows.map(_ \ "offset") == List(JInt(0), JInt(3), JInt(5)))
    assert(rows.map(_ \ "bits") == List(JInt(3), JInt(2), JInt(2)))
    assert(rows.map(_ \ "instance") == List(JString("checkpoint_region_readout_adapter.router"),
      JString("checkpoint_region_readout_adapter.region0.router"), JString("checkpoint_region_readout_adapter.region1.router")))
    val root = result.state.circuit.modules.find(_.name == "NativeHost").get.asInstanceOf[Module]
    val c = all(root.body).collect { case x: Connect => x.loc.serialize -> x.expr.serialize }.toMap
    assert(c("checkpoint_region_cut_snapshot.live_control_state") == "cat(capture_router_state, capture_cut_control_state)")
    assert(result.cut.graft.files.find(_.fileSuffix == ".native-region-graft.sv").get.body.contains(
      ".capture_state(capture_router_state[0 +: 3])"))
    (0 until 2).foreach { i =>
      val body = result.cut.graft.files.find(_.fileSuffix == s".native-region-graft-$i.sv").get.body
      assert(body.contains(".capture_state(capture_router_state)"))
      assert(body.contains(".restore_hold(1'b0),.restore_load(1'b0),.restore_state(2'b0)"))
    }
    assert((parse(snapshot.cut.graft.binding) \ "router_state_observation") == JNothing)
    intercept[IllegalArgumentException](NativeCheckpointRegionCutSnapshot.validate(regions, owners,
      result.copy(includeRouterState = false)))
    Seq("readout_router_state_atomically_owned", "staging_owner_restore_supported", "complete_fpga_capture",
      "deployable_capture_image", "metasim_restore_supported").foreach(k => assert((b \ k) == JBool(false)))
    saveSnapshot(result, "native-regions-cut-router-snapshot")
  }
}
