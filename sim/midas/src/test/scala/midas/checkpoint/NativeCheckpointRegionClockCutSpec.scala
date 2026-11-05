// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{CircuitState, WDefInstance}
import firrtl.annotations.ModuleTarget
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class NativeCheckpointRegionClockCutSpec extends AnyFlatSpec {
  import NativeCheckpointRegionsFixture._
  private lazy val regions = NativeCheckpointRegions.generate(source, targets)
  private val owners = Seq("fixture.external_writers")
  private lazy val cut = NativeCheckpointRegionClockCut.generate(regions, owners)
  private def all(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  behavior.of("Generated common low-phase cut on existing native regions")

  it should "connect one raw-clock controller and preserve all original execution and readout" in {
    val root = cut.state.circuit.modules.find(_.name == "NativeHost").get.asInstanceOf[Module]
    val statements = all(root.body)
    val controller = cut.state.circuit.modules.collect { case e: ExtModule if e.defname == "CheckpointPhysicalClockCut" => e }
    assert(controller.size == 1)
    assert(controller.head.params.toSet == Set(IntParam("GATES", 4), IntParam("OWNERS", 1),
      IntParam("EXTERNAL_CLOCK_NETWORK", 1), IntParam("RESTORE_ENABLE", 0)))
    assert(statements.collect { case i: WDefInstance if i.module == controller.head.name => i.name } ==
      Seq(NativeCheckpointRegionClockCut.instance))
    val connections = statements.collect { case c: Connect => c.loc.serialize -> c.expr.serialize }.toMap
    assert(connections("checkpoint_region_clock_cut.clock") == "clock")
    assert(connections("checkpoint_region_clock_cut.ce") == "capture_logical_ce")
    assert(connections("checkpoint_region_clock_cut.freeze_request") ==
      "or(capture_cut_freeze_request, capture_cut_accepted_pending)")
    Seq("capture_clock_mask", "capture_read_enable", "capture_captured_ce").foreach(n =>
      assert(root.ports.find(_.name == n).get.direction == Output))
    val reg = statements.collectFirst { case r: DefRegister if r.name == "capture_cut_accepted_pending" => r }.get
    assert(reg.clock.serialize == "clock" && reg.reset == UIntLiteral(0, IntWidth(1)))
    val before = GeneratedExternalOwners.instances(regions.state, regions.state.circuit.main).toMap
    val after = GeneratedExternalOwners.instances(cut.state, cut.state.circuit.main).toMap
    before.foreach { case (p, m) => assert(after.get(p).exists(_.name == m.name)) }
    assert(after("NativeHost.stage") == before("NativeHost.stage"))
    NativeCheckpointRegionClockCut.validate(regions, owners, cut)
  }

  it should "gate both outer handshakes and data including invalid selector responses" in {
    val root = cut.state.circuit.modules.find(_.name == "NativeHost").get.asInstanceOf[Module]
    val connections = all(root.body).collect { case c: Connect => c.loc.serialize -> c.expr.serialize }.toMap
    assert(connections("capture_cut_raw_request_valid") ==
      "and(and(capture_request_valid, capture_read_enable), capture_cut_freeze_request)")
    assert(connections("capture_request_ready") ==
      "and(and(capture_cut_raw_request_ready, capture_read_enable), capture_cut_freeze_request)")
    assert(connections("capture_response_valid") == "and(capture_cut_raw_response_valid, capture_read_enable)")
    assert(connections("capture_cut_raw_response_ready") == "and(capture_response_ready, capture_read_enable)")
    assert(connections("capture_response_data").startsWith("mux(capture_read_enable, capture_cut_raw_response_data,"))
    assert(connections("capture_response_error") == "and(capture_cut_raw_response_error, capture_read_enable)")
    assert(connections("capture_cut_accepted_pending").contains("capture_cut_raw_response_valid"))
    assert(connections("capture_cut_accepted_pending").startsWith("mux(capture_reset,"))
    assert(!connections("capture_cut_accepted_pending").contains("not(capture_cut_raw_request_ready)"))
  }

  it should "retain owner ordering coverage deficits and every incomplete-image guard" in {
    val binding = parse(cut.binding)
    assert((binding \ "owners") == JArray(owners.map(JString(_)).toList))
    assert((binding \ "physical_clock_gates") == JInt(4))
    assert((binding \ "control_state_bits") == JInt(75))
    assert((binding \ "uncovered_functional_state") == (parse(regions.descriptors.manifest) \ "uncovered_functional_state"))
    assert((binding \ "uncovered_physical_gates") == (parse(regions.descriptors.manifest) \ "uncovered_physical_gates"))
    Seq("target_time_authority_proven", "external_writer_owners_bound", "reset_writer_owners_bound",
      "production_freeze_owners_bound", "controller_state_atomically_owned", "complete_fpga_capture",
      "deployable_capture_image", "metasim_restore_supported").foreach(k => assert((binding \ k) == JBool(false)))
    val two = NativeCheckpointRegionClockCut.generate(regions, Seq("fixture.fased", "fixture.axi_writers"))
    assert((parse(two.binding) \ "owners") == JArray(List(JString("fixture.fased"), JString("fixture.axi_writers"))))
    assert(two.state.circuit.modules.collectFirst { case e: ExtModule if e.defname == "CheckpointPhysicalClockCut" =>
      e.ports.find(_.name == "owner_present").get.tpe }.get == UIntType(IntWidth(2)))
  }

  it should "restore the complete two-edge controller and accepted-request owner only under explicit held opt-in" in {
    val restored = NativeCheckpointRegionClockCut.generate(regions, owners, restoreController = true)
    NativeCheckpointRegionClockCut.validate(regions, owners, restored)
    val root = restored.state.circuit.modules.find(_.name == "NativeHost").get.asInstanceOf[Module]
    val connections = all(root.body).collect { case c: Connect => c.loc.serialize -> c.expr.serialize }.toMap
    assert(root.ports.find(_.name == "capture_cut_restore_state").get.tpe == UIntType(IntWidth(75)))
    assert(connections("checkpoint_region_clock_cut.restore_hold") ==
      "or(capture_cut_restore_hold, or(capture_cut_restore_load_pos, capture_cut_restore_load_neg))")
    assert(connections("checkpoint_region_clock_cut.restore_load_pos") ==
      "and(capture_cut_restore_hold, capture_cut_restore_load_pos)")
    assert(connections("checkpoint_region_clock_cut.restore_load_neg") ==
      "and(capture_cut_restore_hold, capture_cut_restore_load_neg)")
    assert(connections("checkpoint_region_clock_cut.restore_state") == "bits(capture_cut_restore_state, 73, 0)")
    assert(connections("capture_cut_accepted_pending").contains("bits(capture_cut_restore_state, 74, 74)"))
    assert(connections("capture_cut_accepted_pending").startsWith("mux(capture_reset, UInt<1>(\"h0\"), mux(or("))
    assert((parse(restored.binding) \ "controller_restore_extra_storage_bits") == JInt(0))
    Seq("controller_restore_resume_supported", "router_endpoint_functional_restore_supported",
      "complete_fpga_capture", "deployable_capture_image").foreach(k => assert((parse(restored.binding) \ k) == JBool(false)))
    assert(!cut.state.circuit.modules.find(_.name == "NativeHost").get.asInstanceOf[Module]
      .ports.exists(_.name.startsWith("capture_cut_restore_")))
    intercept[IllegalArgumentException](NativeCheckpointRegionClockCut.validate(regions, owners,
      restored.copy(restoreController = false)))
    intercept[IllegalArgumentException](NativeCheckpointRegionClockCut.validate(regions, owners,
      restored.copy(state = cut.state)))
  }

  it should "reject changed circuitry rehashed bindings owner order and source resources" in {
    val changed = cut.state.copy(circuit = cut.state.circuit.copy(info = FileInfo(StringLit("changed"))))
    val coherent = SemanticStateInventory.canonical(parse(cut.binding) merge JObject(List(
      "attached_sha256" -> JString(SemanticStateInventory.sha256(changed.circuit.serialize)))))
    intercept[IllegalArgumentException](NativeCheckpointRegionClockCut.validate(regions, owners,
      cut.copy(state = changed, binding = coherent)))
    intercept[IllegalArgumentException](NativeCheckpointRegionClockCut.validate(regions, Seq("fixture.other_owner"), cut))
    intercept[IllegalArgumentException](NativeCheckpointRegionClockCut.validate(regions, owners,
      cut.copy(files = cut.files.map(f => f.copy(body = f.body + "\n")))))
    Seq(Seq.empty[String], Seq("duplicate", "duplicate"), Seq("bad owner"), Seq("")).foreach(bad =>
      intercept[IllegalArgumentException](NativeCheckpointRegionClockCut.generate(regions, bad)))
  }

  it should "reject native shell ownership without an atomic staging contract and reserved identifiers" in {
    val mixed = NativeCheckpointRegions.generate(source, targets, shellTargets = Seq(ModuleTarget("NativeHost", "NativeHost")))
    assert(intercept[IllegalArgumentException](NativeCheckpointRegionClockCut.generate(mixed, owners)).getMessage
      .contains("without shell/borrowed-reader owners"))
    val h = source
    val collision = h.copy(circuit = h.circuit.copy(modules = h.circuit.modules.map {
      case m: Module if m.name == h.circuit.main => m.copy(ports = m.ports :+
        Port(NoInfo, "capture_cut_collision", Input, UIntType(IntWidth(1))))
      case m => m
    }))
    val r = NativeCheckpointRegions.generate(collision, targets)
    assert(intercept[IllegalArgumentException](NativeCheckpointRegionClockCut.generate(r, owners)).getMessage
      .contains("identifier collision"))
  }

  it should "emit the actual generated attachment beside the retained complete regional audit fixture" in {
    save(regions, "native-regions-common-cut")
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("native-regions-common-cut")
    def write(n: String, text: String): Unit = Files.write(out.resolve(n), text.getBytes(UTF_8))
    write("attached.fir", cut.state.circuit.serialize)
    write("attached-annotations.json", firrtl.annotations.JsonProtocol.serialize(cut.state.annotations))
    write("cut-binding.json", cut.binding)
    write("graft-binding.json", cut.graft.binding)
    cut.files.filterNot(f => cut.graft.files.contains(f)).foreach(f => write("FireSim-generated" + f.fileSuffix, f.body))
    val placement = midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, Some("shell/CL"))
    val constrained = midas.passes.WriteXDCFile.execute(cut.state.copy(annotations = cut.state.annotations :+ placement))
    val emitter = new SemanticCleanEmitter
    val emitted = new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(constrained.copy(annotations = constrained.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
      .getEmittedCircuit.value
    write("attached.sv", emitted)
    assert(emitted.contains("CheckpointPhysicalClockCut #("))
    assert(emitted.contains("capture_cut_accepted_pending"))
  }

  it should "emit the complete held-controller attachment without changing original execution storage" in {
    val restored = NativeCheckpointRegionClockCut.generate(regions, owners, restoreController = true)
    save(regions, "native-regions-controller-restore")
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("native-regions-controller-restore")
    def write(n: String, text: String): Unit = Files.write(out.resolve(n), text.getBytes(UTF_8))
    write("attached.fir", restored.state.circuit.serialize)
    write("cut-binding.json", restored.binding)
    restored.files.filterNot(f => restored.graft.files.contains(f))
      .foreach(f => write("FireSim-generated" + f.fileSuffix, f.body))
    // Include the actual native graft resources and exact emitted clock constraints.
    restored.graft.files.foreach(f => write("FireSim-generated" + f.fileSuffix, f.body))
    val placement = midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, Some("shell/CL"))
    val constrained = midas.passes.WriteXDCFile.execute(restored.state.copy(annotations = restored.state.annotations :+ placement))
    val emitter = new SemanticCleanEmitter
    val emitted = new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(constrained.copy(annotations = constrained.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
      .getEmittedCircuit.value
    write("attached.sv", emitted)
    assert(emitted.contains(".RESTORE_ENABLE(1)"))
  }
}
