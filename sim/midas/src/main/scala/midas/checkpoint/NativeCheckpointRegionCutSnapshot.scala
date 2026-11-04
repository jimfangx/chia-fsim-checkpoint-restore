// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, WDefInstance, WRef, WSubField}
import firrtl.ir._
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Atomically observe a generated regional cut before borrowing its readout.
  * This owns the controller/pending observation only. The readout/router,
  * staging owner, external writers and global target time still need complete
  * capture/restore bindings before any production image can be admitted.
  */
object NativeCheckpointRegionCutSnapshot {
  import SemanticStateInventory.{canonical, sha256}
  val prefix = "capture_snapshot_"
  val instance = "checkpoint_region_cut_snapshot"
  val definition = "CheckpointRegionCutSnapshot"
  case class Result(state: CircuitState, cut: NativeCheckpointRegionClockCut.Result,
      files: Seq[GoldenGateOutputFileAnnotation], binding: String, includeRouterState: Boolean = false,
      restoreStaging: Boolean = false)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "native region cut snapshot: " + why)
  private def all(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }

  def generate(source: NativeCheckpointRegions.Result, owners: Seq[String],
      includeRouterState: Boolean = false, restoreStaging: Boolean = false): Result = {
    // Regenerate rather than accepting a caller-supplied, potentially modified
    // cut. This authenticates original regions, owners and raw-clock routing.
    val cut = NativeCheckpointRegionClockCut.generate(source, owners, observeRouterState = includeRouterState)
    val root = cut.state.circuit.modules.collectFirst {
      case m: Module if m.name == cut.state.circuit.main => m
    }.get
    val JString(clock) = parse(cut.binding) \ "raw_clock"
    val controlBits = source.descriptors.physicalClockGates + 71
    val routerObservation = parse(cut.graft.binding) \ "router_state_observation"
    val routerBits = if (!includeRouterState) 0 else {
      val JInt(width) = routerObservation \ "bits"
      check(width > 0 && width.isValidInt, "invalid complete router observation width")
      check(root.ports.contains(Port(NoInfo, "capture_router_state", Output, UIntType(IntWidth(width)))),
        "missing complete router observation")
      width.toInt
    }
    val bits = controlBits + routerBits
    check(!(root.ports.map(_.name) ++ all(root.body).collect { case d: IsDeclaration => d.name })
      .exists(n => n.startsWith(prefix) || n == instance) &&
      !cut.state.circuit.modules.exists {
        case m: ExtModule => Set(definition, "CheckpointNativeStateSnapshot").exists(n => m.name == n || m.defname == n)
        case m => Set(definition, "CheckpointNativeStateSnapshot").contains(m.name)
      }, "reserved staging identifier collision")
    def uint(n: Int): Type = UIntType(IntWidth(n))
    def p(n: String, d: Direction, w: Int = 1): Port = Port(NoInfo, n, d, uint(w))
    val restoreInputs = Seq(p("restore_hold", Input), p("restore_load", Input), p("restore_state", Input, bits + 71))
    val inputs = Seq(p("reset", Input)) ++ restoreInputs ++ Seq(p("host_freeze_request", Input),
      p("snapshot_request", Input), p("snapshot_release", Input), p("physical_read_enable", Input),
      p("physical_failed", Input), p("source_pending", Input), p("target_cycle", Input, 64),
      p("live_control_state", Input, bits), p("request_valid", Input), p("source_request_ready", Input),
      p("source_response_valid", Input), p("source_response_data", Input, 64),
      p("source_response_error", Input), p("response_ready", Input), p("word_address", Input, 32))
    val outputs = Seq(p("restore_ready", Output), p("cut_freeze_request", Output), p("request_ready", Output),
      p("source_request_valid", Output), p("source_response_ready", Output),
      p("response_valid", Output), p("response_data", Output, 64), p("response_error", Output),
      p("snapshot_ready", Output), p("snapshot_valid", Output), p("snapshot_failed", Output),
      p("word_data", Output, 64), p("cut_cycle", Output, 64), p("word_error", Output),
      p("snapshot_error", Output, 6), p("captured_control_state", Output, bits),
      p("staging_control_state", Output, bits + 71))
    val adapter = ExtModule(NoInfo, definition, Seq(Port(NoInfo, "clock", Input, ClockType)) ++
      inputs ++ outputs, definition, Seq(IntParam("CONTROL_BITS", bits), IntParam("RESTORE_ENABLE", if (restoreStaging) 1 else 0)))
    // Interpose on ALL six readout directions and the physical freeze request.
    // The existing pending register continues tracking the inner accepted
    // request, never the unqualified external request.
    val handshake = Seq("request_valid", "request_ready", "response_valid", "response_ready", "response_data", "response_error")
    val renamed = handshake.map(n => "capture_" + n -> (prefix + "source_" + n)).toMap +
      ("capture_cut_freeze_request" -> (prefix + "cut_freeze_request"))
    val originalPorts = root.ports.filter(p => renamed.contains(p.name))
    check(originalPorts.size == renamed.size, "missing original cut/readout ports")
    def ref(n: String, t: Type = uint(1)): Expression = WRef(n, t)
    def pin(n: String): Expression = WSubField(WRef(instance), n)
    def expr(e: Expression): Expression = e match {
      case r: WRef if renamed.contains(r.name) => r.copy(name = renamed(r.name))
      case x => x.mapExpr(expr)
    }
    def stmt(s: Statement): Statement = s.mapStmt(stmt).mapExpr(expr)
    val in = restoreInputs.map(p => p.name -> (prefix + p.name)).toMap ++
      Map("reset" -> "capture_reset", "host_freeze_request" -> "capture_cut_freeze_request",
      "snapshot_request" -> (prefix + "request"), "snapshot_release" -> (prefix + "release"),
      "physical_read_enable" -> "capture_read_enable", "physical_failed" -> "capture_cut_failed",
      "source_pending" -> "capture_cut_pending", "target_cycle" -> "capture_cut_target_cycle",
      "live_control_state" -> "capture_cut_control_state", "word_address" -> (prefix + "word_address")) ++
      handshake.filter(n => Set("request_valid", "response_ready").contains(n)).map(n => n -> ("capture_" + n)) ++
      handshake.filter(n => Set("request_ready", "response_valid", "response_data", "response_error").contains(n))
        .map(n => ("source_" + n) -> renamed("capture_" + n))
    val out = Map("restore_ready" -> (prefix + "restore_ready"),
      "cut_freeze_request" -> renamed("capture_cut_freeze_request"),
      "snapshot_ready" -> (prefix + "ready"), "snapshot_valid" -> (prefix + "valid"),
      "snapshot_failed" -> (prefix + "failed"), "snapshot_error" -> (prefix + "error")) ++
      Seq("word_data", "cut_cycle", "word_error", "captured_control_state", "staging_control_state")
        .map(n => n -> (prefix + n)) ++
      handshake.filter(n => Set("request_ready", "response_valid", "response_data", "response_error").contains(n))
        .map(n => n -> ("capture_" + n)) ++
      handshake.filter(n => Set("request_valid", "response_ready").contains(n))
        .map(n => ("source_" + n) -> renamed("capture_" + n))
    check(in.keySet == inputs.map(_.name).toSet && out.keySet == outputs.map(_.name).toSet,
      "incomplete staging port ownership")
    val restorePorts = if (!restoreStaging) Nil else restoreInputs.map(p => p.copy(name = prefix + p.name)) :+
      p(prefix + "restore_ready", Output)
    val extra = restorePorts ++ Seq(p(prefix + "request", Input), p(prefix + "release", Input), p(prefix + "word_address", Input, 32),
      p(prefix + "ready", Output), p(prefix + "valid", Output), p(prefix + "failed", Output),
      p(prefix + "word_data", Output, 64), p(prefix + "cut_cycle", Output, 64),
      p(prefix + "word_error", Output), p(prefix + "error", Output, 6),
      p(prefix + "captured_control_state", Output, bits), p(prefix + "staging_control_state", Output, bits + 71))
    val connections = Seq(Connect(NoInfo, pin("clock"), ref(clock, ClockType))) ++
      inputs.map(p => Connect(NoInfo, pin(p.name), if (p.name == "live_control_state" && includeRouterState)
        DoPrim(firrtl.PrimOps.Cat, Seq(ref("capture_router_state", uint(routerBits)),
          ref("capture_cut_control_state", uint(controlBits))), Nil, uint(bits))
        else if (!restoreStaging && restoreInputs.exists(_.name == p.name)) UIntLiteral(0, p.tpe.asInstanceOf[UIntType].width)
        else ref(in(p.name), p.tpe))) ++
      outputs.map(p => Connect(NoInfo, ref(out(p.name), p.tpe), pin(p.name)))
    val rewritten = root.copy(ports = root.ports ++ extra, body = Block(
      originalPorts.map(p => DefWire(NoInfo, renamed(p.name), p.tpe)) ++
        (if (restoreStaging) Nil else Seq(DefWire(NoInfo, prefix + "restore_ready", uint(1)))) ++ Seq(
        WDefInstance(NoInfo, instance, definition, firrtl.Utils.module_type(adapter)), stmt(root.body)) ++ connections))
    val circuit = cut.state.circuit.copy(modules = cut.state.circuit.modules.map(m =>
      if (m.name == root.name) rewritten else m) :+ adapter)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(firrtl.passes.ToWorkingIR.run(circuit)))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    val resources = Seq("CheckpointRegionCutSnapshot.sv" -> ".native-region-cut-snapshot.sv",
      "CheckpointNativeStateSnapshot.sv" -> ".native-region-cut-staging.sv").map { case (name, suffix) =>
      val resource = scala.io.Source.fromResource(name)
      val body = try resource.mkString finally resource.close()
      GoldenGateOutputFileAnnotation(body, suffix)
    }
    val routerBinding = if (!includeRouterState) Nil else List(
      "controller_state_bits" -> JInt(controlBits), "router_state_bits" -> JInt(routerBits),
      "router_state_offset" -> JInt(controlBits), "router_state_observation" -> routerObservation,
      "router_observation_atomically_staged" -> JBool(true),
      "payload_layout_lsb0" -> JArray(List(JString("cut_control"), JString("router_state"))))
    val restoreBinding = if (!restoreStaging) Nil else List(
      "staging_restore_extra_storage_bits" -> JInt(0),
      "staging_restore_contract" -> JString("whole_owner_raw_posedge_under_hold_with_physical_read_permission_and_idle_readers"),
      "original_controller_router_endpoint_restore_supported" -> JBool(false))
    val binding = canonical(JObject(List("snapshot_version" -> JInt(if (restoreStaging) 3 else if (includeRouterState) 2 else 1),
      "scope" -> JString("selected_native_regions_atomic_cut_control_observation_only"),
      "cut_binding_sha256" -> JString(sha256(cut.binding)),
      "attached_sha256" -> JString(sha256(flowed.serialize)),
      "adapter" -> JString(root.name + "." + instance), "raw_clock" -> JString(clock),
      "adapter_definition_sha256" -> JString(sha256(adapter.serialize)),
      "resource_hashes" -> JObject(resources.map(f => f.fileSuffix -> JString(sha256(f.body))).toList),
      "control_state_bits" -> JInt(bits), "staging_owner_bits" -> JInt(bits + 71),
      "controller_observation_atomically_staged" -> JBool(true),
      "readout_router_state_atomically_owned" -> JBool(false), "staging_owner_restore_supported" -> JBool(restoreStaging),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "metasim_restore_supported" -> JBool(false)) ++ routerBinding ++ restoreBinding))
    Result(cut.state.copy(circuit = flowed), cut, cut.files ++ resources, binding, includeRouterState, restoreStaging)
  }

  def validate(source: NativeCheckpointRegions.Result, owners: Seq[String], result: Result): Unit = {
    val expected = generate(source, owners, result.includeRouterState, result.restoreStaging)
    check(result == expected, "attached atomic snapshot differs from independent regeneration")
  }
}
