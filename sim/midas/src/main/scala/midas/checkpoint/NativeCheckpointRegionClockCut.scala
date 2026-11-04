// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, WDefInstance, WRef, WSubField}
import firrtl.ir._
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** A synthesized common low-phase controller for selected native regions.
  * Only selected functional subtrees stop; the enclosing host and all readout
  * logic keep their proven raw clock. The caller must separately own external
  * reset/CE/AXI/memory writers, target time and every uncovered execution.
  * This bounded connection never admits a production checkpoint image.
  */
object NativeCheckpointRegionClockCut {
  import SemanticStateInventory.{canonical, sha256}
  val prefix = "capture_cut_"
  val instance = "checkpoint_region_clock_cut"
  val definition = "CheckpointPhysicalClockCut"
  case class Result(state: CircuitState, graft: NativeCheckpointRegionGraft.Result,
      files: Seq[GoldenGateOutputFileAnnotation], binding: String, observeRouterState: Boolean = false)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "native region common cut: " + why)
  private def all(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }

  def generate(source: NativeCheckpointRegions.Result, owners: Seq[String],
      observeRouterState: Boolean = false): Result = {
    // Reconstruct all source, descriptors, CE bit order and graft endpoints.
    val graft = NativeCheckpointRegionGraft.generate(source, observeRouterState = observeRouterState)
    check(source.regions.nonEmpty && source.shells.isEmpty,
      "native regions without shell/borrowed-reader owners required")
    check(owners.nonEmpty && owners.size <= 65535 && owners.distinct.size == owners.size &&
      owners.forall(_.matches("[A-Za-z0-9_.:/-]+")), "explicit unique ordered external owners required")
    val gates = source.descriptors.physicalClockGates
    check(gates > 0, "physical CE owners required")
    val root = graft.state.circuit.modules.collectFirst {
      case m: Module if m.name == graft.state.circuit.main => m
    }.get
    val JString(clock) = parse(source.binding) \ "raw_clock"
    check(root.ports.exists(p => p.name == clock && p.direction == Input && p.tpe == ClockType),
      "missing proven scalar raw source clock")
    check(!(root.ports.map(_.name) ++ all(root.body).collect { case d: IsDeclaration => d.name })
      .exists(n => n.startsWith(prefix) || n == instance) &&
      !graft.state.circuit.modules.exists {
        case m: ExtModule => m.name == definition || m.defname == definition
        case m => m.name == definition
      }, "reserved controller identifier collision")
    val bit = UIntType(IntWidth(1))
    def uint(n: Int): Type = UIntType(IntWidth(n))
    def p(n: String, d: Direction, w: Int = 1): Port = Port(NoInfo, n, d, uint(w))
    val controllerInputs = Seq(p("reset", Input), p("restore_hold", Input),
      p("restore_load_pos", Input), p("restore_load_neg", Input), p("restore_state", Input, gates + 70),
      p("freeze_request", Input), p("ce", Input, gates), p("target_cycle", Input, 64),
      p("owner_present", Input, owners.size))
    val controllerOutputs = Seq(Port(NoInfo, "root_clock", Output, ClockType),
      p("gate_clock", Output, gates), p("clock_mask", Output), p("stopped", Output), p("read_enable", Output),
      p("failed", Output), p("captured_ce", Output, gates), p("cut_cycle", Output, 64),
      p("error", Output, 3), p("control_state", Output, gates + 70))
    val controller = ExtModule(NoInfo, definition,
      Seq(Port(NoInfo, "clock", Input, ClockType)) ++ controllerInputs ++ controllerOutputs, definition,
      Seq(IntParam("GATES", gates), IntParam("OWNERS", owners.size),
        IntParam("EXTERNAL_CLOCK_NETWORK", 1), IntParam("RESTORE_ENABLE", 0)))
    val owned = Map("capture_clock_mask" -> 1, "capture_read_enable" -> 1, "capture_captured_ce" -> gates)
    owned.foreach { case (n, w) =>
      check(root.ports.count(p => p.name == n && p.direction == Input && p.tpe == uint(w)) == 1,
        "missing or changed collector input " + n)
    }
    val handshake = Seq("request_valid", "request_ready", "response_valid", "response_ready", "response_data", "response_error")
    val raw = handshake.map(n => "capture_" + n -> (prefix + "raw_" + n)).toMap
    val rawPorts = root.ports.filter(p => raw.contains(p.name))
    check(rawPorts.size == handshake.size, "missing readout handshake")
    def ref(n: String, t: Type = bit): Expression = WRef(n, t)
    def and(a: Expression, b: Expression): Expression = DoPrim(firrtl.PrimOps.And, Seq(a, b), Nil, bit)
    def pin(n: String): Expression = WSubField(WRef(instance), n)
    def expr(e: Expression): Expression = e match {
      case r: WRef if raw.contains(r.name) => r.copy(name = raw(r.name))
      case x => x.mapExpr(expr)
    }
    def stmt(s: Statement): Statement = s.mapStmt(stmt).mapExpr(expr)
    val pending = prefix + "accepted_pending"
    val permission = ref("capture_read_enable")
    val requested = ref(prefix + "freeze_request")
    val accept = and(and(ref("capture_request_valid"), ref(raw("capture_request_ready"))), and(permission, requested))
    val finish = and(and(ref(raw("capture_response_valid")), ref("capture_response_ready")), permission)
    val one = UIntLiteral(1, IntWidth(1)); val zero = UIntLiteral(0, IntWidth(1))
    val pendingReg = DefRegister(NoInfo, pending, bit, ref(clock, ClockType), zero, ref(pending))
    val connections = Seq(
      Connect(NoInfo, ref(pending), Mux(ref("capture_reset"), zero,
        Mux(accept, one, Mux(finish, zero, ref(pending), bit), bit), bit)),
      Connect(NoInfo, pin("clock"), ref(clock, ClockType)), Connect(NoInfo, pin("reset"), ref("capture_reset")),
      Connect(NoInfo, pin("restore_hold"), zero), Connect(NoInfo, pin("restore_load_pos"), zero),
      Connect(NoInfo, pin("restore_load_neg"), zero),
      Connect(NoInfo, pin("restore_state"), UIntLiteral(0, IntWidth(gates + 70))),
      Connect(NoInfo, pin("freeze_request"), DoPrim(firrtl.PrimOps.Or, Seq(requested, ref(pending)), Nil, bit)),
      Connect(NoInfo, pin("ce"), ref("capture_logical_ce", uint(gates))),
      Connect(NoInfo, pin("target_cycle"), ref(prefix + "target_cycle", uint(64))),
      Connect(NoInfo, pin("owner_present"), ref(prefix + "owner_present", uint(owners.size))),
      Connect(NoInfo, ref(raw("capture_request_valid")), and(and(ref("capture_request_valid"), permission), requested)),
      Connect(NoInfo, ref("capture_request_ready"), and(and(ref(raw("capture_request_ready")), permission), requested)),
      Connect(NoInfo, ref("capture_response_valid"), and(ref(raw("capture_response_valid")), permission)),
      Connect(NoInfo, ref(raw("capture_response_ready")), and(ref("capture_response_ready"), permission)),
      Connect(NoInfo, ref("capture_response_data", uint(64)),
        Mux(permission, ref(raw("capture_response_data"), uint(64)), UIntLiteral(0, IntWidth(64)), uint(64))),
      Connect(NoInfo, ref("capture_response_error"), and(ref(raw("capture_response_error")), permission))) ++
      owned.toSeq.sortBy(_._1).map { case (n, w) => Connect(NoInfo, ref(n, uint(w)), pin(n.stripPrefix("capture_"))) } ++
      Seq("stopped" -> "stopped", "failed" -> "failed", "cycle" -> "cut_cycle", "error" -> "error")
        .map { case (n, out) => Connect(NoInfo, ref(prefix + n), pin(out)) } ++ Seq(
        Connect(NoInfo, ref(prefix + "pending"), ref(pending)),
        Connect(NoInfo, ref(prefix + "control_state", uint(gates + 71)),
          DoPrim(firrtl.PrimOps.Cat, Seq(ref(pending), pin("control_state")), Nil, uint(gates + 71))))
    val extra = Seq(p(prefix + "freeze_request", Input), p(prefix + "target_cycle", Input, 64),
      p(prefix + "owner_present", Input, owners.size), p(prefix + "stopped", Output), p(prefix + "failed", Output),
      p(prefix + "cycle", Output, 64), p(prefix + "error", Output, 3), p(prefix + "control_state", Output, gates + 71),
      p(prefix + "pending", Output))
    // Track accepted transactions directly, independently of router readiness.
    val rewritten = root.copy(ports = root.ports.map(p => if (owned.contains(p.name)) p.copy(direction = Output) else p) ++ extra,
      body = Block(rawPorts.map(p => DefWire(NoInfo, raw(p.name), p.tpe)) ++ Seq(pendingReg,
        WDefInstance(NoInfo, instance, definition, firrtl.Utils.module_type(controller)), stmt(root.body)) ++ connections))
    val circuit = graft.state.circuit.copy(modules = graft.state.circuit.modules.map(m =>
      if (m.name == root.name) rewritten else m) :+ controller)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(firrtl.passes.ToWorkingIR.run(circuit)))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    val resource = scala.io.Source.fromResource("CheckpointPhysicalClockCut.sv")
    val body = try resource.mkString finally resource.close()
    val file = GoldenGateOutputFileAnnotation(body, ".native-region-cut-controller.sv")
    val binding = canonical(JObject(List("cut_version" -> JInt(1),
      "scope" -> JString("selected_native_regions_common_low_phase_capture_only"),
      "regions_binding_sha256" -> JString(sha256(source.binding)),
      "graft_binding_sha256" -> JString(sha256(graft.binding)),
      "attached_sha256" -> JString(sha256(flowed.serialize)),
      "controller" -> JString(root.name + "." + instance), "raw_clock" -> JString(clock),
      "controller_definition_sha256" -> JString(sha256(controller.serialize)),
      "controller_source_sha256" -> JString(sha256(body)),
      "owners" -> JArray(owners.map(JString(_)).toList),
      "physical_clock_gates" -> JInt(gates),
      "physical_ce_order" -> (parse(source.descriptors.manifest) \ "physical_ce_order"),
      "pending_owner" -> JString(root.name + "." + pending),
      "control_state_bits" -> JInt(gates + 71),
      "control_state_lsb0" -> JArray(List(JString("physical_controller"), JString("accepted_request_pending"))),
      "release_contract" -> JString("block_new_requests_and_hold_cut_through_accepted_response"),
      "fault_contract" -> JString("combinational_handshake_revocation_sticky_stop_until_shared_reset"),
      "uncovered_functional_state" -> (parse(source.descriptors.manifest) \ "uncovered_functional_state"),
      "uncovered_physical_gates" -> (parse(source.descriptors.manifest) \ "uncovered_physical_gates"),
      "target_time_authority_proven" -> JBool(false), "external_writer_owners_bound" -> JBool(false),
      "reset_writer_owners_bound" -> JBool(false), "production_freeze_owners_bound" -> JBool(false),
      "controller_state_atomically_owned" -> JBool(false), "complete_fpga_capture" -> JBool(false),
      "deployable_capture_image" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    Result(graft.state.copy(circuit = flowed), graft, graft.files :+ file, binding, observeRouterState)
  }

  def validate(source: NativeCheckpointRegions.Result, owners: Seq[String], result: Result): Unit = {
    val expected = generate(source, owners, result.observeRouterState)
    check(result.state == expected.state && result.graft == expected.graft &&
      result.files == expected.files && result.binding == expected.binding,
      "attached common-cut circuit differs from independent regeneration")
  }
}
