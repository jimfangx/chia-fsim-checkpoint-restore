// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, Parser}
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Atomic observation of the running collector AND the bank response owner.
  * The functional banks stay closed until staging succeeds. Staging holds the
  * same collector cut through reads and host withdrawal; release requires an
  * idle reader. Faults revoke both handshakes before an edge and retain the cut.
  * This companion supplies no production transport, global target-time proof,
  * staging restore owner, or complete simulator/host/functional-memory coverage.
  */
object F2NativeCollectorSnapshot {
  import SemanticStateInventory.{canonical, sha256}
  val prefix = "checkpoint_f2_shadow_"
  val instance = prefix + "adapter"
  val definition = "CheckpointNativeStateSnapshot"
  case class Result(state: CircuitState, collector: F2ClockResetCutWiring.Result, binding: String)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "F2 native collector staging: " + why)
  private def statements(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }

  def generate(input: CircuitState, manifest: String, plan: String, resetPort: String,
      cyclePort: String, owners: Int, dependencies: String, banks: GeneratedStateBankAccess.Result): Result = {
    // Reconstruct every input and ownership obligation, rather than trusting a
    // caller-provided connected circuit or a binding with edited hashes.
    val collector = F2ClockResetCutWiring.generateNative(input, manifest, plan, resetPort,
      cyclePort, owners, dependencies, banks)
    val root = collector.state.circuit.modules.collectFirst {
      case m: Module if m.name == input.circuit.main => m
    }.get
    check(!(root.ports.map(_.name) ++ statements(root.body).collect { case d: IsDeclaration => d.name })
      .exists(_.startsWith(prefix)), "reserved staging identifier collision")
    check(!collector.state.circuit.modules.exists {
      case m: ExtModule => m.name == definition || m.defname == definition
      case m => m.name == definition
    }, "staging definition collision")
    val gates = (parse(plan) \ "physical_gates").asInstanceOf[JArray].arr.size
    val clock = (parse(plan) \ "common_source_clock").asInstanceOf[JString].s
    // Check actual compiler-owned registers before exposing a fixed ABI.
    val endpoint = Seq("ckpt_pending" -> 1, "ckpt_saved_data" -> 64, "ckpt_saved_error" -> 1)
    val registers = statements(root.body).collect { case r: DefRegister => r }
    endpoint.foreach { case (name, width) =>
      check(registers.count(r => r.name == name && r.tpe == UIntType(IntWidth(width)) &&
        r.clock.serialize == "ckpt_clock") == 1, "changed endpoint state ABI: " + name)
    }
    val collectorBits = gates + 79
    val bits = collectorBits + 66
    check(root.ports.count(p => p.name == "checkpoint_f2_control_state" &&
      p.direction == Output && p.tpe == UIntType(IntWidth(collectorBits))) == 1, "changed collector state ABI")
    def port(n: String, d: Direction, w: Int): Port = Port(NoInfo, n, d, UIntType(IntWidth(w)))
    val inputs = Seq("request" -> 1, "release" -> 1, "word_address" -> 32)
    val outputs = Seq("ready" -> 1, "valid" -> 1, "failed" -> 1, "word_data" -> 64,
      "word_error" -> 1, "cut_cycle" -> 64, "error" -> 6, "control_state" -> (bits + 71))
    val ext = ExtModule(NoInfo, definition, Seq(Port(NoInfo, "clock", Input, ClockType)) ++
      Seq("reset" -> 1, "restore_hold" -> 1, "restore_load" -> 1, "restore_state" -> (bits + 71),
        "request" -> 1, "release_snapshot" -> 1, "owners_present" -> 1,
        "functional_frozen" -> 1, "reader_idle" -> 1, "borrow_attempt" -> 1,
        "target_cycle" -> 64, "live_state" -> bits, "word_address" -> 32).map { case (n,w) => port(n, Input, w) } ++
      Seq("restore_ready" -> 1, "request_ready" -> 1, "valid" -> 1, "borrow_enable" -> 1,
        "failed" -> 1, "word_data" -> 64, "word_error" -> 1, "snapshot_state" -> bits,
        "cut_cycle" -> 64, "error" -> 6, "control_state" -> (bits + 71)).map { case (n,w) => port(n, Output, w) },
      definition, Seq(IntParam("BITS", bits), IntParam("RESTORE_ENABLE", 0)))
    val extra = s"""circuit Stage :
  module Stage :
    $instance.clock <= $clock
    $instance.reset <= checkpoint_f2_controller_reset
    $instance.restore_hold <= UInt<1>(0)
    $instance.restore_load <= UInt<1>(0)
    $instance.restore_state <= UInt<${bits + 71}>(0)
    $instance.request <= ${prefix}request
    $instance.release_snapshot <= ${prefix}release
    $instance.owners_present <= not(checkpoint_f2_failed)
    $instance.functional_frozen <= and(checkpoint_f2_clock_mask, checkpoint_f2_stopped)
    $instance.reader_idle <= not(ckpt_pending)
    $instance.borrow_attempt <= and(ckpt_req_valid, checkpoint_f2_freeze_request)
    $instance.target_cycle <= $cyclePort
    $instance.live_state <= cat(checkpoint_f2_control_state, cat(ckpt_saved_error, cat(ckpt_saved_data, ckpt_pending)))
    $instance.word_address <= ${prefix}word_address
    ${prefix}permission <= and($instance.borrow_enable, checkpoint_f2_read_enable)
    ${prefix}ready <= $instance.request_ready
    ${prefix}valid <= and($instance.valid, checkpoint_f2_read_enable)
    ${prefix}failed <= $instance.failed
    ${prefix}word_data <= mux(${prefix}valid, $instance.word_data, UInt<64>(0))
    ${prefix}word_error <= or($instance.word_error, not(${prefix}valid))
    ${prefix}cut_cycle <= $instance.cut_cycle
    ${prefix}error <= $instance.error
    ${prefix}control_state <= $instance.control_state
"""
    val wiring = Parser.parse(extra).modules.head.asInstanceOf[Module].body
    def expression(text: String): Expression = Parser.parse(
      "circuit E :\n  module E :\n    node value = " + text + "\n")
      .modules.head.asInstanceOf[Module].body match {
        case n: DefNode => n.value
        case Block(Seq(n: DefNode)) => n.value
        case _ => throw new IllegalArgumentException("invalid staging expression")
      }
    // An occupied epoch cannot be released by a protocol fault: the snapshot
    // keeps occupied set unless a SAFE idle release is accepted. Retained
    // errors also hold failed empty epochs. Use those registers for cut hold,
    // not the live failure cone (which includes the collector's own cycle
    // comparison). Immediate fault revocation still gates every handshake.
    val hold = expression(s"or(${prefix}request, or(bits($instance.control_state, $bits, $bits), orr($instance.error)))")
    def rewrite(s: Statement): Statement = s match {
      // The ordinary bank needs only pending reset because invalid payload is
      // invisible on its read interface. Complete instrumentation capture also
      // owns those inactive bytes: define them before the FIRST staged cut.
      // This changes no functional storage and no accepted response. Later
      // reads still retain their old data/error after response acknowledgement.
      // LowForm has already lowered synchronous reset into next-state muxes.
      case c: Connect if Set("ckpt_saved_data", "ckpt_saved_error")(c.loc.serialize) =>
        val width = endpoint.toMap.apply(c.loc.serialize)
        c.copy(expr = Mux(firrtl.WRef("checkpoint_f2_controller_reset", UIntType(IntWidth(1))),
          UIntLiteral(0, IntWidth(width)), c.expr, UIntType(IntWidth(width))))
      case c: Connect if c.loc.serialize == F2ClockResetCutWiring.adapter + ".freeze_request" =>
        c.copy(expr = DoPrim(firrtl.PrimOps.Or, Seq(c.expr, hold), Nil, UIntType(IntWidth(1))))
      case c: Connect if Set("checkpoint_f2_bank_ckpt_req_valid", "ckpt_req_ready", "ckpt_resp_valid",
          "checkpoint_f2_bank_ckpt_resp_ready")(c.loc.serialize) =>
        def permission(e: Expression): Expression = e match {
          case r: firrtl.WRef if r.name == "checkpoint_f2_read_enable" => r.copy(name = prefix + "permission")
          case x => x.mapExpr(permission)
        }
        c.copy(expr = permission(c.expr))
      case x => x.mapStmt(rewrite)
    }
    val composed = root.copy(ports = root.ports ++ inputs.map { case (n,w) => port(prefix + n, Input, w) } ++
      outputs.map { case (n,w) => port(prefix + n, Output, w) },
      body = Block(Seq(firrtl.WDefInstance(instance, definition),
        DefWire(NoInfo, prefix + "permission", UIntType(IntWidth(1))), rewrite(root.body), wiring)))
    val circuit = collector.state.circuit.copy(modules = collector.state.circuit.modules.map {
      case m if m.name == root.name => composed
      case m => m
    } :+ ext)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(firrtl.passes.ToWorkingIR.run(circuit)))
    val resolved = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(resolved); firrtl.passes.CheckFlows.run(resolved)
    val state = collector.state.copy(circuit = resolved)
    val fields = endpoint.scanLeft(0)((offset, f) => offset + f._2).dropRight(1).zip(endpoint).map {
      case (offset, (name, width)) => JObject(List("path" -> JString(root.name + "." + name),
        "offset" -> JInt(offset), "width" -> JInt(width)))
    } :+ JObject(List("path" -> JString(root.name + ".checkpoint_f2_control_state"),
      "offset" -> JInt(66), "width" -> JInt(collectorBits)))
    val sources = Seq("CheckpointNativeStateSnapshot.sv", "CheckpointF2ClockResetCut.sv",
      "CheckpointF2ResetGuard.sv", "CheckpointResetOwnedClockCut.sv", "CheckpointPhysicalClockCut.sv").map { name =>
      val stream = getClass.getResourceAsStream("/" + name)
      check(stream != null, "missing state implementation resource " + name)
      val bytes = try stream.readAllBytes() finally stream.close()
      name -> JString(sha256(new String(bytes, java.nio.charset.StandardCharsets.UTF_8)))
    }
    val binding = canonical(JObject(List("staging_version" -> JInt(1),
      "scope" -> JString("compiler_connected_native_collector_atomic_observation_only"),
      "collector_binding_sha256" -> JString(sha256(collector.binding)),
      "reference_sha256" -> JString(sha256(input.circuit.serialize)),
      "wired_sha256" -> JString(sha256(state.circuit.serialize)),
      "sources" -> JObject(sources.toList),
      "raw_clock" -> JString(root.name + "." + clock), "bits" -> JInt(bits),
      "words" -> JInt((bits + 63) / 64), "state" -> JArray(fields.toList),
      "staging_owner_bits" -> JInt(bits + 71), "staging_owner" -> JString(root.name + "." + instance),
      "snapshot_contract" -> JString("pre_edge_collector_and_idle_endpoint_before_first_functional_read"),
      "endpoint_initialization" -> JString("pending_data_and_error_synchronous_ungated_controller_reset_zero"),
      "release_contract" -> JString("explicit_idle_release_after_drain_then_common_low_phase_release"),
      "cut_hold_contract" -> JString("request_or_occupied_or_retained_error_no_live_failure_feedback"),
      "staging_restore_supported" -> JBool(false), "target_time_authority_proven" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "complete_fpga_capture" -> JBool(false),
      "deployable_capture_image" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    Result(state, collector, binding)
  }
}
