// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, Parser, WDefInstance, WRef, WSubField}
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Stage the explicitly selected original live registers in an existing shell.
  * Their declarations and next-state equations remain unchanged. The bank
  * endpoint and staging use the SAME original raw clock; no independent access
  * clock can race staging. Bank handshakes open only after staging and close
  * immediately on release/fault. This does not own external writers, native
  * reader pipelines, target time, the snapshot's own restore, or a full image.
  */
object NativeCheckpointLiveShellSnapshot {
  import SemanticStateInventory.{canonical, sha256}
  val definition = "CheckpointNativeStateSnapshot"
  case class Result(state: CircuitState, shell: NativeCheckpointShellStateAccess.Result, binding: String,
      stageNativeState: Boolean = false, enableRestore: Boolean = false)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "native live shell snapshot: " + why)
  private def statements(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  def generate(shell: NativeCheckpointShellStateAccess.Result, stageNativeState: Boolean = false,
      enableRestore: Boolean = false): Result = {
    NativeCheckpointShellStateAccess.validate(shell)
    check(shell.liveRegisters.nonEmpty, "explicit live register partition required")
    val doc = parse(shell.mapping)
    val JString(path) = doc \ "selected_execution"
    val owners = (doc \ "unsupported_live_owners").asInstanceOf[JArray].arr
    val clocks = owners.map(r => (r \ "raw_clock").asInstanceOf[JString].s).distinct
    check(clocks.size == 1, "all live owners must share one original raw input clock")
    val rawClock = clocks.head
    val p = shell.sidebandPrefix
    val prefix = p + "live_snapshot_"
    val instance = prefix + "adapter"
    val live = GeneratedExternalOwners.instances(shell.state, shell.state.circuit.main).toMap
    val selected = live(path).asInstanceOf[Module]
    check(selected.ports.count(q => q.name == rawClock && q.direction == Input && q.tpe == ClockType) == 1,
      "original raw input clock missing")
    val all = statements(selected.body)
    check(all.count { case r: DefRegister => r.name == p + "ckpt_pending"; case _ => false } == 1,
      "one actual bank response owner required")
    val components = path.split("\\.").toVector
    val chain = components.indices.map(i => components.take(i + 1).mkString(".")).map { execution =>
      val m = live(execution).asInstanceOf[Module]
      check(live.values.count(_.name == m.name) == 1, "unique routing execution required: " + execution)
      check(!(m.ports.map(_.name) ++ statements(m.body).collect { case d: IsDeclaration => d.name })
        .exists(_.startsWith(prefix)), "reserved staging identifier collision")
      check(m.ports.count(q => q.name == p + "ckpt_clock" && q.direction == Input && q.tpe == ClockType) == 1,
        "missing independently routed bank access clock")
      m
    }
    // A local Clock input can itself be driven by a stopped child gate or a
    // data cast in an ancestor. Prove the complete original route to a root
    // input before calling it an ungated staging/access authority.
    var enclosingRaw = rawClock
    chain.dropRight(1).zipWithIndex.reverse.foreach { case (m,index) =>
      val all = statements(m.body)
      val drivers = all.collect {
        case c: Connect => c.loc.serialize -> c.expr
        case n: DefNode => n.name -> n.value
      }.groupBy(_._1)
      val aliases = all.collect {
        case n: DefNode if n.value.tpe == ClockType => n.name
        case w: DefWire if w.tpe == ClockType => w.name
      }.toSet
      def resolve(e: Expression, seen: Set[String] = Set.empty): String = {
        check(!seen(e.serialize), "ancestor clock alias cycle")
        e match {
          case r: WRef if r.tpe == ClockType && m.ports.exists(q =>
              q.name == r.name && q.direction == Input && q.tpe == ClockType) => r.name
          case r: WRef if r.tpe == ClockType && aliases(r.name) =>
            val ds = drivers.getOrElse(r.name, Nil)
            check(ds.size == 1, "ancestor clock alias requires one driver")
            resolve(ds.head._2, seen + r.name)
          case _ => throw new IllegalArgumentException(
            "native live shell snapshot: nontransparent ancestor raw clock " + e.serialize)
        }
      }
      val ds = drivers.getOrElse(components(index + 1) + "." + enclosingRaw, Nil)
      check(ds.size == 1, "ancestor raw clock requires one complete driver")
      enclosingRaw = resolve(ds.head._2)
    }
    check(!shell.state.circuit.modules.exists {
      case m: ExtModule => m.name == definition || m.defname == definition
      case m => m.name == definition
    }, "staging definition collision")
    var bits = 0
    val rows = owners.map { owner =>
      val JString(hierarchy) = owner \ "hierarchy"
      val name = hierarchy.stripPrefix(path + ".")
      val r = all.collect { case r: DefRegister if r.name == name => r }.head
      val width = (owner \ "width").asInstanceOf[JInt].num
      check(width > 0 && width.isValidInt && bits.toLong + width.toLong + 71 <= Int.MaxValue,
        "staging vector exceeds supported width")
      val row = JObject(List("state_id" -> (owner \ "state_id"), "hierarchy" -> JString(hierarchy),
        "expression" -> JString(name), "width" -> JInt(width), "offset" -> JInt(bits),
        "signed" -> JBool(r.tpe.isInstanceOf[SIntType]),
        "declaration_sha256" -> (owner \ "declaration_sha256"), "driver_sha256" -> (owner \ "driver_sha256")))
      bits += width.toInt
      row
    }
    val originalBits = bits
    val observation = parse(shell.banks.binding) \ "native_state_observation"
    // The synchronous adapter already inventories every derived register,
    // including its emitter-owned RAM data pipeline. Project its local names
    // through the same shell graft rather than discovering a second subset.
    val nativeRows = if (!stageNativeState) Nil else {
      check(observation.isInstanceOf[JObject], "native staging requires a source-bound synchronous observation")
      val local = (observation \ "state").asInstanceOf[JArray].arr
      val JInt(nativeBits) = observation \ "bits"
      val nativeModule = shell.banks.state.circuit.modules.head.asInstanceOf[Module]
      val nativeDrivers = statements(nativeModule.body).collect {
        case c: Connect => c.loc.serialize -> c.expr
        case n: DefNode => n.name -> n.value
      }.groupBy(_._1)
      def sameRawClock(clock: String, seen: Set[String] = Set.empty): Boolean = {
        check(!seen(clock), "native clock alias cycle")
        if (Set(rawClock, "ckpt_clock")(clock)) true
        else nativeDrivers.get(clock) match {
          case Some(Seq((_, e: WRef))) if e.tpe == ClockType => sameRawClock(e.serialize, seen + clock)
          case Some(Seq((_, e: WSubField))) if e.tpe == ClockType => sameRawClock(e.serialize, seen + clock)
          case _ => false
        }
      }
      check(nativeBits > 0 && nativeBits.isValidInt && bits.toLong + nativeBits.toLong + 71 <= Int.MaxValue,
        "native staging vector exceeds supported width")
      val projected = local.map { row =>
        val JString(expression) = row \ "expression"
        val JString(hierarchy) = row \ "hierarchy"
        val JInt(offset) = row \ "offset"
        val name = hierarchy.stripPrefix(shell.reference.circuit.main + ".")
        check(name != hierarchy, "unrooted native state owner")
        val actualName = if (name.startsWith("ckpt_")) p + name else name
        val actualExpression = if (expression.startsWith("ckpt_")) p + expression else expression
        val JString(clock) = row \ "clock_expression"
        check(sameRawClock(clock), "native state must share the original raw clock")
        val base = row.asInstanceOf[JObject].obj.filterNot(x =>
          Set("hierarchy", "expression", "offset", "instrumented_state_id")(x._1))
        JObject(base ++ List("hierarchy" -> JString(path + "." + actualName),
          "expression" -> JString(actualExpression), "offset" -> JInt(originalBits + offset),
          "instrumented_state_id" -> JString(SimulatorStateInventory.stateID(path + "." + actualName, "register")),
          "local_instrumented_state_id" -> (row \ "instrumented_state_id")))
      }
      bits += nativeBits.toInt
      projected
    }
    def port(n: String, d: Direction, w: Int): Port = Port(NoInfo, n, d, UIntType(IntWidth(w)))
    val inputs = Seq("reset" -> 1, "request" -> 1, "release" -> 1, "owners_present" -> 1,
      "owners_frozen" -> 1, "target_cycle" -> 64, "word_address" -> 32)
    val outputs = Seq("ready" -> 1, "valid" -> 1, "borrow_enable" -> 1, "failed" -> 1,
      "word_data" -> 64, "word_error" -> 1, "cut_cycle" -> 64, "error" -> 6,
      "control_state" -> (bits + 71))
    // Loading this local owner is independent of restoring the original live
    // registers/native reader. Keep those deficits explicit. Hold must span
    // the complete load/readback interval, even for an empty donor owner.
    val restoreInputs = if (enableRestore) Seq("restore_hold" -> 1, "restore_load" -> 1,
      "restore_state" -> (bits + 71)) else Nil
    val restoreOutputs = if (enableRestore) Seq("restore_ready" -> 1) else Nil
    val extraPorts = (inputs ++ restoreInputs).map { case (n,w) => port(prefix + n, Input, w) } ++
      (outputs ++ restoreOutputs).map { case (n,w) => port(prefix + n, Output, w) }
    val ext = ExtModule(NoInfo, definition, Seq(Port(NoInfo, "clock", Input, ClockType)) ++
      Seq("reset" -> 1, "restore_hold" -> 1, "restore_load" -> 1, "restore_state" -> (bits + 71),
        "request" -> 1, "release_snapshot" -> 1, "owners_present" -> 1,
        "functional_frozen" -> 1, "reader_idle" -> 1, "borrow_attempt" -> 1,
        "target_cycle" -> 64, "live_state" -> bits, "word_address" -> 32).map { case (n,w) => port(n, Input, w) } ++
      Seq("restore_ready" -> 1, "request_ready" -> 1, "valid" -> 1, "borrow_enable" -> 1,
        "failed" -> 1, "word_data" -> 64, "word_error" -> 1, "snapshot_state" -> bits,
        "cut_cycle" -> 64, "error" -> 6, "control_state" -> (bits + 71))
        .map { case (n,w) => port(n, Output, w) }, definition,
      Seq(IntParam("BITS", bits), IntParam("RESTORE_ENABLE", if (enableRestore) 1 else 0)))
    val vector = (rows ++ nativeRows).reverse.map { row =>
      val JString(name) = row \ "expression"
      if ((row \ "signed") == JBool(true)) s"asUInt($name)" else name
    }.reduceLeft((a,b) => s"cat($a, $b)")
    val connections = s"""circuit W :
  module W :
    $instance.clock <= $rawClock
    $instance.reset <= ${prefix}reset
    $instance.restore_hold <= ${if (enableRestore) prefix + "restore_hold" else "UInt<1>(0)"}
    $instance.restore_load <= ${if (enableRestore) prefix + "restore_load" else "UInt<1>(0)"}
    $instance.restore_state <= ${if (enableRestore) prefix + "restore_state" else s"UInt<${bits + 71}>(0)"}
    $instance.request <= ${prefix}request
    $instance.release_snapshot <= ${prefix}release
    $instance.owners_present <= and(${prefix}owners_present, not(${p}ckpt_reset))
    $instance.functional_frozen <= and(${p}ckpt_freeze, ${prefix}owners_frozen)
    $instance.reader_idle <= ${if (stageNativeState) s"and(${prefix}raw_req_ready, not(${prefix}raw_resp_valid))" else s"not(${p}ckpt_pending)"}
    $instance.borrow_attempt <= ${p}ckpt_req_valid
    $instance.target_cycle <= ${prefix}target_cycle
    $instance.live_state <= $vector
    $instance.word_address <= ${prefix}word_address
    ${prefix}gated_req_valid <= and(${p}ckpt_req_valid, $instance.borrow_enable)
    ${prefix}gated_resp_ready <= and(${p}ckpt_resp_ready, $instance.borrow_enable)
    ${p}ckpt_req_ready <= and(${prefix}raw_req_ready, $instance.borrow_enable)
    ${p}ckpt_resp_valid <= and(${prefix}raw_resp_valid, $instance.borrow_enable)
    ${prefix}functional_hold <= ${if (enableRestore) s"or(${prefix}restore_hold, " else ""}or(${p}ckpt_freeze, or(${prefix}request, or(bits($instance.control_state, $bits, $bits), orr($instance.error))))${if (enableRestore) ")" else ""}
"""
    val wires = Seq("gated_req_valid", "gated_resp_ready", "raw_req_ready", "raw_resp_valid", "functional_hold")
      .map(n => DefWire(NoInfo, prefix + n, UIntType(IntWidth(1))))
    val replaceInputs = Map(p + "ckpt_clock" -> rawClock,
      p + "ckpt_req_valid" -> (prefix + "gated_req_valid"),
      p + "ckpt_resp_ready" -> (prefix + "gated_resp_ready"),
      p + "ckpt_freeze" -> (prefix + "functional_hold"))
    val replaceOutputs = Map(p + "ckpt_req_ready" -> (prefix + "raw_req_ready"),
      p + "ckpt_resp_valid" -> (prefix + "raw_resp_valid"))
    def expr(e: Expression): Expression = e match {
      case r: WRef if replaceInputs.contains(r.name) => r.copy(name = replaceInputs(r.name))
      case x => x.mapExpr(expr)
    }
    def bank(s: Statement): Statement = s match {
      // Initialize only derived explicit storage. The implicit RAM data owner
      // is initialized by the original RAM execution, never invented/reset by
      // this adapter. Once initialized, every inactive byte is retained.
      case c: Connect if stageNativeState && nativeRows.exists(row =>
          (row \ "expression") == JString(c.loc.serialize)) =>
        val r = all.collect { case r: DefRegister if r.name == c.loc.serialize => r }.head
        val zero = if (r.tpe.isInstanceOf[SIntType]) SIntLiteral(0, r.tpe.asInstanceOf[SIntType].width)
          else UIntLiteral(0, r.tpe.asInstanceOf[UIntType].width)
        c.copy(expr = Mux(WRef(p + "ckpt_reset", UIntType(IntWidth(1))), zero, expr(c.expr), r.tpe))
      case c: Connect if replaceOutputs.contains(c.loc.serialize) =>
        c.copy(loc = WRef(replaceOutputs(c.loc.serialize), c.loc.tpe), expr = expr(c.expr))
      case x => x.mapStmt(bank).mapExpr(expr)
    }
    val outputWiring = (outputs ++ restoreOutputs).map { case (name,_) =>
      val pin = if (name == "ready") "request_ready" else name
      Connect(NoInfo, WRef(prefix + name), WSubField(WRef(instance), pin))
    }
    val changed = chain.zipWithIndex.map { case (m,index) =>
      val ports = m.ports.filterNot(_.name == p + "ckpt_clock") ++ extraPorts
      val body = if (index == chain.size - 1)
        Block(Seq(WDefInstance(instance, definition)) ++ wires ++ Seq(bank(m.body),
          Parser.parse(connections).modules.head.asInstanceOf[Module].body) ++ outputWiring)
      else {
        val child = components(index + 1)
        def removeClock(s: Statement): Statement = s match {
          case c: Connect if c.loc.serialize == child + "." + p + "ckpt_clock" => EmptyStmt
          case x => x.mapStmt(removeClock)
        }
        Block(Seq(removeClock(m.body)) ++ extraPorts.map { q =>
          val pin = WSubField(WRef(child), q.name, q.tpe)
          val ref = WRef(q.name, q.tpe)
          if (q.direction == Input) Connect(NoInfo, pin, ref) else Connect(NoInfo, ref, pin)
        })
      }
      m.name -> m.copy(ports = ports, body = body)
    }.toMap
    val circuit = shell.state.circuit.copy(modules = shell.state.circuit.modules.map(m => changed.getOrElse(m.name, m)) :+ ext)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(firrtl.passes.ToWorkingIR.run(circuit)))
    val resolved = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(resolved); firrtl.passes.CheckFlows.run(resolved)
    val state = shell.state.copy(circuit = resolved)
    val in = getClass.getResourceAsStream("/CheckpointNativeStateSnapshot.sv")
    check(in != null, "missing synthesized staging resource")
    val source = try new String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally in.close()
    val flags = List("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported", "staging_restore_supported", "target_time_authority_proven").map(_ -> JBool(false))
    val nativeBinding = if (!stageNativeState) Nil else List("native_state" -> JArray(nativeRows.toList),
      "original_live_bits" -> JInt(originalBits), "native_bits" -> JInt(bits - originalBits),
      "native_observation" -> observation,
      "derived_initialization" -> JString("explicit_derived_registers_on_bank_reset_implicit_ram_data_by_original_execution"),
      "reader_idle_contract" -> JString("all_native_phases_idle_and_no_pending_response"))
    val binding = canonical(JObject(List("snapshot_version" -> JInt(if (stageNativeState) 2 else 1),
      "scope" -> JString(if (stageNativeState) "live_shell_and_native_reader_atomic_observation_only" else
        "selected_original_live_shell_register_atomic_observation_only"),
      "shell_binding_sha256" -> JString(sha256(shell.binding)),
      "wired_sha256" -> JString(sha256(state.circuit.serialize)),
      "wired_annotations_sha256" -> JString(sha256(firrtl.annotations.JsonProtocol.serialize(state.annotations))),
      "source_sha256" -> JString(sha256(source)), "selected_execution" -> JString(path),
      "raw_clock" -> JString(path + "." + rawClock), "sideband_prefix" -> JString(prefix),
      "enclosing_raw_clock" -> JString(shell.state.circuit.main + "." + enclosingRaw),
      "bits" -> JInt(bits), "words" -> JInt((bits.toLong + 63) / 64), "state" -> JArray(rows),
      "staging_owner" -> JString(path + "." + instance), "staging_owner_bits" -> JInt(bits + 71),
      "sampling_contract" -> JString("one_raw_preedge_after_caller_certified_freeze_before_first_bank_handshake"),
      "local_staging_restore_enabled" -> JBool(enableRestore),
      "local_staging_restore_scope" -> JString("complete_payload_occupied_cut_cycle_and_sticky_error_only"),
      "local_staging_restore_bits" -> JInt(bits + 71),
      "local_staging_restore_extra_storage_bits" -> JInt(0),
      "local_staging_restore_hold_contract" -> JString("hold_parks_functional_bank_and_revokes_borrow_before_load"),
      "original_live_and_native_restore_supported" -> JBool(false),
      "bank_clock_contract" -> JString("independent_access_clock_removed_endpoint_tied_to_original_live_raw_clock"),
      "hold_contract" -> JString("request_or_occupied_or_retained_error_without_live_fault_feedback"),
      "unowned_native_state" -> (parse(shell.banks.binding) \ "native_state_observation"),
      "original_unsupported_live_owners" -> JArray(owners)) ++ nativeBinding ++ flags))
    Result(state, shell, binding, stageNativeState, enableRestore)
  }

  def validate(result: Result): Unit = {
    val expected = generate(result.shell, result.stageNativeState, result.enableRestore)
    check(result.state == expected.state && result.binding == expected.binding,
      "changed live owners, packing, read permission, raw clock, staging or binding")
  }
}
