// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, WDefInstance, WRef, WSubField}
import firrtl.ir._
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Attach the immutable staging copy to the SAME bank bus as original storage.
  * Descriptor sizing is explicit; this does not resize the reserved widget or
  * remove its production atomic-owner refusal. All new live state is exposed
  * as a separate unsupported owner, never included in its own staged payload.
  */
object NativeCheckpointLiveShellBankReadout {
  import SemanticStateInventory.{canonical, sha256}
  val definition = "CheckpointLiveShellSnapshotReadout"
  private val prefix = "checkpoint_live_readout_"
  case class Result(state: CircuitState, snapshot: NativeCheckpointLiveShellSnapshot.Result,
      entry: StateAccessEntry, binding: String, files: Seq[GoldenGateOutputFileAnnotation])
  private def check(ok: Boolean, why: String): Unit = require(ok, "live shell bank readout: " + why)
  private def statements(s: Statement): Seq[Statement] = {
    val xs = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { xs += x; x.foreachStmt(visit) }
    visit(s); xs.toSeq
  }
  private def resource(name: String): String = {
    val s = scala.io.Source.fromResource(name); try s.mkString finally s.close()
  }
  def generate(snapshot: NativeCheckpointLiveShellSnapshot.Result): Result = {
    NativeCheckpointLiveShellSnapshot.validate(snapshot)
    val doc = parse(snapshot.binding)
    val JString(selectedPath) = doc \ "selected_execution"
    val JString(rawClock) = doc \ "raw_clock"
    val JString(enclosingClock) = doc \ "enclosing_raw_clock"
    val JInt(bits) = doc \ "bits"
    val p = snapshot.shell.sidebandPrefix
    val sp = p + "live_snapshot_"
    val rootName = snapshot.state.circuit.main
    val executions = GeneratedExternalOwners.instances(snapshot.state, rootName).toMap
    val parts = selectedPath.split("\\.")
    val chain = parts.indices.map(i => executions(parts.take(i + 1).mkString(".")).asInstanceOf[Module])
    val bank = snapshot.shell.entries.map(_.bank).max + 1
    check(bank < 255 && snapshot.shell.entries.forall(_.bank != 255), "no unused functional bank selector")
    check(snapshot.shell.entries.size < 65535, "appended descriptor exceeds transport ABI")
    val entry = StateAccessEntry(sha256(snapshot.binding + "\nimmutable_live_shell_native_payload_lsb0"),
      bank, 0, 64, ((bits + 63) / 64).toInt)
    check(!snapshot.state.circuit.modules.exists(m => m.name == definition || (m match {
      case e: ExtModule => e.defname == definition; case _ => false
    })), "readout definition collision")
    chain.foreach(m => check(!(m.ports.map(_.name) ++ statements(m.body).collect {
      case d: IsDeclaration => d.name
    }).exists(_.startsWith(prefix)), "reserved identifier collision"))
    val bus = Seq("req_valid" -> (Input, 1), "req_ready" -> (Output, 1),
      "bank" -> (Input, 8), "slot" -> (Input, 16), "address" -> (Input, 32),
      "resp_valid" -> (Output, 1), "resp_ready" -> (Input, 1),
      "resp_data" -> (Output, 64), "resp_error" -> (Output, 1))
    val roles = Map("req_valid" -> "request_valid", "req_ready" -> "request_ready",
      "resp_valid" -> "response_valid", "resp_ready" -> "response_ready",
      "resp_data" -> "response_data", "resp_error" -> "response_error",
      "bank" -> "bank", "slot" -> "slot", "address" -> "address")
    def port(n: String, d: Direction, w: BigInt): Port = Port(NoInfo, n, d, UIntType(IntWidth(w)))
    val ext = ExtModule(NoInfo, definition, Seq(Port(NoInfo, "clock", Input, ClockType),
      port("reset", Input, 1), port("permission", Input, 1), port("snapshot_state", Input, bits),
      port("idle", Output, 1), port("control_state", Output, 69)) ++ bus.flatMap { case (n,(d,w)) =>
      Seq(port(roles(n), d, w), port("original_" + roles(n), if (d == Input) Output else Input, w))
    }, definition, Seq(IntParam("BITS", bits), IntParam("SNAPSHOT_BANK", bank)))
    val instance = prefix + "adapter"
    val idlePort = port(prefix + "idle", Input, 1)
    val attemptPort = port(prefix + "attempt", Input, 1)
    val busNames = bus.map { case (n,_) => (p + "ckpt_" + n) -> (prefix + "inner_" + n) }.toMap
    val changed = chain.zipWithIndex.map { case (m,i) =>
      def rewrite(e: Expression): Expression = e match {
        case r: WRef if i == 0 && busNames.contains(r.name) => r.copy(name = busNames(r.name))
        case x => x.mapExpr(rewrite)
      }
      def statement(s: Statement): Statement = s match {
        case c: Connect if i == chain.size - 1 && c.loc.serialize == sp + "adapter.reader_idle" =>
          c.copy(expr = DoPrim(firrtl.PrimOps.And, Seq(c.expr, WRef(idlePort.name, idlePort.tpe)), Nil, idlePort.tpe))
        case c: Connect if i == chain.size - 1 && c.loc.serialize == sp + "adapter.borrow_attempt" =>
          c.copy(expr = WRef(attemptPort.name, attemptPort.tpe))
        case x => x.mapStmt(statement).mapExpr(rewrite)
      }
      val body = statement(m.body)
      val routes = if (i == chain.size - 1) Nil else Seq(idlePort, attemptPort).map { q =>
        Connect(NoInfo, WSubField(WRef(parts(i + 1)), q.name, q.tpe), WRef(q.name, q.tpe))
      }
      val extra = if (i != 0) routes else {
        def pin(n: String): Expression = WSubField(WRef(instance), n)
        val wires = bus.map { case (n,(_,w)) => DefWire(NoInfo, prefix + "inner_" + n, UIntType(IntWidth(w))) }
        val connectBus = bus.flatMap { case (n,(d,w)) =>
          val outer = WRef(p + "ckpt_" + n, UIntType(IntWidth(w)))
          val inner = WRef(prefix + "inner_" + n, UIntType(IntWidth(w)))
          if (d == Input) Seq(Connect(NoInfo, pin(roles(n)), outer), Connect(NoInfo, inner, pin("original_" + roles(n))))
          else Seq(Connect(NoInfo, outer, pin(roles(n))), Connect(NoInfo, pin("original_" + roles(n)), inner))
        }
        wires ++ Seq(WDefInstance(instance, definition),
          Connect(NoInfo, pin("clock"), WRef(enclosingClock.stripPrefix(rootName + "."), ClockType)),
          Connect(NoInfo, pin("reset"), WRef(p + "ckpt_reset")),
          Connect(NoInfo, pin("permission"), WRef(sp + "borrow_enable")),
          Connect(NoInfo, pin("snapshot_state"), DoPrim(firrtl.PrimOps.Bits, Seq(WRef(sp + "control_state")),
            Seq(bits - 1, BigInt(0)), UIntType(IntWidth(bits)))),
          Connect(NoInfo, WRef(idlePort.name), pin("idle")),
          Connect(NoInfo, WRef(attemptPort.name), WRef(p + "ckpt_req_valid")),
          Connect(NoInfo, WRef(prefix + "control_state"), pin("control_state"))) ++ connectBus ++ routes
      }
      val ports = if (i == 0) m.ports :+ port(prefix + "control_state", Output, 69)
        else m.ports ++ Seq(idlePort, attemptPort)
      val local = if (i == 0) Seq(DefWire(NoInfo, idlePort.name, idlePort.tpe),
        DefWire(NoInfo, attemptPort.name, attemptPort.tpe)) else Nil
      val (declarations, connections) = extra.partition(_.isInstanceOf[IsDeclaration])
      m.name -> m.copy(ports = ports, body = Block(local ++ declarations ++ Seq(body) ++ connections))
    }.toMap
    val circuit = snapshot.state.circuit.copy(modules = snapshot.state.circuit.modules.map(m =>
      changed.getOrElse(m.name, m)) :+ ext)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(firrtl.passes.ToWorkingIR.run(circuit)))
    val resolved = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(resolved); firrtl.passes.CheckFlows.run(resolved)
    val state = snapshot.state.copy(circuit = resolved)
    val files = Seq("CheckpointLiveShellSnapshotReadout.sv", "CheckpointSnapshotStateBank.sv")
      .map(n => GoldenGateOutputFileAnnotation(resource(n), ".live-shell-bank-" + n))
    val binding = canonical(JObject(List("readout_version" -> JInt(1),
      "scope" -> JString("selected_live_shell_staged_payload_bank_readout_only"),
      "snapshot_binding_sha256" -> JString(sha256(snapshot.binding)),
      "wired_sha256" -> JString(sha256(state.circuit.serialize)),
      "wired_annotations_sha256" -> JString(sha256(firrtl.annotations.JsonProtocol.serialize(state.annotations))),
      "raw_clock" -> JString(enclosingClock), "selected_raw_clock" -> JString(rawClock),
      "entry" -> JObject(List("state_id" -> JString(entry.stateID), "bank" -> JInt(bank),
        "slot" -> JInt(0), "width" -> JInt(64), "depth" -> JInt(entry.depth))),
      "payload_bits" -> JInt(bits), "payload" -> doc,
      "readout_owner" -> JString(rootName + "." + instance), "readout_owner_bits" -> JInt(69),
      "readout_owner_layout_lsb0" -> JArray(List(JString("pending"), JString("snapshot_owner"),
        JString("snapshot_bank_data_64_error_pending_failed"))),
      "readout_state_owner" -> JString("unsupported_live_readout_state_capture_and_restore"),
      "files" -> JObject(files.map(f => f.fileSuffix -> JString(sha256(f.body))).toList),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "metasim_restore_supported" -> JBool(false), "production_freeze_owners_bound" -> JBool(false))))
    Result(state, snapshot, entry, binding, files)
  }
  def validate(result: Result): Unit = {
    val expected = generate(result.snapshot)
    check(result == expected, "changed source, staged payload, bank routing, descriptor or new owner")
  }
}
