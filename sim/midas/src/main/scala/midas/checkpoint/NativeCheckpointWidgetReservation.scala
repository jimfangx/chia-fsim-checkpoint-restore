// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, WDefInstance}
import firrtl.annotations.{InstanceTarget, ReferenceTarget, SingleTargetAnnotation}
import firrtl.ir._
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._

case class NativeCheckpointWidgetPinAnnotation(target: ReferenceTarget, role: String)
    extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget) = copy(target = n)
}
case class NativeCheckpointWidgetExecutionAnnotation(target: InstanceTarget, entries: Int,
    gates: Int, owners: Seq[String], queueDepth: Int) extends SingleTargetAnnotation[InstanceTarget] {
  def duplicate(n: InstanceTarget) = copy(target = n)
}

/** A phase boundary, never a state adapter. The actual transport owns sequential
  * state outside FIRRTL. Reserving it must not make that state disappear from
  * inventory or become an acknowledgement that its owner can freeze.
  */
object NativeCheckpointWidgetReservation {
  import SemanticStateInventory.{canonical, sha256}
  // Deliberately unbound. Both the exact parameter and #error header must be
  // consumed by a future checked binding before final image emission.
  val digest = "f" * 64
  def header(base: BigInt, stream: Int): String =
    s"// CHECKPOINT_WIDGET_RESERVATION_BEGIN base=$base stream=$stream\n" +
      "#error Unbound_checkpoint_widget_reservation\n// CHECKPOINT_WIDGET_RESERVATION_END\n"

  private def check(ok: Boolean, why: String): Unit =
    require(ok, "checkpoint widget reservation: " + why)
  private def statements(s: Statement): Seq[Statement] = {
    val all = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(s: Statement): Unit = { all += s; s.foreachStmt(visit) }
    visit(s); all.toSeq
  }
  private def resource(name: String): String = {
    val in = getClass.getResourceAsStream("/" + name)
    check(in != null, "missing transport resource " + name)
    try new String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally in.close()
  }
  private val widgetPins = Map(
    "reset" -> "controllerReset", "clock_mask" -> "physical_clockMask",
    "read_enable" -> "physical_readEnable", "captured_ce" -> "physical_capturedCE",
    "logical_ce" -> "physical_logicalCE", "descriptor_entry" -> "descriptorEntry",
    "descriptor_valid" -> "descriptorValid", "descriptor_bank" -> "descriptorBank",
    "descriptor_slot" -> "descriptorSlot", "descriptor_width" -> "descriptorWidth",
    "descriptor_depth" -> "descriptorDepth", "request_valid" -> "bankRequestValid",
    "request_ready" -> "bankRequestReady", "bank" -> "bank", "slot" -> "slot",
    "address" -> "address", "response_valid" -> "bankResponseValid",
    "response_ready" -> "bankResponseReady", "response_data" -> "bankResponseData",
    "response_error" -> "bankResponseError")

  /** Validate actual lowered pins, not names guessed from emitted Verilog.
    * This receipt intentionally retains unsupported transport/owner coverage.
    * Nothing here strips the markers, rewrites identity, or grants admission.
    */
  def inspect(state: CircuitState): String = {
    check(state.form == LowForm, "resolved LowForm required")
    val executions = state.annotations.collect { case a: NativeCheckpointWidgetExecutionAnnotation => a }
    val pins = state.annotations.collect { case a: NativeCheckpointWidgetPinAnnotation => a }
    check(executions.size == 1, "one explicit existing execution required")
    val a = executions.head
    midas.widgets.CheckpointCaptureWidgetParams(a.entries, digest, a.owners, a.queueDepth,
      physicalClockGates = a.gates, deferredGraft = true)
    val expected = NativeCheckpointInPlaceGraft.capturePorts(a.gates).map(p => p.name.stripPrefix("capture_") -> p).toMap
    check(pins.size == expected.size && pins.map(_.role).toSet == expected.keySet &&
      pins.map(_.target).distinct.size == pins.size, "missing, duplicate or aliased capture pins")
    val live = GeneratedExternalOwners.instances(state, state.circuit.main)
    val byPath = live.toMap
    def resolve(module: String, path: Seq[(firrtl.annotations.TargetToken.Instance,
        firrtl.annotations.TargetToken.OfModule)]): String = {
      val roots = live.filter(_._2.name == module)
      check(roots.size == 1, "ambiguous or stale target root " + module)
      var scope = roots.head._1
      path.foreach { case (i, m) =>
        scope += "." + i.value
        check(byPath.get(scope).exists(_.name == m.value), "stale target instance path")
      }
      scope
    }
    check(a.target.circuit == state.circuit.main && a.target.asPath.nonEmpty,
      "selected execution must be concrete")
    val selected = resolve(a.target.module, a.target.asPath)
    check(byPath(selected).isInstanceOf[Module], "selected execution is opaque; link it before inspection")
    val hostPaths = pins.map { pin =>
      check(pin.target.circuit == state.circuit.main && pin.target.component.isEmpty,
        "foreign or indexed capture wire")
      resolve(pin.target.module, pin.target.path)
    }.distinct
    check(hostPaths.size == 1, "capture pins require one concrete host")
    val hostPath = hostPaths.head
    val owner = byPath(hostPath) match {
      case m: Module => m
      case _ => throw new IllegalArgumentException("checkpoint widget reservation: opaque capture host")
    }
    check(selected.startsWith(hostPath + "."), "selected execution is outside capture host")
    val ss = statements(owner.body)
    val wireTypes = ss.collect {
      case w: DefWire => w.name -> w.tpe
      case n: DefNode => n.name -> n.value.tpe
    }.toMap
    val nodes = ss.collect { case n: DefNode => n.name -> n.value }.toMap
    val connects = ss.collect { case c: Connect => c }
    val widgetInstances = ss.collect { case i: WDefInstance
      if state.circuit.modules.exists(m => m.name == i.module && m.name == "CheckpointCaptureWidget") => i }
    check(widgetInstances.size == 1, "one registered checkpoint widget required")
    val widget = widgetInstances.head
    val widgetPath = hostPath + "." + widget.name
    val widgets = live.filter(_._2.name == "CheckpointCaptureWidget")
    check(widgets.map(_._1) == Seq(widgetPath), "duplicate capture widget")
    val rows = pins.sortBy(_.role).map { pin =>
      val t = pin.target
      check(t.circuit == state.circuit.main && t.component.isEmpty &&
        wireTypes.get(t.ref).contains(expected(pin.role).tpe), "stale or reshaped capture wire " + pin.role)
      val source = expected(pin.role).direction == Input
      val widgetPin = widget.name + ".capture_" + widgetPins(pin.role)
      val drivers = connects.filter(_.loc.serialize == t.ref).map(_.expr) ++ nodes.get(t.ref).toSeq
      check(drivers.size == 1, "undriven or multiply driven reservation " + pin.role)
      if (source) check(drivers.head.serialize == widgetPin,
        "changed widget source " + pin.role)
      else {
        def zero(e: Expression, seen: Set[String]): Boolean = e match {
          case UIntLiteral(v, w) => v == 0 && w == expected(pin.role).tpe.asInstanceOf[UIntType].width
          case r: firrtl.WRef if nodes.contains(r.name) && !seen(r.name) => zero(nodes(r.name), seen + r.name)
          case _ => false
        }
        check(zero(drivers.head, Set(t.ref)),
          "changed reservation placeholder " + pin.role)
        check(connects.filter(_.loc.serialize == widgetPin).map(_.expr.serialize) == Seq(t.ref),
          "changed widget sink " + pin.role)
      }
      JObject(List("role" -> JString(pin.role), "target" -> JString(t.serialize),
        "type" -> JString(expected(pin.role).tpe.serialize), "widget_pin" -> JString(widgetPin),
        "source" -> JString(if (source) "widget" else "unbound_zero_placeholder")))
    }
    val transports = live.collect { case (p, m: ExtModule) if m.defname == "CheckpointPhysicalCaptureTransport" => p -> m }
    check(transports.size == 1 && transports.head._1.startsWith(widgetPath + "."),
      "one actual widget physical transport required")
    val (transportPath, transport) = transports.head
    val resources = Seq("CheckpointPhysicalCaptureTransport.sv", "CheckpointPhysicalClockCut.sv",
      "CheckpointCaptureTransport.sv", "CheckpointCaptureCut.sv")
    val inline = state.annotations.collect { case a: firrtl.transforms.BlackBoxInlineAnno
      if a.target.name == transport.name => a }
    check(inline.size == resources.size && inline.map(_.name).toSet == resources.toSet &&
      inline.forall(a => a.target.circuit.name == state.circuit.main && a.text == resource(a.name)) &&
      !state.annotations.exists {
        case a: firrtl.transforms.BlackBoxPathAnno => a.target.name == transport.name
        case a: firrtl.transforms.BlackBoxResourceAnno => a.target.name == transport.name
        case _ => false
      }, "unresolved, duplicated or changed physical transport sources")
    val params = transport.params.collect { case IntParam(n, v) => n -> v }.toMap
    check(transport.params.size == 5 && params == Map("GATES" -> BigInt(a.gates),
      "OWNERS" -> BigInt(a.owners.size), "ENTRIES" -> BigInt(a.entries),
      "EXTERNAL_CLOCK_NETWORK" -> BigInt(1), "MANIFEST_SHA256" -> BigInt(digest, 16)),
      "changed physical transport reservation parameters")
    // Check exact flattened public ABI against the same Chisel bundle that
    // creates this blackbox. Opaque state remains unsupported, even with ABI.
    def abi(names: Seq[(String, Int)], direction: Direction): Seq[Port] =
      names.map { case (n, w) => Port(NoInfo, n, direction, UIntType(IntWidth(w))) }
    val expectedTransport = Seq(Port(NoInfo, "clock", Input, ClockType),
      Port(NoInfo, "root_clock", Output, ClockType)) ++ abi(Seq(
      "reset" -> 1, "target_cycle" -> 64, "owner_present" -> a.owners.size,
      "owner_frozen" -> a.owners.size, "owner_error" -> a.owners.size,
      "mmio_valid" -> 1, "mmio_write" -> 1, "mmio_address" -> 8, "mmio_wdata" -> 32,
      "mmio_response_ready" -> 1, "descriptor_valid" -> 1, "descriptor_bank" -> 8,
      "descriptor_slot" -> 16, "descriptor_width" -> 16, "descriptor_depth" -> 32,
      "bank_request_ready" -> 1, "bank_response_valid" -> 1, "bank_response_data" -> 64,
      "bank_response_error" -> 1, "dma_ready" -> 1, "ce" -> a.gates), Input) ++ abi(Seq(
      "freeze" -> 1, "mmio_ready" -> 1, "mmio_response_valid" -> 1, "mmio_rdata" -> 32,
      "mmio_response_error" -> 1, "descriptor_entry" -> 32, "bank_request_valid" -> 1,
      "bank" -> 8, "slot" -> 16, "address" -> 32, "bank_response_ready" -> 1,
      "dma_valid" -> 1, "dma_data" -> 512, "dma_last" -> 1, "dma_abort" -> 1,
      "gate_clock" -> a.gates, "clock_mask" -> 1, "captured_ce" -> a.gates,
      "physical_stopped" -> 1, "physical_read_enable" -> 1, "physical_failed" -> 1,
      "physical_cut_cycle" -> 64, "physical_error" -> 3), Output)
    check(transport.ports.map(_.copy(info = NoInfo)).toSet == expectedTransport.toSet &&
      transport.ports.size == expectedTransport.size, "changed physical transport ABI")
    val raw = connects.filter(_.loc.serialize == widget.name + ".clock")
    check(raw.size == 1 && raw.head.expr.serialize == "clock" &&
      owner.ports.exists(p => p.name == "clock" && p.tpe == ClockType && p.direction == Input),
      "widget lost host raw clock")
    canonical(JObject(List("reservation_version" -> JInt(1),
      "scope" -> JString("registered_widget_resources_and_unbound_lowered_readout_pins_only"),
      "source_sha256" -> JString(sha256(state.circuit.serialize)),
      "annotations_sha256" -> JString(sha256(firrtl.annotations.JsonProtocol.serialize(state.annotations))),
      "target" -> JString(a.target.serialize), "host" -> JString(hostPath), "widget" -> JString(widgetPath),
      "entries" -> JInt(a.entries), "gates" -> JInt(a.gates), "queue_depth" -> JInt(a.queueDepth),
      "owners" -> JArray(a.owners.map(JString(_)).toList), "pins" -> JArray(rows.toList),
      "transport" -> JString(transportPath), "transport_definition_sha256" -> JString(sha256(transport.serialize)),
      "transport_sources" -> JObject(Seq("CheckpointPhysicalCaptureTransport.sv", "CheckpointPhysicalClockCut.sv",
        "CheckpointCaptureTransport.sv", "CheckpointCaptureCut.sv").map(n => n -> JString(sha256(resource(n)))).toList),
      "external_transport_state_owner" -> JString("unsupported_no_state_adapter"),
      "in_place_checkpoint_widget_connected" -> JBool(false), "production_freeze_owners_bound" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "metasim_restore_supported" -> JBool(false))))
  }

  /** Fail even if someone deletes a marker annotation or bypasses the normal
    * reservation API. An unbound digest/header can never become a real image.
    */
  def requireBound(state: CircuitState): Unit = {
    val markers = state.annotations.exists {
      case _: NativeCheckpointWidgetExecutionAnnotation | _: NativeCheckpointWidgetPinAnnotation => true
      case a: GoldenGateOutputFileAnnotation => a.body.contains("Unbound_checkpoint_widget_reservation")
      case _ => false
    }
    val sentinel = state.circuit.modules.exists {
      case m: ExtModule if m.defname == "CheckpointPhysicalCaptureTransport" =>
        m.params.contains(IntParam("MANIFEST_SHA256", BigInt(digest, 16)))
      case _ => false
    }
    check(!markers && !sentinel,
      "unbound lowered readout; immutable descriptor/driver binding and external transport state owner required before image emission")
  }
}
