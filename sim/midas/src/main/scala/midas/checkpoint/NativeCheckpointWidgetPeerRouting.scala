// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, WRef, WSubField}
import firrtl.annotations.{JsonProtocol, TargetToken}
import firrtl.ir._
import midas.passes.{ResolveSimulationInline, SimulationInlineResultAnnotation}
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Follow the reserved endpoints through native widget inlining. This is a
  * routing contract for a future graft, not an external-state adapter or image
  * admission. Both phase inputs are required: the pre-inline inspector binds
  * the registered widget boundary and immutable transport sources. The retained
  * MMIO/DMA execution regressions remain separate requirements.
  */
object NativeCheckpointWidgetPeerRouting {
  import SemanticStateInventory.{canonical, sha256}
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "checkpoint widget peer routing: " + why)
  private def statements(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(s: Statement): Unit = { out += s; s.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  // Public physical-transport ABI names, never flattened widget name guesses.
  private val physicalPins = Map(
    "clock_mask" -> "clock_mask", "read_enable" -> "physical_read_enable",
    "captured_ce" -> "captured_ce", "logical_ce" -> "ce",
    "descriptor_entry" -> "descriptor_entry", "descriptor_valid" -> "descriptor_valid",
    "descriptor_bank" -> "descriptor_bank", "descriptor_slot" -> "descriptor_slot",
    "descriptor_width" -> "descriptor_width", "descriptor_depth" -> "descriptor_depth",
    "request_valid" -> "bank_request_valid", "request_ready" -> "bank_request_ready",
    "bank" -> "bank", "slot" -> "slot", "address" -> "address",
    "response_valid" -> "bank_response_valid", "response_ready" -> "bank_response_ready",
    "response_data" -> "bank_response_data", "response_error" -> "bank_response_error")

  def inspect(before: CircuitState, after: CircuitState): String = {
    val reservation = parse(NativeCheckpointWidgetReservation.inspect(before))
    check(!after.circuit.modules.exists {
      case e: ExtModule => e.defname == "CheckpointNativeCaptureTransport"
      case _ => false
    }, "installed native widget cannot use the physical deferred reservation/peer ABI")
    check(after.form == LowForm && after.circuit.main == before.circuit.main,
      "same resolved LowForm circuit required")
    val history = after.annotations.collect { case a: SimulationInlineResultAnnotation => a }
    check(history.size == 1, "one native inline history required")
    val context = ResolveSimulationInline.context(history.head)
    def bound(key: String, text: String): Unit =
      check((context \ key) == JString(sha256(text)), "stale inline " + key)
    bound("input_sha256", before.circuit.serialize)
    bound("output_sha256", after.circuit.serialize)
    bound("input_annotations_sha256", JsonProtocol.serialize(before.annotations))
    bound("output_annotations_sha256", JsonProtocol.serialize(after.annotations.filterNot(_ == history.head)))
    val executions = after.annotations.collect { case a: NativeCheckpointWidgetExecutionAnnotation => a }
    val original = before.annotations.collect { case a: NativeCheckpointWidgetExecutionAnnotation => a }.head
    check(executions.size == 1 && executions.head == original, "changed execution/owner/CE ordering")
    val a = executions.head
    val expected = NativeCheckpointInPlaceGraft.capturePorts(a.gates)
      .map(p => p.name.stripPrefix("capture_") -> p).toMap
    val pins = after.annotations.collect { case p: NativeCheckpointWidgetPinAnnotation => p }
    check(pins.size == expected.size && pins.map(_.role).toSet == expected.keySet &&
      pins.map(_.target).distinct.size == pins.size, "missing, duplicate or aliased peer endpoints")
    check(pins.toSet == before.annotations.collect {
      case p: NativeCheckpointWidgetPinAnnotation => p
    }.toSet, "changed original-host endpoint markers")
    val live = GeneratedExternalOwners.instances(after, after.circuit.main)
    val byPath = live.toMap
    def resolve(module: String, path: Seq[(TargetToken.Instance, TargetToken.OfModule)]): String = {
      val roots = live.filter(_._2.name == module)
      check(roots.size == 1, "ambiguous or stale target root " + module)
      var scope = roots.head._1
      path.foreach { case (i, m) =>
        scope += "." + i.value
        check(byPath.get(scope).exists(_.name == m.value), "stale peer instance path")
      }
      scope
    }
    val hosts = pins.map { p =>
      check(p.target.circuit == after.circuit.main && p.target.component.isEmpty,
        "foreign or indexed peer endpoint")
      resolve(p.target.module, p.target.path)
    }.distinct
    check(hosts.size == 1, "one concrete peer host required")
    val hostPath = hosts.head
    check((reservation \ "host") == JString(hostPath), "changed reservation host")
    val host = byPath(hostPath) match {
      case m: Module => m
      case _ => throw new IllegalArgumentException("checkpoint widget peer routing: opaque host")
    }
    val selected = resolve(a.target.module, a.target.asPath)
    check(selected.startsWith(hostPath + ".") && byPath(selected).isInstanceOf[Module],
      "selected execution outside peer host or opaque")
    val transports = live.collect {
      case (path, m: ExtModule) if m.defname == "CheckpointPhysicalCaptureTransport" => path -> m
    }
    check(transports.size == 1 && transports.head._1.split('.').dropRight(1).mkString(".") == hostPath,
      "one physical transport promoted into the peer host required")
    val (transportPath, transport) = transports.head
    check((reservation \ "transport_definition_sha256") == JString(sha256(transport.serialize)),
      "changed physical transport definition/ABI/parameters")
    def sources(s: CircuitState) = s.annotations.collect {
      case x: firrtl.transforms.BlackBoxInlineAnno if x.target.name == transport.name => x
    }
    check(sources(after).toSet == sources(before).toSet && sources(after).size == sources(before).size &&
      !after.annotations.exists {
        case x: firrtl.transforms.BlackBoxPathAnno => x.target.name == transport.name
        case x: firrtl.transforms.BlackBoxResourceAnno => x.target.name == transport.name
        case _ => false
      }, "changed or unresolved transport sources")
    val instance = transportPath.split('.').last
    val ss = statements(host.body)
    val types = (host.ports.map(p => p.name -> p.tpe) ++ ss.collect {
      case w: DefWire => w.name -> w.tpe
      case n: DefNode => n.name -> n.value.tpe
    }).toMap
    val driverRows = ss.collect {
      case n: DefNode => n.name -> n.value
      case c: Connect => c.loc.serialize -> c.expr
    }
    val drivers = driverRows.groupBy(_._1).map { case (n, xs) => n -> xs.map(_._2) }
    def driver(n: String): Expression = {
      check(drivers.get(n).exists(_.size == 1), "missing or multiple drivers at " + n)
      drivers(n).head
    }
    def field(n: String) = instance + "." + n
    // Stop at the annotated sink BEFORE chasing its zero placeholder. Equal
    // constants do not establish connectivity: swapped return pins must fail.
    def route(start: String, stop: String, casts: Boolean = false): Seq[String] = {
      val seen = scala.collection.mutable.LinkedHashSet.empty[String]
      def walk(n: String): Unit = {
        check(!seen(n), "alias cycle at " + n); seen += n
        if (n != stop) {
          def next(e: Expression): Unit = e match {
            case r: WRef => walk(r.name)
            case r: WSubField => walk(r.serialize)
            case DoPrim(firrtl.PrimOps.AsUInt, Seq(e), Nil, _) if casts => next(e)
            case _ => check(false, "non-alias or wrong endpoint from " + n + " to " + stop)
          }
          next(driver(n))
        }
      }
      walk(start); seen.toSeq
    }
    check(host.ports.exists(p => p.name == "clock" && p.tpe == ClockType && p.direction == Input),
      "missing raw host clock")
    check(host.ports.exists(p => p.name == "reset" && p.tpe == UIntType(IntWidth(1)) && p.direction == Input),
      "missing raw host reset")
    val rawRoute = route(field("clock"), "clock")
    val resetRoute = route(field("reset"), "reset", casts = true)
    val rows = pins.sortBy(_.role).map { p =>
      check(types.get(p.target.ref).contains(expected(p.role).tpe), "reshaped peer " + p.role)
      val source = expected(p.role).direction == Input
      val target = if (p.role == "reset") "reset" else field(physicalPins(p.role))
      val path = if (source) route(p.target.ref, target, casts = p.role == "reset")
        else route(target, p.target.ref)
      if (!source) {
        val seen = scala.collection.mutable.Set.empty[String]
        def zero(n: String): Boolean = {
          check(!seen(n), "placeholder cycle at " + n); seen += n
          driver(n) match {
            case UIntLiteral(v, w) => v == 0 && w == expected(p.role).tpe.asInstanceOf[UIntType].width
            case r: WRef => zero(r.name)
            case _ => false
          }
        }
        check(zero(p.target.ref), "changed peer placeholder " + p.role)
      }
      JObject(List("role" -> JString(p.role), "target" -> JString(p.target.serialize),
        "direction" -> JString(if (source) "from_transport" else "to_transport"),
        "physical_pin" -> JString(target), "route" -> JArray(path.map(JString(_)).toList)))
    }
    canonical(JObject(List("peer_routing_version" -> JInt(1),
      "scope" -> JString("native_inlined_reserved_widget_peer_routes_only"),
      "reservation_sha256" -> JString(sha256(canonical(reservation))),
      "inline_history_sha256" -> JString(sha256(history.head.body)),
      "source_sha256" -> JString(sha256(after.circuit.serialize)),
      "annotations_sha256" -> JString(sha256(JsonProtocol.serialize(after.annotations))),
      "host" -> JString(hostPath), "target" -> JString(a.target.serialize),
      "transport" -> JString(transportPath), "pins" -> JArray(rows.toList),
      "raw_clock_route" -> JArray(rawRoute.map(JString(_)).toList),
      "reset_route" -> JArray(resetRoute.map(JString(_)).toList),
      "external_transport_state_owner" -> JString("unsupported_no_state_adapter"),
      "in_place_checkpoint_widget_connected" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "complete_fpga_capture" -> JBool(false),
      "deployable_capture_image" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
  }
}
