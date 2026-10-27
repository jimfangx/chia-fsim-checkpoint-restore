// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, WRef, WSubField}
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Connect disjoint native executions to the ONE existing reserved widget.
  * The shared adapter remains at the original root. Its control/readout wires
  * travel to the actual peer host without duplicating functional execution.
  * This supplies neither a common-cut owner nor production image admission.
  */
object NativeCheckpointRegionWidgetGraft {
  import SemanticStateInventory.{canonical, sha256}
  case class Result(state: CircuitState, graft: NativeCheckpointRegionGraft.Result, binding: String,
      annotationProjection: String)
  private val prefix = "checkpoint_region_widget_"
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "checkpoint region widget graft: " + why)
  private def declarations(s: Statement): Seq[String] = {
    val names = scala.collection.mutable.ArrayBuffer.empty[String]
    def visit(x: Statement): Unit = { x match { case d: IsDeclaration => names += d.name; case _ => () }; x.foreachStmt(visit) }
    visit(s); names.toSeq
  }

  def connect(before: CircuitState, after: CircuitState, source: NativeCheckpointRegions.Result): Result = {
    val peerText = NativeCheckpointWidgetPeerRouting.inspect(before, after)
    check(source.host.form == LowForm && source.host.circuit == after.circuit,
      "original region host differs from actual lowered host")
    check(after.annotations.count(_ == firrtl.transforms.NoCircuitDedupAnnotation) == 1,
      "source NoCircuitDedup policy absent or ambiguous in actual host")
    check(source.inventory.nonEmpty, "explicit actual-widget external inventory permission required")
    source.inventory.get.validate(before, after)
    source.inventory.get.validateSource(source.host)
    check(source.inventory.get.unsupportedOwners.arr.nonEmpty, "missing unsupported transport ownership deficit")
    val graft = NativeCheckpointRegionGraft.generate(source)
    val execution = after.annotations.collect { case a: NativeCheckpointWidgetExecutionAnnotation => a }.head
    check(execution.entries == source.descriptors.entries.size && execution.gates == source.descriptors.physicalClockGates,
      "reserved entry/gate count differs from reconstructed descriptors")
    val JString(hostPath) = parse(peerText) \ "host"
    val live = GeneratedExternalOwners.instances(after, after.circuit.main)
    val originalModules = after.circuit.modules.map(m => m.name -> m).toMap
    val paths = hostPath.split('.').indices.map(i => hostPath.split('.').take(i + 1).mkString(".")).toVector
    check(paths.head == after.circuit.main, "peer host outside original root")
    val byPath = live.toMap
    val chain = paths.map { path =>
      val module = byPath(path).asInstanceOf[Module]
      check(live.count(_._2.name == module.name) == 1, "routing ancestor has shared execution: " + module.name)
      check(!(module.ports.map(_.name) ++ declarations(module.body)).exists(_.startsWith(prefix)),
        "reserved route identifier collision")
      module
    }
    val ports = NativeCheckpointInPlaceGraft.capturePorts(execution.gates)
    val pins = after.annotations.collect { case a: NativeCheckpointWidgetPinAnnotation => a.role -> a.target.ref }.toMap
    val emittedExecutions = GeneratedExternalOwners.instances(graft.state, graft.state.circuit.main)
      .groupBy(_._2.name).view.mapValues(_.map(_._1).toSet).toMap
    val changedExecutions = live.groupBy(_._2.name).collect { case (name, executions)
      if emittedExecutions.getOrElse(name, Set.empty[String]) != executions.map(_._1).toSet => name }.toSet
    val changedNames = graft.state.circuit.modules.filter(m => originalModules.get(m.name).exists(_ != m)).map(_.name).toSet ++
      chain.map(_.name) ++ changedExecutions
    // Typed storage contracts are projected below. Other targeted metadata on
    // modified definitions or changed execution sets needs its own contract.
    // An unchanged shared definition may lose all executions to specialized
    // banks, or gain mask-gate executions that would broaden a local contract.
    val rawClockSources = after.annotations.collect { case a: midas.passes.HostClockSource => a }
    check(rawClockSources.size == 1 && rawClockSources.head.target.circuit == after.circuit.main &&
      rawClockSources.head.target.module == chain.last.name && rawClockSources.head.target.path.isEmpty &&
      rawClockSources.head.target.component.isEmpty && rawClockSources.head.target.ref == "clock" &&
      chain.last.ports.exists(p => p.name == "clock" && p.direction == Input && p.tpe == ClockType),
      "one unchanged peer-host raw clock source required")
    after.annotations.filterNot(a => GeneratedStateBankAnnotations.supported(a) ||
      a.isInstanceOf[midas.passes.HostClockSource] ||
      a.isInstanceOf[NativeCheckpointWidgetPinAnnotation] || a.isInstanceOf[NativeCheckpointWidgetExecutionAnnotation])
      .foreach(a => check(!a.getTargets.exists(t => changedNames.exists(n =>
        ("(?:\\||:)" + java.util.regex.Pattern.quote(n) + "(?=[/>]|$)").r.findFirstIn(t.serialize).nonEmpty)),
        "unknown host annotation targets changed definition: " + a.getClass.getName))
    val rows = scala.collection.mutable.ArrayBuffer.empty[JValue]
    val graftModules = graft.state.circuit.modules.map(m => m.name -> m).toMap
    val routed = chain.zipWithIndex.map { case (original, index) =>
      val module = graftModules(original.name).asInstanceOf[Module]
      val root = index == 0
      val peer = index == chain.size - 1
      def signal(p: Port): Expression = WRef(if (root) p.name else prefix + p.name.stripPrefix("capture_"), p.tpe)
      val outputRoles = ports.filter(_.direction == Output).map(_.name.stripPrefix("capture_")).toSet
      val counts = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
      val roleByName = outputRoles.map(r => pins(r) -> r).toMap
      def replace(s: Statement): Statement = s match {
        case n: DefNode if peer && roleByName.contains(n.name) =>
          val role = roleByName(n.name); counts(role) += 1
          n.copy(value = signal(ports.find(_.name == "capture_" + role).get))
        case c: Connect if peer && roleByName.contains(c.loc.serialize) =>
          val role = roleByName(c.loc.serialize); counts(role) += 1
          c.copy(expr = signal(ports.find(_.name == "capture_" + role).get))
        case s => s.mapStmt(replace)
      }
      val body = replace(module.body)
      if (peer) check(outputRoles.forall(counts(_) == 1), "return peer placeholder must have one driver")
      val extra = ports.map { p =>
        val role = p.name.stripPrefix("capture_")
        if (peer) {
          if (p.direction == Input) Connect(NoInfo, signal(p), WRef(pins(role), p.tpe)) else EmptyStmt
        } else {
          val child = paths(index + 1).split('.').last
          val pin = WSubField(WRef(child), prefix + role, p.tpe)
          if (p.direction == Input) Connect(NoInfo, signal(p), pin) else Connect(NoInfo, pin, signal(p))
        }
      }
      rows += JObject(List("path" -> JString(paths(index)), "module" -> JString(module.name),
        "peer_host" -> JBool(peer)))
      val nextPorts = if (root) original.ports else module.ports ++ ports.map(p =>
        p.copy(name = prefix + p.name.stripPrefix("capture_"), direction = if (p.direction == Input) Output else Input))
      val wires = if (root) ports.map(p => DefWire(NoInfo, p.name, p.tpe)) else Nil
      module.name -> module.copy(ports = nextPorts, body = Block(wires ++ Seq(body) ++ extra))
    }.toMap
    val circuit = graft.state.circuit.copy(modules = graft.state.circuit.modules.map(m => routed.getOrElse(m.name, m)))
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(firrtl.passes.ToWorkingIR.run(circuit)))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    val projected = NativeCheckpointWidgetAnnotations.project(after, after.copy(circuit = flowed))
    val sourceContext = source.host.annotations.filter(SimulatorBankProbeContext.accepts)
    val graftContext = graft.state.annotations.filter(SimulatorBankProbeContext.accepts)
    check(graftContext.toVector == sourceContext.toVector, "generated compiler context changed")
    val generated = graft.state.annotations.filterNot(a => a == firrtl.transforms.NoCircuitDedupAnnotation ||
      SimulatorBankProbeContext.accepts(a))
    check(generated.forall(GeneratedStateBankAnnotations.supported), "unsupported generated storage annotation")
    val result = projected.state.copy(annotations = projected.state.annotations ++
      generated.filterNot(projected.state.annotations.contains))
    val storage = result.copy(annotations = result.annotations.filter(GeneratedStateBankAnnotations.supported))
    val identity = GeneratedExternalOwners.instances(result, result.circuit.main).collect { case (path, m: Module) =>
      GeneratedStateBankAnnotations.Clone(m.name, path, m.name, m)
    }
    val (validated, _) = GeneratedStateBankAnnotations.translate(storage, identity)
    check(validated.toSet == storage.annotations.toSet, "combined storage annotation targets changed")
    val binding = canonical(JObject(List(
      "connection_version" -> JInt(1), "scope" -> JString("reserved_widget_to_existing_disjoint_regions_routes_only"),
      "source_sha256" -> JString(sha256(after.circuit.serialize)),
      "regions_binding_sha256" -> JString(sha256(source.binding)),
      "graft_binding_sha256" -> JString(sha256(graft.binding)),
      "peer_routing_sha256" -> JString(sha256(peerText)),
      "annotation_projection_sha256" -> JString(sha256(projected.binding)),
      "connected_sha256" -> JString(sha256(result.circuit.serialize)),
      "descriptor_manifest_sha256" -> JString(source.descriptors.manifestSha256),
      "descriptor_binding_sha256" -> JString(source.descriptors.bindingSha256),
      "unsupported_external_owners" -> source.inventory.get.unsupportedOwners,
      "reserved_inventory_binding_sha256" -> JString(sha256(source.inventory.get.binding)),
      "host" -> JString(hostPath), "routes" -> JArray(rows.toList),
      "pins" -> JArray(ports.map(p => JObject(List("role" -> JString(p.name.stripPrefix("capture_")),
        "peer" -> JString(pins(p.name.stripPrefix("capture_"))), "type" -> JString(p.tpe.serialize)))).toList),
      "in_place_checkpoint_widget_connected" -> JBool(true), "descriptor_identity_bound" -> JBool(false),
      "external_transport_state_owner" -> JString("unsupported_no_atomic_capture_restore_adapter"),
      "production_freeze_owners_bound" -> JBool(false), "complete_fpga_capture" -> JBool(false),
      "deployable_capture_image" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    Result(result, graft, binding, projected.binding)
  }
}
