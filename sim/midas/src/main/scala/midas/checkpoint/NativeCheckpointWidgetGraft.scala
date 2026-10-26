// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, WRef, WSubField}
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Connect the real reserved widget to a reconstructed graft in its ONE
  * existing selected execution. This bounded connector accepts no source
  * annotation policy beyond NoCircuitDedup: production annotation projection,
  * immutable identity and complete ownership remain mandatory later work.
  * Reservation markers, sentinel identity and image guards are retained.
  */
object NativeCheckpointWidgetGraft {
  import SemanticStateInventory.{canonical, sha256}
  case class Result(state: CircuitState, graft: NativeCheckpointInPlaceGraft.Result, binding: String)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "checkpoint widget graft: " + why)

  def connect(before: CircuitState, after: CircuitState,
      source: NativeCheckpointInPlaceContext.Result): Result = {
    val peerText = NativeCheckpointWidgetPeerRouting.inspect(before, after)
    val peer = parse(peerText)
    def text(doc: JValue, key: String): String = (doc \ key).asInstanceOf[JString].s
    val hostPath = text(peer, "host")
    val live = GeneratedExternalOwners.instances(after, after.circuit.main)
    val execution = after.annotations.collect { case a: NativeCheckpointWidgetExecutionAnnotation => a }.head
    val pins = after.annotations.collect { case a: NativeCheckpointWidgetPinAnnotation => a }
    val selectedModule = execution.target.asPath.last._2.value
    val selectedPaths = live.filter(_._2.name == selectedModule).map(_._1)
    check(selectedPaths.size == 1, "selected definition must have one concrete execution")
    val selected = selectedPaths.head
    check(selected.startsWith(hostPath + ".") && selected.split('.').length == hostPath.split('.').length + 1,
      "selected execution must be an immediate child of peer host")
    check(source.host.form == LowForm && source.host.circuit.main == selectedModule,
      "graft source root must equal existing selected execution")
    check(source.host.annotations.toVector == Vector(firrtl.transforms.NoCircuitDedupAnnotation),
      "source annotation projection unsupported; exact NoCircuitDedup policy required")
    val originals = source.host.circuit.modules.map(m => m.name -> m).toMap
    val originalLive = GeneratedExternalOwners.instances(source.host, selectedModule).map(_._2.name).toSet
    check(originals.keySet == originalLive, "source must contain exactly its reachable definitions")
    val hostModules = after.circuit.modules.map(m => m.name -> m).toMap
    originals.foreach { case (name, module) =>
      check(hostModules.get(name).contains(module), "original selected definition differs: " + name)
    }
    // Reconstruct the actual bank/network/descriptor implementation. Neither a
    // supplied sidecar nor a coherently rehashed circuit can substitute for it.
    val graft = NativeCheckpointInPlaceGraft.generate(source)
    val descriptors = NativeCheckpointInPlaceAccess.descriptors(source)
    check(execution.entries == descriptors.entries.size && execution.gates == descriptors.physicalClockGates,
      "reserved entry/gate count differs from reconstructed descriptors")
    check(graft.state.annotations.toVector == Vector(firrtl.transforms.NoCircuitDedupAnnotation),
      "generated annotation projection unsupported")
    val replacements = graft.state.circuit.modules.filter(m => originals.get(m.name).exists(_ != m))
    val changedNames = replacements.map(_.name).toSet
    after.annotations.filterNot(_.isInstanceOf[NativeCheckpointWidgetExecutionAnnotation]).foreach { a =>
      check(!a.getTargets.exists(t => changedNames.exists(n =>
        ("(?:\\||:)" + java.util.regex.Pattern.quote(n) + "(?=[/>]|$)").r.findFirstIn(t.serialize).nonEmpty)),
        "host annotation targets changed selected definition; projection unsupported: " + a.getClass.getName)
    }
    replacements.foreach { m =>
      check(live.filter(_._2.name == m.name).forall { case (p, _) => p == selected || p.startsWith(selected + ".") },
        "changed definition has an execution outside selected subtree: " + m.name)
    }
    val additions = graft.state.circuit.modules.filterNot(m => originals.contains(m.name))
    check(additions.forall(m => !hostModules.contains(m.name)), "generated module collides with host definition")
    val root = graft.state.circuit.modules.find(_.name == selectedModule).get.asInstanceOf[Module]
    val expected = NativeCheckpointInPlaceGraft.capturePorts(execution.gates)
    check(root.ports == originals(selectedModule).ports ++ expected, "changed selected functional interface")
    val host = live.find(_._1 == hostPath).get._2.asInstanceOf[Module]
    check(live.count(_._2.name == host.name) == 1, "peer host definition is shared")
    val instance = selected.split('.').last
    val byRole = pins.map(p => p.role -> p.target.ref).toMap
    val outputRoles = expected.filter(_.direction == Output).map(p => p.name.stripPrefix("capture_")).toSet
    val changed = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
    def selectedPin(role: String, tpe: Type): Expression = WSubField(WRef(instance), "capture_" + role, tpe)
    val expectedByRole = expected.map(p => p.name.stripPrefix("capture_") -> p).toMap
    val roleByName = outputRoles.map(r => byRole(r) -> r).toMap
    def rewrite(s: Statement): Statement = s match {
      case n: DefNode if roleByName.contains(n.name) =>
        val role = roleByName(n.name); changed(role) += 1
        n.copy(value = selectedPin(role, expectedByRole(role).tpe))
      case c: Connect if roleByName.contains(c.loc.serialize) =>
        val role = roleByName(c.loc.serialize); changed(role) += 1
        c.copy(expr = selectedPin(role, expectedByRole(role).tpe))
      case s => s.mapStmt(rewrite)
    }
    val body = rewrite(host.body)
    check(outputRoles.forall(changed(_) == 1), "return peer placeholder must have one driver")
    val connections = expected.filter(_.direction == Input).map { p =>
      val role = p.name.stripPrefix("capture_")
      Connect(NoInfo, selectedPin(role, p.tpe), WRef(byRole(role), p.tpe))
    }
    val replacementMap = replacements.map(m => m.name -> m).toMap
    val circuit = after.circuit.copy(modules = after.circuit.modules.map {
      case m: Module if m.name == host.name => m.copy(body = Block(Seq(body) ++ connections))
      case m => replacementMap.getOrElse(m.name, m)
    } ++ additions)
    val working = firrtl.passes.ToWorkingIR.run(circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    // Retain every original annotation byte-for-byte. The source has no
    // target-bearing annotations, so there is no inferred rebasing exemption.
    val result = after.copy(circuit = flowed)
    val rows = expected.sortBy(_.name).map { p =>
      val role = p.name.stripPrefix("capture_")
      JObject(List("role" -> JString(role), "peer" -> JString(byRole(role)),
        "selected_pin" -> JString(instance + "." + p.name), "type" -> JString(p.tpe.serialize),
        "direction" -> JString(if (p.direction == Input) "from_transport" else "to_transport")))
    }
    val binding = canonical(JObject(List(
      "connection_version" -> JInt(1), "scope" -> JString("reserved_widget_to_existing_selected_graft_routes_only"),
      "peer_routing_sha256" -> JString(sha256(peerText)), "source_sha256" -> JString(sha256(after.circuit.serialize)),
      "original_selected_sha256" -> JString(sha256(source.host.circuit.serialize)),
      "graft_sha256" -> JString(sha256(graft.state.circuit.serialize)),
      "graft_binding_sha256" -> JString(sha256(graft.binding)),
      "connected_sha256" -> JString(sha256(result.circuit.serialize)),
      "host" -> JString(hostPath), "selected" -> JString(selected), "selected_module" -> JString(selectedModule),
      "pins" -> JArray(rows.toList),
      "replaced_modules" -> JArray(replacements.sortBy(_.name).map(m => JObject(List(
        "module" -> JString(m.name), "before_sha256" -> JString(sha256(originals(m.name).serialize)),
        "after_sha256" -> JString(sha256(m.serialize))))).toList),
      "added_modules" -> JArray(additions.sortBy(_.name).map(m => JObject(List(
        "module" -> JString(m.name), "sha256" -> JString(sha256(m.serialize))))).toList),
      "in_place_checkpoint_widget_connected" -> JBool(true), "descriptor_identity_bound" -> JBool(false),
      "external_transport_state_owner" -> JString("unsupported_no_state_adapter"),
      "production_annotations_translated" -> JBool(false), "production_freeze_owners_bound" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "metasim_restore_supported" -> JBool(false))))
    Result(result, graft, binding)
  }
}
