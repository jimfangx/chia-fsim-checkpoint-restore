// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, WRef}
import firrtl.annotations.JsonProtocol
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Explicit analysis permission for the actual reserved transport. Its state
  * remains an unsupported external owner. The temporary projection NEVER
  * enters an instrumented or emitted circuit and grants no capture ownership.
  */
final class ReservedCheckpointInventory private (
    private val original: CircuitState, private val peerBinding: String,
    private val transport: ExtModule, val unsupportedOwners: JArray, val binding: String) {
  private[checkpoint] def analysisFor(host: CircuitState): CircuitState = {
    require(host.form == LowForm && host.circuit == original.circuit &&
      host.annotations.toVector == Vector(firrtl.transforms.NoCircuitDedupAnnotation),
      "reserved checkpoint inventory: changed original host or NoCircuitDedup-only storage policy")
    // Invalid outputs carry no functional clock/CE provenance. Existing clock
    // and dependency analyses reject any attempt to use these as functional
    // clocks or CE dependencies, instead of inventing a value or a state owner.
    val body = Block(transport.ports.filter(_.direction == Output).map(p => IsInvalid(NoInfo, WRef(p.name, p.tpe))))
    host.copy(circuit = host.circuit.copy(modules = host.circuit.modules.map {
      case m if m == transport => Module(m.info, m.name, m.ports, body)
      case m => m
    }))
  }
  def validate(before: CircuitState, after: CircuitState): Unit = {
    require(after.form == original.form && after.circuit == original.circuit &&
      after.annotations.toVector == original.annotations.toVector &&
      NativeCheckpointWidgetPeerRouting.inspect(before, after) == peerBinding,
      "reserved checkpoint inventory: token belongs to a different widget/source epoch")
  }
}

object ReservedCheckpointInventory {
  import SemanticStateInventory.{canonical, sha256}
  def forReservedWidget(before: CircuitState, after: CircuitState): ReservedCheckpointInventory = {
    // This checks the real ABI, exact inline resources, reservation parameters,
    // registered endpoints and native-inline provenance, not just a module name.
    val peer = NativeCheckpointWidgetPeerRouting.inspect(before, after)
    val JString(path) = parse(peer) \ "transport"
    val live = GeneratedExternalOwners.instances(after, after.circuit.main)
    val transport = live.find(_._1 == path).get._2.asInstanceOf[ExtModule]
    require(transport.defname == "CheckpointPhysicalCaptureTransport" &&
      live.count(_._2.name == transport.name) == 1,
      "reserved checkpoint inventory: one exact reserved physical transport required")
    val sources = after.annotations.collect {
      case a: firrtl.transforms.BlackBoxInlineAnno if a.target.name == transport.name => a
    }
    val unsupported = JArray(List(JObject(List(
      "hierarchy" -> JString(path), "module" -> JString(transport.name),
      "defname" -> JString(transport.defname), "definition_sha256" -> JString(sha256(transport.serialize)),
      "sources" -> JObject(sources.sortBy(_.name).map(a => a.name -> JString(sha256(a.text))).toList),
      "ownership" -> JString("unsupported_no_atomic_capture_restore_adapter"),
      "analysis_projection" -> JString("invalid_outputs_no_storage_no_emission"),
      "capture_supported" -> JBool(false), "restore_supported" -> JBool(false)))))
    val binding = canonical(JObject(List(
      "inventory_permission_version" -> JInt(1), "scope" -> JString("reserved_widget_global_analysis_only"),
      "host_sha256" -> JString(sha256(after.circuit.serialize)),
      "host_annotations_sha256" -> JString(sha256(JsonProtocol.serialize(after.annotations))),
      "peer_routing_sha256" -> JString(sha256(peer)),
      "unsupported_external_owners" -> unsupported,
      "production_freeze_owners_bound" -> JBool(false), "complete_fpga_capture" -> JBool(false),
      "deployable_capture_image" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    new ReservedCheckpointInventory(after, peer, transport, unsupported, binding)
  }
}
