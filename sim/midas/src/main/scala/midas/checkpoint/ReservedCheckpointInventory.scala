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
    private val transport: ExtModule, val source: CircuitState,
    val unsupportedOwners: JArray, val binding: String) {
  private[checkpoint] def validateSource(host: CircuitState): Unit = {
    require(host.form == source.form && host.circuit == source.circuit &&
      host.annotations.toVector == source.annotations.toVector,
      "reserved checkpoint inventory: changed original source policy or compiler context")
  }
  private[checkpoint] def analysisFor(host: CircuitState): CircuitState = {
    require(host.form == LowForm && host.circuit == original.circuit &&
      host.annotations.toVector == source.annotations.filterNot(SimulatorBankProbeContext.accepts).toVector,
      "reserved checkpoint inventory: changed original host or bound typed storage policy")
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
  def forReservedWidget(before: CircuitState, after: CircuitState): ReservedCheckpointInventory =
    create(before, after, projectSource = false)

  /** Explicit whole-host projection, not a caller-selected annotation subset.
    * The full original annotation sequence remains bound and emitted. Only
    * typed storage contracts and checked inert compiler context enter banking.
    * Unknown owner metadata remains subject to the connection/image guards.
    */
  def forReservedWidgetSource(before: CircuitState, after: CircuitState): ReservedCheckpointInventory =
    create(before, after, projectSource = true)

  private def create(before: CircuitState, after: CircuitState,
      projectSource: Boolean): ReservedCheckpointInventory = {
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
    val source = after.copy(annotations = if (projectSource) after.annotations.filter(a =>
      GeneratedStateBankAnnotations.supported(a) || SimulatorBankProbeContext.accepts(a)) else
      Seq(firrtl.transforms.NoCircuitDedupAnnotation))
    require(source.annotations.count(_ == firrtl.transforms.NoCircuitDedupAnnotation) == 1,
      "reserved checkpoint inventory: one explicit NoCircuitDedup source policy required")
    val contextFields = if (projectSource) {
      val beforeContext = before.annotations.filter(SimulatorBankProbeContext.accepts)
      val context = source.annotations.filter(SimulatorBankProbeContext.accepts)
      // Check the actual pre-inline sequence too: InlineInstances may coalesce
      // duplicate annotations, which must not hide conflicting source options.
      val beforeSnapshot = SimulatorBankProbeContext.snapshot(beforeContext,
        before.circuit.serialize, before.circuit.serialize)
      val snapshot = SimulatorBankProbeContext.snapshot(context,
        after.circuit.serialize, after.circuit.serialize)
      require(beforeContext.forall(a => context.count(_ == a) == 1),
        "reserved checkpoint inventory: original compiler context absent or ambiguous after inline")
      GeneratedStateBankAnnotations.preflight(source.copy(annotations =
        source.annotations.filterNot(SimulatorBankProbeContext.accepts)))
      List("source_policy" -> JObject(List(
        "policy_version" -> JInt(1),
        "scope" -> JString("exact_ordered_whole_host_typed_storage_and_inert_context"),
        "source_annotations_sha256" -> JString(sha256(JsonProtocol.serialize(source.annotations))),
        "retained_annotations_sha256" -> JString(sha256(JsonProtocol.serialize(
          after.annotations.filterNot(source.annotations.contains)))),
        "original_context" -> parse(beforeSnapshot), "source_context" -> parse(snapshot))))
    } else Nil
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
      "deployable_capture_image" -> JBool(false), "metasim_restore_supported" -> JBool(false)) ++ contextFields))
    new ReservedCheckpointInventory(after, peer, transport, source, unsupported, binding)
  }
}
