// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import firrtl.ir._
import midas.stage.GoldenGateOutputFileAnnotation
import midas.widgets.{CheckpointCaptureABI, CheckpointCaptureWidgetParams}
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Bind the selected readout's reconstructed global descriptors to BOTH the
  * real transport and its registered host header. This is identity binding,
  * not a freeze-owner adapter or permission to emit a production image.
  */
object NativeCheckpointWidgetIdentity {
  import SemanticStateInventory.{canonical, sha256}
  val incomplete = "#error Incomplete_checkpoint_hardware_coverage"
  val prefix = "CHECKPOINT_CAPTURE"
  case class Result(state: CircuitState, connection: NativeCheckpointWidgetGraft.Result, binding: String)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "checkpoint widget identity: " + why)

  def bind(before: CircuitState, after: CircuitState,
      source: NativeCheckpointInPlaceContext.Result): Result = {
    // Rebuild all routes, ROMs, state identities and source bindings. No
    // caller-provided connected circuit or descriptor sidecar is trusted.
    val connection = NativeCheckpointWidgetGraft.connect(before, after, source)
    val descriptors = NativeCheckpointInPlaceAccess.descriptors(source)
    val execution = after.annotations.collect { case a: NativeCheckpointWidgetExecutionAnnotation => a }.head
    val peer = parse(NativeCheckpointWidgetPeerRouting.inspect(before, after))
    val JString(transportPath) = peer \ "transport"
    val live = GeneratedExternalOwners.instances(connection.state, connection.state.circuit.main)
    val transport = live.find(_._1 == transportPath).get._2.asInstanceOf[ExtModule]
    check(live.count(_._2.name == transport.name) == 1, "transport definition is shared")
    val headers = connection.state.annotations.collect {
      case a: GoldenGateOutputFileAnnotation if a.fileSuffix == ".const.h" => a
    }
    check(headers.size == 1, "one actual registered host header required")
    val header = headers.head
    val originalHeaders = before.annotations.collect {
      case a: GoldenGateOutputFileAnnotation if a.fileSuffix == ".const.h" => a
    }
    check(originalHeaders.size == 1 && originalHeaders.head == header,
      "registered header changed or duplicated across native lowering")
    val marker = "(?m)^// CHECKPOINT_WIDGET_RESERVATION_BEGIN base=([0-9]+) stream=([0-9]+)\\n#error Unbound_checkpoint_widget_reservation\\n// CHECKPOINT_WIDGET_RESERVATION_END\\n".r
    val matches = marker.findAllMatchIn(header.body).toVector
    check(matches.size == 1 && !header.body.contains(prefix + "_") &&
      header.body.sliding("Unbound_checkpoint_widget_reservation".length)
        .count(_ == "Unbound_checkpoint_widget_reservation") == 1,
      "missing, duplicate or conflicting registered header reservation")
    val matched = matches.head
    val base = BigInt(matched.group(1)); val streamValue = BigInt(matched.group(2))
    check(streamValue.isValidInt, "stream index exceeds host ABI")
    val stream = streamValue.toInt
    check(base >= 0 && base + CheckpointCaptureABI.registers.map(_._2).max < (BigInt(1) << 64),
      "MMIO address exceeds host ABI")
    val cfg = CheckpointCaptureWidgetParams(descriptors.entries.size, descriptors.manifestSha256,
      execution.owners, execution.queueDepth, Some(descriptors), execution.gates)
    // Keep a compilation error until complete physical/functional/host owner
    // coverage and restore have a real contract. Metadata binding alone cannot
    // remove the production admission guard.
    val replacement = incomplete + "\n" + CheckpointCaptureABI.header(prefix, base, cfg, stream) +
      "#ifdef GET_INCLUDES\n#include \"bridges/checkpoint_capture.h\"\n#endif\n" +
      "#ifdef GET_BRIDGE_CONSTRUCTOR\n" +
      s"registry.add_widget(new checkpoint_capture_t(simif, *registry.get_stream_engine(), ${base}ULL, ${stream}U,\n" +
      s"  ${descriptors.cppManifest}));\n#endif\n"
    val boundHeader = header.copy(body = header.body.take(matched.start) + replacement + header.body.drop(matched.end))
    val boundTransport = transport.copy(params = transport.params.map {
      case IntParam("MANIFEST_SHA256", _) => IntParam("MANIFEST_SHA256", BigInt(descriptors.manifestSha256, 16))
      case p => p
    })
    check(descriptors.manifestSha256 != NativeCheckpointWidgetReservation.digest,
      "reconstructed identity aliases unbound sentinel")
    val result = connection.state.copy(circuit = connection.state.circuit.copy(modules =
      connection.state.circuit.modules.map(m => if (m.name == transport.name) boundTransport else m)),
      annotations = connection.state.annotations.map(a => if (a == header) boundHeader else a))
    val binding = canonical(JObject(List(
      "identity_version" -> JInt(1), "scope" -> JString("selected_readout_transport_and_host_identity_only"),
      "connection_sha256" -> JString(sha256(connection.binding)),
      "unbound_sha256" -> JString(sha256(connection.state.circuit.serialize)),
      "bound_sha256" -> JString(sha256(result.circuit.serialize)),
      "descriptor_manifest_sha256" -> JString(descriptors.manifestSha256),
      "descriptor_binding_sha256" -> JString(descriptors.bindingSha256),
      "transport" -> JString(transportPath), "transport_module" -> JString(transport.name),
      "transport_before_sha256" -> JString(sha256(transport.serialize)),
      "transport_after_sha256" -> JString(sha256(boundTransport.serialize)),
      "header_before_sha256" -> JString(sha256(header.body)),
      "header_after_sha256" -> JString(sha256(boundHeader.body)),
      "header_prefix" -> JString(prefix), "base" -> JInt(base), "stream" -> JInt(stream),
      "entries" -> JInt(execution.entries), "gates" -> JInt(execution.gates),
      "owners" -> JArray(execution.owners.map(JString(_)).toList), "queue_depth" -> JInt(execution.queueDepth),
      "descriptor_identity_bound" -> JBool(true), "production_annotations_translated" -> JBool(false),
      "external_transport_state_owner" -> JString("unsupported_no_state_adapter"),
      "production_freeze_owners_bound" -> JBool(false), "complete_fpga_capture" -> JBool(false),
      "deployable_capture_image" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    Result(result, connection, binding)
  }
}
