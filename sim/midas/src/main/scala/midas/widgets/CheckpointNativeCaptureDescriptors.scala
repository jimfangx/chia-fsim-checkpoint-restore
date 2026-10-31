// See LICENSE for license details.
package midas.widgets

import chisel3._
import chisel3.util._
import firesim.lib.nasti._
import midas.checkpoint.{SemanticStateInventory, StateAccessEntry}
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** The native RTL consumes only the original ROM and appends its own entries.
  * The host must instead use the complete, ordered stream table below. Neither
  * table is a semantic DUT inventory or permission to deploy a capture image.
  */
final class CheckpointNativeCaptureDescriptors private (
    val original: CheckpointCaptureDescriptors, val endpoint: CheckpointNativeEndpointParams,
    val ownerNames: Seq[String], val entries: Seq[StateAccessEntry],
    val manifest: String, val binding: String) {
  val manifestSha256: String = SemanticStateInventory.sha256(manifest)
  val bindingSha256: String = SemanticStateInventory.sha256(binding)
  require(endpoint.manifestSha256 == manifestSha256 && endpoint.entries == original.entries.size &&
    endpoint.totalEntries == entries.size, "native descriptors: endpoint/table identity mismatch")

  /** Instantiate CheckpointCaptureDescriptorROM(original), NEVER a ROM for the
    * appended table: CheckpointNativeCaptureTransport appends those entries.
    */
  def checkEndpoint(candidate: CheckpointNativeEndpointParams): Unit =
    require(candidate == endpoint, "native descriptors: changed endpoint ABI or manifest")

  def cppManifest: String = {
    val bytes = manifestSha256.grouped(2).toSeq.reverse.map(b => s"0x$b").mkString(", ")
    val rows = entries.map(e => s"{${e.bank}U, ${e.slot}U, ${e.width}U, ${e.depth}U}").mkString(", ")
    s"checkpoint_capture_manifest_t{{{$bytes}}, {$rows}}"
  }

  /** The driver validates the native appended table and endpoint shadow layout
    * before any MMIO access. Complete stream descriptors accompany this ABI.
    */
  def cppNativeBinding: String = {
    val values = Seq(endpoint.entries, endpoint.bits, endpoint.gates, endpoint.owners,
      if (endpoint.captureNativeControl) 1 else 0, endpoint.snapshotBank, endpoint.nativeControlBank,
      2, endpoint.layout.endpointBits + CheckpointWidgetEndpoint.bits)
    "checkpoint_native_capture_binding_t{" + values.map(v => s"${v}U").mkString(", ") + "}"
  }

  /** Native-specific metadata, deliberately distinct from the production
    * widget header. It cannot authorize an image or release a restore owner.
    */
  def header(prefix: String): String = {
    require(prefix.matches("[A-Za-z_][A-Za-z0-9_]*"), "native descriptors: invalid C identifier")
    val values = Seq("NATIVE_DESCRIPTOR_LAYOUT_VERSION" -> 1,
      "ORIGINAL_ROM_ENTRIES" -> endpoint.entries, "STREAM_ENTRIES" -> entries.size,
      "EXTERNAL_OWNERS" -> endpoint.owners, "COORDINATOR_OWNERS" -> (endpoint.owners + 1),
      "NATIVE_BITS" -> endpoint.bits, "PHYSICAL_CLOCK_GATES" -> endpoint.gates,
      "EXTERNAL_CLOCK_NETWORK" -> 1, "CAPTURE_NATIVE_CONTROL" -> (if (endpoint.captureNativeControl) 1 else 0),
      "SNAPSHOT_BANK" -> endpoint.snapshotBank, "NATIVE_CONTROL_BANK" -> endpoint.nativeControlBank,
      "NATIVE_OWNER_BITS" -> endpoint.layout.nativeOwnerBits,
      "RAW_CONTROL_BITS" -> endpoint.layout.rawBits, "SEPARATE_BANK_BITS" -> 67,
      "SHADOW_CONTROL_BITS" -> endpoint.layout.endpointBits,
      "SHADOW_BITS" -> (endpoint.layout.endpointBits + CheckpointWidgetEndpoint.bits),
      "SHADOW_LAYOUT_VERSION" -> 2, "SHADOW_RESTORE_SUPPORTED" -> 0,
      "FRAME_BYTES" -> 64, "TRANSPORT_VERSION" -> 1,
      "COMPLETE_FPGA_CAPTURE" -> 0, "DEPLOYABLE_CAPTURE_IMAGE" -> 0,
      "PRODUCTION_FREEZE_OWNERS_BOUND" -> 0, "METASIM_RESTORE_SUPPORTED" -> 0)
    val sb = new StringBuilder("// Native descriptor binding only; original ROM and complete stream counts differ.\n")
    values.foreach { case (name, value) => sb.append(s"#define ${prefix}_${name} ${value}U\n") }
    Seq("MANIFEST_SHA256" -> manifestSha256, "DESCRIPTOR_BINDING_SHA256" -> bindingSha256,
      "ORIGINAL_MANIFEST_SHA256" -> original.manifestSha256,
      "ORIGINAL_BINDING_SHA256" -> original.bindingSha256).foreach { case (name, value) =>
      sb.append(s"#define ${prefix}_${name} \"$value\"\n")
    }
    entries.zipWithIndex.foreach { case (e, i) =>
      sb.append(s"// stream descriptor[$i] StateID: ${e.stateID}; bank=${e.bank} slot=${e.slot} width=${e.width} depth=${e.depth}\n")
    }
    ownerNames.zipWithIndex.foreach { case (name, i) => sb.append(s"// external owner[$i]: $name\n") }
    sb.toString
  }
}

object CheckpointNativeCaptureDescriptors {
  import SemanticStateInventory.{canonical, sha256}

  def bind(original: CheckpointCaptureDescriptors, bits: Int, gates: Int,
      ownerNames: Seq[String], captureNativeControl: Boolean = true,
      snapshotBank: Int = 252, nativeControlBank: Int = 251): CheckpointNativeCaptureDescriptors = {
    require(original != null, "native descriptors: missing validated original descriptors")
    require(ownerNames.nonEmpty && ownerNames.distinct.size == ownerNames.size &&
      ownerNames.forall(_.matches("[A-Za-z0-9_.:/-]+")), "native descriptors: explicit unique named owners required")
    // Validate sizes before arithmetic or allocating entries. The temporary
    // digest is never emitted; the final params below use the complete table.
    val geometry = CheckpointNativeEndpointParams(bits, gates, ownerNames.size, original.entries.size,
      "1" * 64, captureNativeControl, snapshotBank, nativeControlBank)
    require(original.physicalClockGates == 0 || original.physicalClockGates == gates,
      "native descriptors: original physical CE count differs from native clock geometry")
    // Reserve both selectors even when control capture is disabled, so an
    // option change cannot reinterpret an original functional bank silently.
    require(original.entries.forall(e => e.bank != snapshotBank && e.bank != nativeControlBank),
      "native descriptors: original bank aliases reserved native selector")
    val resources = JObject(CheckpointNativeCaptureTransportBlackBox.resources.map { name =>
      val in = getClass.getResourceAsStream("/" + name)
      require(in != null, "native descriptors: missing source resource " + name)
      val text = try new String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally in.close()
      name -> JString(sha256(text))
    }.toList)
    val abi = JObject(List("bits" -> JInt(bits), "gates" -> JInt(gates),
      "external_owners" -> JArray(ownerNames.map(JString).toList),
      "coordinator_owners" -> JInt(ownerNames.size + 1), "external_clock_network" -> JInt(1),
      "capture_native_control" -> JBool(captureNativeControl), "snapshot_bank" -> JInt(snapshotBank),
      "native_control_bank" -> JInt(nativeControlBank), "original_entries" -> JInt(original.entries.size),
      "stream_entries" -> JInt(geometry.totalEntries), "native_owner_bits" -> JInt(geometry.layout.nativeOwnerBits),
      "raw_control_bits" -> JInt(geometry.layout.rawBits), "separate_bank_bits" -> JInt(67),
      "shadow_control_bits" -> JInt(geometry.layout.endpointBits),
      "shadow_bits" -> JInt(geometry.layout.endpointBits + CheckpointWidgetEndpoint.bits),
      "shadow_layout_version" -> JInt(2), "shadow_restore_supported" -> JBool(false)))
    val identity = canonical(JObject(List("domain" -> JString("native_transport_descriptor_v1"),
      "original_manifest_sha256" -> JString(original.manifestSha256),
      "original_binding_sha256" -> JString(original.bindingSha256), "abi" -> abi, "sources" -> resources)))
    def appended(kind: String, bank: Int, slot: Int, width: Int, depth: Int): StateAccessEntry =
      StateAccessEntry(sha256(identity + "\n" + kind), bank, slot, width, depth)
    val control = if (captureNativeControl) Seq(appended("native_owner", nativeControlBank, 0, 64,
      ((geometry.layout.nativeOwnerBits.toLong + 63) / 64).toInt)) else Nil
    val full = if (bits / 64 > 0) Seq(appended("staged_payload_full", snapshotBank, 0, 64, bits / 64)) else Nil
    val tail = if (bits % 64 > 0) Seq(appended("staged_payload_tail", snapshotBank, 1, bits % 64, 1)) else Nil
    val entries = original.entries.toVector ++ control ++ full ++ tail
    require(entries.map(_.stateID).distinct.size == entries.size &&
      entries.map(e => (e.bank, e.slot)).distinct.size == entries.size,
      "native descriptors: duplicate stream StateID or bank selector")
    val roles = Vector.fill(original.entries.size)("original") ++ control.map(_ => "native_owner") ++
      full.map(_ => "staged_payload_full") ++ tail.map(_ => "staged_payload_tail")
    val state = entries.zip(roles).zipWithIndex.map { case ((e, role), i) => JObject(List(
      "ordinal" -> JInt(i), "domain" -> JString(role), "state_id" -> JString(e.stateID),
      "bank" -> JInt(e.bank), "slot" -> JInt(e.slot), "width" -> JInt(e.width), "depth" -> JInt(e.depth))) }
    val deficits = List("complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false),
      "semantic_dut_inventory" -> JBool(false),
      "native_control_bank_state_owner" -> JString("unsupported_live_response_registers"),
      "external_transport_state_owner" -> JString("unsupported_no_atomic_capture_restore_adapter"),
      "physical_clock_owner" -> JString("external_common_cut_and_gate_owners_required"))
    val manifest = canonical(JObject(List("descriptor_version" -> JInt(1),
      "scope" -> JString("native_transport_ordered_stream_descriptors_only"), "abi" -> abi,
      "original_manifest_sha256" -> JString(original.manifestSha256),
      "original_binding_sha256" -> JString(original.bindingSha256), "sources" -> resources,
      "state" -> JArray(state.toList)) ++ deficits))
    val binding = canonical(JObject(List("binding_version" -> JInt(1),
      "scope" -> JString("validated_original_rom_and_native_appended_stream_binding_only"),
      "manifest_sha256" -> JString(sha256(manifest)), "manifest" -> parse(manifest),
      "original_manifest" -> parse(original.manifest), "original_binding" -> parse(original.binding),
      "original_rom_entries" -> JInt(original.entries.size),
      "original_rom_appended_entries_valid" -> JBool(false)) ++ deficits))
    new CheckpointNativeCaptureDescriptors(original, geometry.copy(manifestSha256 = sha256(manifest)),
      ownerNames.toVector, entries, manifest, binding)
  }
}

/** The checked original ROM drives the native endpoint internally. There is
  * no external descriptor input that can substitute a table after binding.
  * This remains a component: the production reservation/graft is unchanged.
  */
class CheckpointNativeBoundWidgetEndpoint(descriptors: CheckpointNativeCaptureDescriptors,
    np: NastiParameters = NastiParameters(32, 32, 4)) extends Module {
  private val cfg = descriptors.endpoint
  val io = IO(new Bundle {
    val ctrl = Flipped(new NastiIO(np)); val stream = Decoupled(UInt(512.W))
    val native = new CheckpointNativeEndpointSideband(cfg)
    val descriptorEntry = Output(UInt(32.W))
    val bankRequestValid = Output(Bool()); val bankRequestReady = Input(Bool())
    val bank = Output(UInt(8.W)); val slot = Output(UInt(16.W)); val address = Output(UInt(32.W))
    val bankResponseValid = Input(Bool()); val bankResponseReady = Output(Bool())
    val bankResponseData = Input(UInt(64.W)); val bankResponseError = Input(Bool())
    val streamLast = Output(Bool()); val streamAbort = Output(Bool())
    val endpointState = Output(UInt(CheckpointWidgetEndpoint.bits.W))
  })
  val endpoint = Module(new CheckpointNativeWidgetEndpoint(cfg, np))
  val originalROM = Module(new CheckpointCaptureDescriptorROM(descriptors.original))
  endpoint.io.ctrl <> io.ctrl; io.stream <> endpoint.io.stream; io.native <> endpoint.io.native
  originalROM.io.entry := endpoint.io.descriptorEntry
  endpoint.io.descriptorValid := originalROM.io.valid
  endpoint.io.descriptorBank := originalROM.io.bank; endpoint.io.descriptorSlot := originalROM.io.slot
  endpoint.io.descriptorWidth := originalROM.io.bitWidth; endpoint.io.descriptorDepth := originalROM.io.depth
  io.descriptorEntry := endpoint.io.descriptorEntry
  io.bankRequestValid := endpoint.io.bankRequestValid; endpoint.io.bankRequestReady := io.bankRequestReady
  io.bank := endpoint.io.bank; io.slot := endpoint.io.slot; io.address := endpoint.io.address
  endpoint.io.bankResponseValid := io.bankResponseValid; io.bankResponseReady := endpoint.io.bankResponseReady
  endpoint.io.bankResponseData := io.bankResponseData; endpoint.io.bankResponseError := io.bankResponseError
  io.streamLast := endpoint.io.streamLast; io.streamAbort := endpoint.io.streamAbort
  io.endpointState := endpoint.io.endpointState
}
