// See LICENSE for license details.
package midas.widgets

import chisel3._
import chisel3.util._
import firesim.lib.nasti._
import midas.checkpoint.SemanticStateInventory.{canonical, sha256}
import org.json4s._

/** Explicit component configuration. The manifest must describe the original
  * descriptors followed by the optional native owner and payload descriptors.
  * This API neither constructs that manifest nor authorizes a production image.
  */
case class CheckpointNativeEndpointParams(bits: Int, gates: Int, owners: Int, entries: Int,
    manifestSha256: String, captureNativeControl: Boolean = true,
    snapshotBank: Int = 252, nativeControlBank: Int = 251) {
  require(bits > 0 && gates > 0 && gates <= 65535 &&
    bits.toLong + gates + 694 + CheckpointWidgetEndpoint.bits + 97 <= Int.MaxValue,
    "native endpoint: invalid native/clock/full-shadow geometry")
  require(owners > 0 && owners < 65535 && entries > 0 && entries <= 65535,
    "native endpoint: invalid owner/descriptor count")
  require(snapshotBank >= 0 && snapshotBank < 255 && nativeControlBank >= 0 &&
    nativeControlBank < 255 && snapshotBank != nativeControlBank,
    "native endpoint: reserved selectors alias or exceed functional banks")
  require(manifestSha256.matches("[0-9a-f]{64}") && BigInt(manifestSha256, 16) != 0 &&
    manifestSha256 != midas.checkpoint.NativeCheckpointWidgetReservation.digest,
    "native endpoint: explicit nonzero, nonreservation manifest required")
  val payloadEntries = (if (bits / 64 > 0) 1 else 0) + (if (bits % 64 > 0) 1 else 0)
  val totalEntries = entries.toLong + payloadEntries + (if (captureNativeControl) 1 else 0)
  require(totalEntries <= 65535, "native endpoint: appended descriptor count exceeds transport ABI")
  val layout = CheckpointNativeEndpointLayout(bits, gates)

  /** Every original composite bit and the separate bank owner is observable.
    * These live words still lack atomic capture/restore ownership.
    */
  def observationBinding: String = canonical(JObject(List(
    "layout_version" -> JInt(1), "scope" -> JString("native_endpoint_live_observation_only"),
    "original_bits" -> JInt(bits), "gates" -> JInt(gates), "owners" -> JInt(owners),
    "external_clock_network" -> JInt(1),
    "clock_ownership" -> JString("externally_owned_clock_network_required"),
    "external_clock_contract" -> JString("caller_supplies_raw_clock_and_routes_clock_mask_and_ce_to_separately_owned_gates"),
    "root_and_gate_clock_outputs" -> JString("constant_zero_in_external_clock_network_mode"),
    "raw_composite_bits" -> JInt(layout.rawBits), "separate_bank_bits" -> JInt(67),
    "endpoint_bits" -> JInt(layout.endpointBits),
    "endpoint_shadow_layout_version" -> JInt(2),
    "endpoint_shadow_bits" -> JInt(layout.endpointBits + CheckpointWidgetEndpoint.bits),
    "endpoint_shadow_restore_supported" -> JBool(false),
    "raw_coordinator_offset" -> JInt(layout.nativeOwnerBits),
    "endpoint_order_lsb0" -> JArray(List(JString("coordinator_411"), JString("native_owner"),
      JString("payload_response_66"), JString("native_control_bank_67"))),
    "native_owner_bits" -> JInt(layout.nativeOwnerBits),
    "capture_native_control" -> JBool(captureNativeControl),
    "original_entries" -> JInt(entries), "total_entries" -> JInt(totalEntries),
    "snapshot_bank" -> JInt(snapshotBank), "native_control_bank" -> JInt(nativeControlBank),
    "manifest_sha256" -> JString(manifestSha256),
    "sources" -> JObject(CheckpointNativeCaptureTransportBlackBox.resources.map { name =>
      val in = getClass.getResourceAsStream("/" + name)
      require(in != null, "native endpoint: missing resource " + name)
      val body = try new String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally in.close()
      name -> JString(sha256(body))
    }.toList),
    "external_transport_state_owner" -> JString("unsupported_no_atomic_capture_restore_adapter"),
    "native_control_bank_state_owner" -> JString("unsupported_live_response_registers"),
    "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
    "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
}

/** A bijective endpoint observation view, distinct from the unchanged SV ABI.
  * Move the coordinator to bit zero for the existing endpoint shadow predicate;
  * retain all remaining composite bits and the separate response bank once.
  */
case class CheckpointNativeEndpointLayout(bits: Int, gates: Int) {
  require(bits > 0 && gates > 0 && bits.toLong + gates + 694 <= Int.MaxValue,
    "native endpoint layout: invalid geometry")
  val nativeOwnerBits = bits + gates + 150
  val rawBits = nativeOwnerBits + 411 + 66
  val endpointBits = rawBits + 67
  def endpointView(raw: UInt, bank: UInt): UInt = {
    require(raw.getWidth == rawBits && bank.getWidth == 67, "native endpoint layout: changed source widths")
    Cat(bank, raw(rawBits - 1, nativeOwnerBits + 411), raw(nativeOwnerBits - 1, 0),
      raw(nativeOwnerBits + 410, nativeOwnerBits))
  }
  def rawComposite(view: UInt): UInt = {
    require(view.getWidth == endpointBits, "native endpoint layout: changed endpoint width")
    Cat(view(rawBits - 1, nativeOwnerBits + 411), view(410, 0), view(nativeOwnerBits + 410, 411))
  }
  def bankState(view: UInt): UInt = {
    require(view.getWidth == endpointBits, "native endpoint layout: changed endpoint width")
    view(endpointBits - 1, rawBits)
  }
}

/** Exact native SV interface: deliberately no invented restore/physical ports. */
class CheckpointNativeCaptureTransportIO(cfg: CheckpointNativeEndpointParams) extends Bundle {
  val clock = Input(Clock()); val reset = Input(Bool())
  val ce = Input(UInt(cfg.gates.W)); val target_cycle = Input(UInt(64.W))
  val owner_present = Input(UInt(cfg.owners.W)); val owner_frozen = Input(UInt(cfg.owners.W))
  val owner_error = Input(UInt(cfg.owners.W)); val reader_idle = Input(Bool())
  val live_state = Input(UInt(cfg.bits.W))
  val root_clock = Output(Clock()); val gate_clock = Output(UInt(cfg.gates.W))
  val clock_mask = Output(Bool()); val freeze = Output(Bool()); val functional_hold = Output(Bool())
  val native_valid = Output(Bool()); val native_failed = Output(Bool())
  val native_cut_cycle = Output(UInt(64.W)); val captured_ce = Output(UInt(cfg.gates.W))
  val snapshot_state = Output(UInt(cfg.bits.W))
  val mmio_valid = Input(Bool()); val mmio_write = Input(Bool()); val mmio_ready = Output(Bool())
  val mmio_address = Input(UInt(8.W)); val mmio_wdata = Input(UInt(32.W))
  val mmio_response_valid = Output(Bool()); val mmio_response_ready = Input(Bool())
  val mmio_rdata = Output(UInt(32.W)); val mmio_response_error = Output(Bool())
  val descriptor_entry = Output(UInt(32.W)); val descriptor_valid = Input(Bool())
  val descriptor_bank = Input(UInt(8.W)); val descriptor_slot = Input(UInt(16.W))
  val descriptor_width = Input(UInt(16.W)); val descriptor_depth = Input(UInt(32.W))
  val bank_request_valid = Output(Bool()); val bank_request_ready = Input(Bool())
  val bank = Output(UInt(8.W)); val slot = Output(UInt(16.W)); val address = Output(UInt(32.W))
  val bank_response_valid = Input(Bool()); val bank_response_ready = Output(Bool())
  val bank_response_data = Input(UInt(64.W)); val bank_response_error = Input(Bool())
  val dma_valid = Output(Bool()); val dma_ready = Input(Bool()); val dma_data = Output(UInt(512.W))
  val dma_last = Output(Bool()); val dma_abort = Output(Bool())
  val control_state = Output(UInt(cfg.layout.rawBits.W))
  val native_control_bank_state = Output(UInt(67.W))
}

object CheckpointNativeCaptureTransportBlackBox {
  val resources = Seq("CheckpointNativeCaptureTransport.sv", "CheckpointNativeStateCapture.sv",
    "CheckpointNativeStateSnapshot.sv", "CheckpointSnapshotStateBank.sv", "CheckpointPhysicalClockCut.sv",
    "CheckpointCaptureTransport.sv", "CheckpointCaptureCut.sv")
}
class CheckpointNativeCaptureTransportBlackBox(cfg: CheckpointNativeEndpointParams)
    extends BlackBox(Map("BITS" -> cfg.bits, "GATES" -> cfg.gates, "OWNERS" -> cfg.owners,
      "ENTRIES" -> cfg.entries, "EXTERNAL_CLOCK_NETWORK" -> 1,
      "SNAPSHOT_BANK" -> cfg.snapshotBank, "NATIVE_CONTROL_BANK" -> cfg.nativeControlBank,
      "CAPTURE_NATIVE_CONTROL" -> (if (cfg.captureNativeControl) 1 else 0),
      "MANIFEST_SHA256" -> BigInt(cfg.manifestSha256, 16))) with HasBlackBoxResource {
  override def desiredName = "CheckpointNativeCaptureTransport"
  val io = IO(new CheckpointNativeCaptureTransportIO(cfg))
  CheckpointNativeCaptureTransportBlackBox.resources.foreach(n => addResource("/" + n))
}

/** Caller-owned functional/native interfaces. Nothing acknowledges a missing
  * owner, invents a target cycle, or connects the physical gates automatically.
  */
class CheckpointNativeEndpointSideband(cfg: CheckpointNativeEndpointParams) extends Bundle {
  val ce = Input(UInt(cfg.gates.W)); val targetCycle = Input(UInt(64.W))
  val ownerPresent = Input(UInt(cfg.owners.W)); val ownerFrozen = Input(UInt(cfg.owners.W))
  val ownerError = Input(UInt(cfg.owners.W)); val readerIdle = Input(Bool())
  val liveState = Input(UInt(cfg.bits.W))
  // Exact SV compatibility outputs are constant zero in external-network mode;
  // actual functional clocks must come from the caller's separately owned gates.
  val rootClock = Output(Clock()); val gateClock = Output(UInt(cfg.gates.W))
  val clockMask = Output(Bool()); val freeze = Output(Bool()); val functionalHold = Output(Bool())
  val valid = Output(Bool()); val failed = Output(Bool()); val cutCycle = Output(UInt(64.W))
  val capturedCE = Output(UInt(cfg.gates.W)); val snapshotState = Output(UInt(cfg.bits.W))
  val rawControlState = Output(UInt(cfg.layout.rawBits.W))
  val nativeControlBankState = Output(UInt(67.W))
  val endpointControlState = Output(UInt(cfg.layout.endpointBits.W))
}

/** Preparatory production API: actual NASTI/stream endpoint and native RTL.
  * No production widget selection, descriptor header, graft or image admission.
  * The shared endpoint observes a coordinator-first view without changing SV.
  */
class CheckpointNativeWidgetEndpoint(cfg: CheckpointNativeEndpointParams,
    np: NastiParameters = NastiParameters(32, 32, 4)) extends Module {
  val io = IO(new Bundle {
    val ctrl = Flipped(new NastiIO(np)); val stream = Decoupled(UInt(512.W))
    val native = new CheckpointNativeEndpointSideband(cfg)
    val descriptorEntry = Output(UInt(32.W)); val descriptorValid = Input(Bool())
    val descriptorBank = Input(UInt(8.W)); val descriptorSlot = Input(UInt(16.W))
    val descriptorWidth = Input(UInt(16.W)); val descriptorDepth = Input(UInt(32.W))
    val bankRequestValid = Output(Bool()); val bankRequestReady = Input(Bool())
    val bank = Output(UInt(8.W)); val slot = Output(UInt(16.W)); val address = Output(UInt(32.W))
    val bankResponseValid = Input(Bool()); val bankResponseReady = Output(Bool())
    val bankResponseData = Input(UInt(64.W)); val bankResponseError = Input(Bool())
    val streamLast = Output(Bool()); val streamAbort = Output(Bool())
    val endpointState = Output(UInt(CheckpointWidgetEndpoint.bits.W))
  })
  val nativeTransport = Module(new CheckpointNativeCaptureTransportBlackBox(cfg))
  val n = nativeTransport.io
  n.clock := clock; n.reset := reset.asBool
  n.ce := io.native.ce; n.target_cycle := io.native.targetCycle
  n.owner_present := io.native.ownerPresent; n.owner_frozen := io.native.ownerFrozen
  n.owner_error := io.native.ownerError; n.reader_idle := io.native.readerIdle; n.live_state := io.native.liveState
  io.native.rootClock := n.root_clock; io.native.gateClock := n.gate_clock
  io.native.clockMask := n.clock_mask; io.native.freeze := n.freeze; io.native.functionalHold := n.functional_hold
  io.native.valid := n.native_valid; io.native.failed := n.native_failed; io.native.cutCycle := n.native_cut_cycle
  io.native.capturedCE := n.captured_ce; io.native.snapshotState := n.snapshot_state
  io.native.rawControlState := n.control_state; io.native.nativeControlBankState := n.native_control_bank_state
  val view = cfg.layout.endpointView(n.control_state, n.native_control_bank_state)
  io.native.endpointControlState := view
  // Compatibility wire only: these inactive restore fields never become
  // native BlackBox ports. The endpoint uses its common MMIO/DMA members.
  val transport = Wire(new CheckpointCaptureTransportIO(cfg.owners, cfg.layout.endpointBits))
  transport.clock := clock; transport.reset := reset.asBool
  transport.restore_hold := false.B; transport.restore_load_pos := false.B; transport.restore_state := 0.U
  transport.target_cycle := io.native.targetCycle
  transport.owner_present := io.native.ownerPresent; transport.owner_frozen := io.native.ownerFrozen
  transport.owner_error := io.native.ownerError; transport.freeze := n.freeze; transport.control_state := view
  n.mmio_valid := transport.mmio_valid; n.mmio_write := transport.mmio_write
  n.mmio_address := transport.mmio_address; n.mmio_wdata := transport.mmio_wdata
  n.mmio_response_ready := transport.mmio_response_ready
  transport.mmio_ready := n.mmio_ready; transport.mmio_response_valid := n.mmio_response_valid
  transport.mmio_rdata := n.mmio_rdata; transport.mmio_response_error := n.mmio_response_error
  io.descriptorEntry := n.descriptor_entry; n.descriptor_valid := io.descriptorValid
  n.descriptor_bank := io.descriptorBank; n.descriptor_slot := io.descriptorSlot
  n.descriptor_width := io.descriptorWidth; n.descriptor_depth := io.descriptorDepth
  transport.descriptor_entry := n.descriptor_entry; transport.descriptor_valid := io.descriptorValid
  transport.descriptor_bank := io.descriptorBank; transport.descriptor_slot := io.descriptorSlot
  transport.descriptor_width := io.descriptorWidth; transport.descriptor_depth := io.descriptorDepth
  io.bankRequestValid := n.bank_request_valid; n.bank_request_ready := io.bankRequestReady
  io.bank := n.bank; io.slot := n.slot; io.address := n.address
  n.bank_response_valid := io.bankResponseValid; io.bankResponseReady := n.bank_response_ready
  n.bank_response_data := io.bankResponseData; n.bank_response_error := io.bankResponseError
  transport.bank_request_valid := n.bank_request_valid; transport.bank_request_ready := io.bankRequestReady
  transport.bank := n.bank; transport.slot := n.slot; transport.address := n.address
  transport.bank_response_valid := io.bankResponseValid; transport.bank_response_ready := n.bank_response_ready
  transport.bank_response_data := io.bankResponseData; transport.bank_response_error := io.bankResponseError
  transport.dma_valid := n.dma_valid; transport.dma_data := n.dma_data; transport.dma_last := n.dma_last
  transport.dma_abort := n.dma_abort; n.dma_ready := transport.dma_ready
  io.streamLast := n.dma_last; io.streamAbort := n.dma_abort
  val cutValid = n.native_valid && !n.native_failed && n.native_cut_cycle === io.native.targetCycle &&
    io.native.ownerPresent.andR && io.native.ownerFrozen.andR && !io.native.ownerError.orR
  io.endpointState := CheckpointWidgetEndpoint.build(np, io.ctrl, transport,
    io.stream, io.native.targetCycle, cutValid)
}
