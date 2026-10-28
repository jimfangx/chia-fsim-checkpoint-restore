// See LICENSE for license details.
package midas.widgets

import chisel3._
import chisel3.util._
import chisel3.util.HasBlackBoxResource
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.InModuleBody
import firesim.lib.nasti._

/** Public byte offsets shared with the host driver. The transport is not a target bridge:
  * its clock must continue while every simulator owner is frozen.
  */
object CheckpointCaptureABI {
  val registers = Seq("COMMAND" -> 0x00, "STATUS" -> 0x04, "ERROR" -> 0x08,
    "CUT_LO" -> 0x10, "CUT_HI" -> 0x14, "WORDS_LO" -> 0x18, "WORDS_HI" -> 0x1c,
    "ENTRY_COUNT" -> 0x20, "CRC32C" -> 0x24) ++
    (0 until 8).map(i => s"MANIFEST_$i" -> (0x40 + 4 * i)) ++
    Seq("STREAM_INVALID" -> 0x60, "STREAM_DRAIN_ACK" -> 0x64,
      "PRODUCED_FRAMES_LO" -> 0x68, "PRODUCED_FRAMES_HI" -> 0x6c,
      "HOST_DRAINED_FRAMES_LO" -> 0x70, "HOST_DRAINED_FRAMES_HI" -> 0x74) ++
    CheckpointLiveWidgetSnapshot.registers

  def header(prefix: String, base: BigInt, cfg: CheckpointCaptureWidgetParams, stream: Int): String = {
    require(!cfg.deferredGraft, "unbound checkpoint reservation cannot emit a descriptor-bound driver header")
    val sb = new StringBuilder
    sb.append(s"// Global checkpoint transport; no complete CPU/host ownership claim.\n")
    sb.append(s"#define ${prefix}_BASE ${base}ULL\n")
    registers.foreach { case (name, offset) =>
      sb.append(s"#define ${prefix}_${name}_OFFSET ${offset}U\n")
      sb.append(s"#define ${prefix}_${name}_ADDR ${base + offset}ULL\n")
    }
    sb.append(s"#define ${prefix}_TO_HOST_STREAM_IDX ${stream}U\n")
    sb.append(s"#define ${prefix}_TO_HOST_QUEUE_DEPTH ${cfg.queueDepth}U\n")
    sb.append(s"#define ${prefix}_ENTRIES ${cfg.entries}U\n")
    sb.append(s"#define ${prefix}_OWNERS ${cfg.ownerNames.size}U\n")
    sb.append(s"#define ${prefix}_PHYSICAL_CLOCK_GATES ${cfg.physicalClockGates}U\n")
    sb.append(s"#define ${prefix}_EXTERNAL_CLOCK_NETWORK ${if (cfg.physicalClockGates > 0) 1 else 0}U\n")
    sb.append(s"#define ${prefix}_MANIFEST_SHA256 \"${cfg.manifestSha256}\"\n")
    cfg.descriptors.foreach { descriptors =>
      sb.append(s"#define ${prefix}_DESCRIPTOR_BINDING_SHA256 \"${descriptors.bindingSha256}\"\n")
      sb.append(s"#define ${prefix}_INTERNAL_DESCRIPTOR_ROM 1U\n")
      descriptors.entries.zipWithIndex.foreach { case (entry, index) =>
        sb.append(s"// descriptor[$index] StateID: ${entry.stateID}\n")
      }
    }
    (Seq("CheckpointCaptureTransport.sv", "CheckpointCaptureCut.sv") ++
      (if (cfg.physicalClockGates > 0) Seq("CheckpointPhysicalCaptureTransport.sv", "CheckpointPhysicalClockCut.sv") else Nil)).foreach { resource =>
      val in = getClass.getResourceAsStream("/" + resource)
      require(in != null, s"missing checkpoint RTL resource: $resource")
      val bytes = try in.readAllBytes() finally in.close()
      val sha = java.security.MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 255}%02x").mkString
      sb.append(s"// $resource SHA256: $sha\n")
    }
    sb.append(s"#define ${prefix}_FRAME_BYTES 64U\n#define ${prefix}_TRANSPORT_VERSION 1U\n")
    sb.append(s"#define ${prefix}_ABORT_REQUIRES_HOST_DRAIN 1U\n")
    val controlBits = if (cfg.physicalClockGates > 0) 481 + cfg.physicalClockGates else 411
    sb.append(s"#define ${prefix}_SHADOW_LAYOUT_VERSION 2U\n")
    sb.append(s"#define ${prefix}_SHADOW_CONTROL_BITS ${controlBits}U\n")
    sb.append(s"#define ${prefix}_SHADOW_BITS ${controlBits + CheckpointLiveWidgetSnapshot.localBits + CheckpointNastiState.bits}U\n")
    sb.append(s"#define ${prefix}_SHADOW_ADAPTER_BITS ${CheckpointNastiState.bits}U\n")
    sb.append(s"#define ${prefix}_SHADOW_RESTORE_SUPPORTED 0U\n")
    var offset = controlBits
    CheckpointLiveWidgetSnapshot.localFields.foreach { case (name, width) =>
      sb.append(s"// shadow local $name: bit $offset, width $width\n")
      offset += width
    }
    CheckpointNastiState.fields.foreach { case (name, width) =>
      sb.append(s"// shadow adapter $name: bit $offset, width $width\n")
      offset += width
    }
    cfg.ownerNames.zipWithIndex.foreach { case (owner, index) =>
      sb.append(s"// owner[$index]: $owner\n")
    }
    sb.toString
  }
}

case class CheckpointCaptureWidgetParams(entries: Int, manifestSha256: String,
    ownerNames: Seq[String], queueDepth: Int = 64,
    descriptors: Option[CheckpointCaptureDescriptors] = None, physicalClockGates: Int = 0,
    deferredGraft: Boolean = false) {
  require(!deferredGraft || (descriptors.isEmpty && physicalClockGates > 0 &&
    manifestSha256 == midas.checkpoint.NativeCheckpointWidgetReservation.digest),
    "checkpoint deferred graft requires the exact unbound reservation contract")
  require(entries > 0 && entries <= 65535, "checkpoint descriptor network is missing or too large")
  require(manifestSha256.matches("[0-9a-f]{64}") && BigInt(manifestSha256, 16) != 0,
    "checkpoint requires a nonzero generated manifest SHA256")
  require(ownerNames.nonEmpty && ownerNames.distinct.size == ownerNames.size &&
    ownerNames.forall(_.matches("[A-Za-z0-9_.:/-]+")), "checkpoint owners must be explicit, unique and named")
  require(queueDepth > 0)
  require(physicalClockGates >= 0 && physicalClockGates <= 65535,
    "physical clock count must fit the descriptor transport")
  require(physicalClockGates == 0 || entries > physicalClockGates,
    "physical capture requires functional and physical CE descriptors")
  descriptors.foreach { d =>
    require(d.entries.size == entries && d.manifestSha256 == manifestSha256,
      "checkpoint descriptor ROM does not match transport manifest/count")
    require(d.physicalClockGates == physicalClockGates,
      "checkpoint descriptor ROM physical CE count does not match transport mode; ordered bank-255 CE descriptors required")
    if (physicalClockGates > 0) {
      val physical = d.entries.takeRight(physicalClockGates)
      require(d.entries.dropRight(physicalClockGates).forall(_.bank != 255) &&
        physical.zipWithIndex.forall { case (e, i) =>
          e.bank == 255 && e.slot == i && e.width == 1 && e.depth == 1
        }, "physical capture requires ordered bank-255 CE descriptors and no functional alias")
    }
  }
}

/** Flat protocol matches CheckpointCaptureTransport.sv, including its dedicated reset.
  * Owners assert present only after a real binding exists; absent owners fail in the cut controller.
  */
class CheckpointCaptureTransportIO(owners: Int, controlStateBits: Int = 411) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val restore_hold = Input(Bool())
  val restore_load_pos = Input(Bool())
  val restore_state = Input(UInt(controlStateBits.W))
  val target_cycle = Input(UInt(64.W))
  val owner_present = Input(UInt(owners.W))
  val owner_frozen = Input(UInt(owners.W))
  val owner_error = Input(UInt(owners.W))
  val freeze = Output(Bool())
  val mmio_valid = Input(Bool())
  val mmio_write = Input(Bool())
  val mmio_ready = Output(Bool())
  val mmio_address = Input(UInt(8.W))
  val mmio_wdata = Input(UInt(32.W))
  val mmio_response_valid = Output(Bool())
  val mmio_response_ready = Input(Bool())
  val mmio_rdata = Output(UInt(32.W))
  val mmio_response_error = Output(Bool())
  val descriptor_entry = Output(UInt(32.W))
  val descriptor_valid = Input(Bool())
  val descriptor_bank = Input(UInt(8.W))
  val descriptor_slot = Input(UInt(16.W))
  val descriptor_width = Input(UInt(16.W))
  val descriptor_depth = Input(UInt(32.W))
  val bank_request_valid = Output(Bool())
  val bank_request_ready = Input(Bool())
  val bank = Output(UInt(8.W))
  val slot = Output(UInt(16.W))
  val address = Output(UInt(32.W))
  val bank_response_valid = Input(Bool())
  val bank_response_ready = Output(Bool())
  val bank_response_data = Input(UInt(64.W))
  val bank_response_error = Input(Bool())
  val dma_valid = Output(Bool())
  val dma_ready = Input(Bool())
  val dma_data = Output(UInt(512.W))
  val dma_last = Output(Bool())
  val dma_abort = Output(Bool())
  // Observation only. The immutable widget shadow copies this vector without
  // establishing restore ownership or waiving external inventory rejection.
  val control_state = Output(UInt(controlStateBits.W))
}

class CheckpointCaptureTransportBlackBox(cfg: CheckpointCaptureWidgetParams)
    extends BlackBox(Map("OWNERS" -> cfg.ownerNames.size, "ENTRIES" -> cfg.entries,
      "RESTORE_ENABLE" -> 0, "MANIFEST_SHA256" -> BigInt(cfg.manifestSha256, 16))) with HasBlackBoxResource {
  override def desiredName = "CheckpointCaptureTransport"
  val io = IO(new CheckpointCaptureTransportIO(cfg.ownerNames.size))
  addResource("/CheckpointCaptureTransport.sv")
  addResource("/CheckpointCaptureCut.sv")
}

/** The native gates live in the compiler-generated external clock network.
  * Instantiating a second set here would freeze the wrong simulator clocks.
  */
class CheckpointPhysicalCaptureTransportIO(owners: Int, gates: Int)
    extends CheckpointCaptureTransportIO(owners, 481 + gates) {
  val restore_load_neg = Input(Bool())
  val ce = Input(UInt(gates.W))
  val root_clock = Output(Clock())
  val gate_clock = Output(UInt(gates.W))
  val clock_mask = Output(Bool())
  val captured_ce = Output(UInt(gates.W))
  val physical_stopped = Output(Bool())
  val physical_read_enable = Output(Bool())
  val physical_failed = Output(Bool())
  val physical_cut_cycle = Output(UInt(64.W))
  val physical_error = Output(UInt(3.W))
}

class CheckpointPhysicalCaptureTransportBlackBox(cfg: CheckpointCaptureWidgetParams)
    extends BlackBox(Map("GATES" -> cfg.physicalClockGates, "OWNERS" -> cfg.ownerNames.size,
      "ENTRIES" -> cfg.entries, "EXTERNAL_CLOCK_NETWORK" -> 1, "RESTORE_ENABLE" -> 0,
      "MANIFEST_SHA256" -> BigInt(cfg.manifestSha256, 16))) with HasBlackBoxResource {
  require(cfg.physicalClockGates > 0)
  override def desiredName = "CheckpointPhysicalCaptureTransport"
  val io = IO(new CheckpointPhysicalCaptureTransportIO(cfg.ownerNames.size, cfg.physicalClockGates))
  Seq("CheckpointPhysicalCaptureTransport.sv", "CheckpointPhysicalClockCut.sv",
    "CheckpointCaptureTransport.sv", "CheckpointCaptureCut.sv").foreach(r => addResource("/" + r))
}

class CheckpointPhysicalCaptureSideband(gates: Int) extends Bundle {
  val logicalCE = Input(UInt(gates.W))
  val clockMask = Output(Bool())
  val capturedCE = Output(UInt(gates.W))
  val stopped = Output(Bool())
  val readEnable = Output(Bool())
  val failed = Output(Bool())
  val cutCycle = Output(UInt(64.W))
  val error = Output(UInt(3.W))
  def connect(t: CheckpointPhysicalCaptureTransportIO): Unit = {
    t.ce := logicalCE
    clockMask := t.clock_mask; capturedCE := t.captured_ce
    stopped := t.physical_stopped; readEnable := t.physical_read_enable
    failed := t.physical_failed; cutCycle := t.physical_cut_cycle; error := t.physical_error
  }
}

/** One request at a time at the backend; AW and W can arrive in either order.
  * Unsupported AXI bursts are rejected without forwarding any operation. For a
  * malformed write burst, W beats are drained through LAST before one SLVERR.
  */
class CheckpointNastiAdapter(np: NastiParameters, restoreEnabled: Boolean = false) extends Module {
  require(np.dataBits == 32 && np.addrBits >= 8 && np.addrBits <= 64 && np.idBits > 0 && np.idBits <= 64)
  val io = IO(new Bundle {
    // Opt-in component port, absent in production. A supervisor must hold the
    // upstream AXI master and downstream response owner at this same epoch.
    val restore = if (restoreEnabled) Some(new Bundle {
      val hold = Input(Bool())
      val load = Input(Bool())
      val state = Input(UInt(CheckpointNastiState.bits.W))
      val accepted = Output(Bool())
      val error = Output(Bool())
    }) else None
    val controlState = Output(UInt(CheckpointNastiState.bits.W))
    val ctrl = Flipped(new NastiIO(np))
    val request = Decoupled(new Bundle {
      val write = Bool()
      val address = UInt(8.W)
      val data = UInt(32.W)
    })
    val response = Flipped(Decoupled(new Bundle {
      val data = UInt(32.W)
      val error = Bool()
    }))
  })
  val aw = Reg(new NastiWriteAddressChannel(np))
  val wd = Reg(new NastiWriteDataChannel(np))
  val ar = Reg(new NastiReadAddressChannel(np))
  val haveAW = RegInit(false.B)
  val haveW = RegInit(false.B)
  val haveAR = RegInit(false.B)
  val badW = RegInit(false.B)
  val busy = RegInit(false.B)
  val writing = RegInit(false.B)
  val responseValid = RegInit(false.B)
  val responseError = Reg(Bool())
  val responseData = Reg(UInt(32.W))
  val responseId = Reg(UInt(np.idBits.W))
  val errorReadRemaining = RegInit(0.U(8.W))
  val originalState =
    CheckpointNastiState.channel("aw", aw, CheckpointNastiState.addressFields) ++
    CheckpointNastiState.channel("wd", wd, CheckpointNastiState.dataFields) ++
    CheckpointNastiState.channel("ar", ar, CheckpointNastiState.addressFields) ++ Seq(
      "haveAW" -> haveAW, "haveW" -> haveW, "haveAR" -> haveAR,
      "badW" -> badW, "busy" -> busy, "writing" -> writing,
      "responseValid" -> responseValid, "responseError" -> responseError,
      "responseData" -> responseData, "responseId" -> responseId,
      "errorReadRemaining" -> errorReadRemaining)
  io.controlState := CheckpointNastiState.pack(originalState)
  // Even a malformed load outside hold must not handshake or partially write.
  val running = io.restore.map(r => !r.hold && !r.load).getOrElse(true.B)
  // No new transaction until its prior response has been consumed. AR may be
  // queued behind a partial write, but may not bypass that write's eventual W.
  // A pending read owns the backend request until it is accepted. An earlier
  // partial write must still be allowed to acquire its missing channel.
  val readOwnsRequest = haveAR && !haveAW && !haveW && !badW
  io.ctrl.aw.ready := running && !haveAW && !busy && !responseValid && !readOwnsRequest
  io.ctrl.w.ready := running && !haveW && !busy && !responseValid && !readOwnsRequest
  io.ctrl.ar.ready := running && !haveAR && !busy && !responseValid
  when(io.ctrl.aw.fire) { aw := io.ctrl.aw.bits; haveAW := true.B }
  when(io.ctrl.ar.fire) { ar := io.ctrl.ar.bits; haveAR := true.B }
  when(io.ctrl.w.fire) {
    wd := io.ctrl.w.bits
    haveW := io.ctrl.w.bits.last
    badW := badW || !io.ctrl.w.bits.last
  }
  val chooseWrite = haveAW && haveW
  val chooseRead = haveAR && !haveAW && !haveW && !badW
  val writeBad = aw.len =/= 0.U || aw.size =/= 2.U || aw.lock || aw.burst > 1.U ||
    aw.addr(1, 0) =/= 0.U || wd.strb =/= 15.U || badW
  val readBad = ar.len =/= 0.U || ar.size =/= 2.U || ar.lock || ar.burst > 1.U ||
    ar.addr(1, 0) =/= 0.U
  val candidate = running && !busy && !responseValid && (chooseWrite || chooseRead)
  val invalid = Mux(chooseWrite, writeBad, readBad)
  io.request.valid := candidate && !invalid
  io.request.bits.write := chooseWrite
  io.request.bits.address := Mux(chooseWrite, aw.addr, ar.addr)
  io.request.bits.data := wd.data
  when(candidate && (invalid || io.request.ready)) {
    writing := chooseWrite
    responseId := Mux(chooseWrite, aw.id, ar.id)
    errorReadRemaining := Mux(!chooseWrite && invalid, ar.len, 0.U)
    when(chooseWrite) { haveAW := false.B; haveW := false.B; badW := false.B }
      .otherwise { haveAR := false.B }
    when(invalid) {
      responseValid := true.B; responseError := true.B; responseData := 0.U
    }.otherwise { busy := true.B }
  }
  io.response.ready := running && busy && !responseValid
  when(io.response.fire) {
    busy := false.B
    responseValid := true.B
    responseError := io.response.bits.error
    responseData := io.response.bits.data
  }
  io.ctrl.b.valid := running && responseValid && writing
  io.ctrl.b.bits.id := responseId
  io.ctrl.b.bits.resp := Mux(responseError, 2.U, 0.U)
  io.ctrl.b.bits.user := 0.U
  io.ctrl.r.valid := running && responseValid && !writing
  io.ctrl.r.bits.id := responseId
  io.ctrl.r.bits.data := responseData
  io.ctrl.r.bits.resp := Mux(responseError, 2.U, 0.U)
  io.ctrl.r.bits.last := errorReadRemaining === 0.U
  io.ctrl.r.bits.user := 0.U
  when(io.ctrl.b.fire || (io.ctrl.r.fire && errorReadRemaining === 0.U)) { responseValid := false.B }
  when(io.ctrl.r.fire && errorReadRemaining =/= 0.U) { errorReadRemaining := errorReadRemaining - 1.U }
  io.restore.foreach { r =>
    r.accepted := r.load && r.hold && !reset.asBool && CheckpointNastiState.paddingValid(r.state, originalState)
    r.error := r.load && !r.accepted
    CheckpointNastiState.load(r.state, originalState, r.accepted)
  }
}

/** This is deliberately a Widget, not a BridgeModule: global state readout must
  * keep making progress when token-producing target and bridge state is frozen.
  * FPGATop must explicitly register this widget in its stream engine and connect
  * every sideband port to generated network/owner logic before enabling it.
  */
class CheckpointCaptureWidget(val cfg: CheckpointCaptureWidgetParams)(implicit p: Parameters)
    extends Widget with StreamToHostCPU {
  override val customSize = Some(BigInt(256))
  val toHostCPUQueueDepth = cfg.queueDepth
  // Like streamEnq, this port must be accessible from the containing module
  // without relying on an anonymous WidgetImp structural refinement.
  val capturePort = InModuleBody {
    val capture = IO(new CheckpointCaptureSideband(cfg.ownerNames.size, cfg.physicalClockGates))
    capture
  }
  lazy val module = new WidgetImp(this) {
    val io = IO(new WidgetIO)
    // Exposes descriptor/bank/owners/freeze, never a target-token port. MMIO and
    // stream members are wired internally below rather than left as top ports.
    val capture = capturePort.getWrappedValue
    val transport: CheckpointCaptureTransportIO = if (cfg.physicalClockGates > 0) {
      val physicalTransport = Module(new CheckpointPhysicalCaptureTransportBlackBox(cfg))
      physicalTransport.io.restore_load_neg := false.B
      capture.physical.get.connect(physicalTransport.io)
      physicalTransport.io
    } else Module(new CheckpointCaptureTransportBlackBox(cfg)).io
    transport.clock := clock
    transport.reset := reset.asBool
    // The component restore supervisor is parked outside this production
    // widget. Its inputs are inactive and the RTL restore mux is disabled.
    transport.restore_hold := false.B
    transport.restore_load_pos := false.B
    transport.restore_state := 0.U
    capture.connect(transport)
    cfg.descriptors.foreach { descriptors =>
      val rom = Module(new CheckpointCaptureDescriptorROM(descriptors))
      rom.io.entry := transport.descriptor_entry
      transport.descriptor_valid := rom.io.valid
      transport.descriptor_bank := rom.io.bank
      transport.descriptor_slot := rom.io.slot
      transport.descriptor_width := rom.io.bitWidth
      transport.descriptor_depth := rom.io.depth
    }
    capture.controllerReset := reset.asBool
    CheckpointWidgetEndpoint.build(p(CtrlNastiKey), io.ctrl, transport,
      streamEnq, capture.targetCycle,
      transport.freeze && capture.ownerPresent.andR && capture.ownerFrozen.andR &&
        !capture.ownerError.orR && capture.physical.map(p => p.readEnable &&
          !p.failed && p.cutCycle === capture.targetCycle).getOrElse(true.B))
    override def genHeader(base: BigInt, memoryRegions: Map[String, BigInt], sb: StringBuilder): Unit = {
      if (cfg.deferredGraft) {
        sb.append(midas.checkpoint.NativeCheckpointWidgetReservation.header(base, toHostStreamIdx))
      } else {
        sb.append(CheckpointCaptureABI.header(getWName.toUpperCase, base, cfg, toHostStreamIdx))
        // Unlike target bridge constructors, this global driver has no target
        // bridge id/plusargs. Emit the actual constructor signature explicitly.
        cfg.descriptors.foreach { descriptors =>
          sb.append("#ifdef GET_INCLUDES\n#include \"bridges/checkpoint_capture.h\"\n#endif\n")
          sb.append("#ifdef GET_BRIDGE_CONSTRUCTOR\n")
          sb.append(s"registry.add_widget(new checkpoint_capture_t(simif, *registry.get_stream_engine(), ${base}ULL, ${toHostStreamIdx}U,\n")
          sb.append(s"  ${descriptors.cppManifest}));\n#endif\n")
        }
      }
    }
  }
}

class CheckpointCaptureSideband(owners: Int, physicalClockGates: Int = 0) extends Bundle {
  val physical = if (physicalClockGates > 0) Some(new CheckpointPhysicalCaptureSideband(physicalClockGates)) else None
  val controllerReset = Output(Bool())
  val targetCycle = Input(UInt(64.W))
  val ownerPresent = Input(UInt(owners.W))
  val ownerFrozen = Input(UInt(owners.W))
  val ownerError = Input(UInt(owners.W))
  val freeze = Output(Bool())
  val descriptorEntry = Output(UInt(32.W))
  val descriptorValid = Input(Bool())
  val descriptorBank = Input(UInt(8.W))
  val descriptorSlot = Input(UInt(16.W))
  val descriptorWidth = Input(UInt(16.W))
  val descriptorDepth = Input(UInt(32.W))
  val bankRequestValid = Output(Bool())
  val bankRequestReady = Input(Bool())
  val bank = Output(UInt(8.W))
  val slot = Output(UInt(16.W))
  val address = Output(UInt(32.W))
  val bankResponseValid = Input(Bool())
  val bankResponseReady = Output(Bool())
  val bankResponseData = Input(UInt(64.W))
  val bankResponseError = Input(Bool())
  val streamLast = Output(Bool())
  val streamAbort = Output(Bool())
  def connect(t: CheckpointCaptureTransportIO): Unit = {
    t.target_cycle := targetCycle
    t.owner_present := ownerPresent; t.owner_frozen := ownerFrozen; t.owner_error := ownerError
    freeze := t.freeze
    descriptorEntry := t.descriptor_entry
    t.descriptor_valid := descriptorValid; t.descriptor_bank := descriptorBank
    t.descriptor_slot := descriptorSlot; t.descriptor_width := descriptorWidth; t.descriptor_depth := descriptorDepth
    bankRequestValid := t.bank_request_valid; t.bank_request_ready := bankRequestReady
    bank := t.bank; slot := t.slot; address := t.address
    t.bank_response_valid := bankResponseValid; bankResponseReady := t.bank_response_ready
    t.bank_response_data := bankResponseData; t.bank_response_error := bankResponseError
    streamLast := t.dma_last; streamAbort := t.dma_abort
  }
}
