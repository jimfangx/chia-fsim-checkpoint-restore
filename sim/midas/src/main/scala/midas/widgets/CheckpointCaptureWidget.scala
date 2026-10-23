// See LICENSE for license details.
package midas.widgets

import chisel3._
import chisel3.util._
import chisel3.util.HasBlackBoxResource
import org.chipsalliance.cde.config.Parameters
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
      "HOST_DRAINED_FRAMES_LO" -> 0x70, "HOST_DRAINED_FRAMES_HI" -> 0x74)

  def header(prefix: String, base: BigInt, cfg: CheckpointCaptureWidgetParams, stream: Int): String = {
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
    sb.append(s"#define ${prefix}_MANIFEST_SHA256 \"${cfg.manifestSha256}\"\n")
    Seq("CheckpointCaptureTransport.sv", "CheckpointCaptureCut.sv").foreach { resource =>
      val in = getClass.getResourceAsStream("/" + resource)
      require(in != null, s"missing checkpoint RTL resource: $resource")
      val bytes = try in.readAllBytes() finally in.close()
      val sha = java.security.MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 255}%02x").mkString
      sb.append(s"// $resource SHA256: $sha\n")
    }
    sb.append(s"#define ${prefix}_FRAME_BYTES 64U\n#define ${prefix}_TRANSPORT_VERSION 1U\n")
    sb.append(s"#define ${prefix}_ABORT_REQUIRES_HOST_DRAIN 1U\n")
    cfg.ownerNames.zipWithIndex.foreach { case (owner, index) =>
      sb.append(s"// owner[$index]: $owner\n")
    }
    sb.toString
  }
}

case class CheckpointCaptureWidgetParams(entries: Int, manifestSha256: String,
    ownerNames: Seq[String], queueDepth: Int = 64) {
  require(entries > 0, "checkpoint descriptor network is missing")
  require(manifestSha256.matches("[0-9a-f]{64}") && BigInt(manifestSha256, 16) != 0,
    "checkpoint requires a nonzero generated manifest SHA256")
  require(ownerNames.nonEmpty && ownerNames.distinct.size == ownerNames.size &&
    ownerNames.forall(_.matches("[A-Za-z0-9_.:/-]+")), "checkpoint owners must be explicit, unique and named")
  require(queueDepth > 0)
}

/** Flat protocol matches CheckpointCaptureTransport.sv, including its dedicated reset.
  * Owners assert present only after a real binding exists; absent owners fail in the cut controller.
  */
class CheckpointCaptureTransportIO(owners: Int) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
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
}

class CheckpointCaptureTransportBlackBox(cfg: CheckpointCaptureWidgetParams)
    extends BlackBox(Map("OWNERS" -> cfg.ownerNames.size, "ENTRIES" -> cfg.entries,
      "MANIFEST_SHA256" -> BigInt(cfg.manifestSha256, 16))) with HasBlackBoxResource {
  override def desiredName = "CheckpointCaptureTransport"
  val io = IO(new CheckpointCaptureTransportIO(cfg.ownerNames.size))
  addResource("/CheckpointCaptureTransport.sv")
  addResource("/CheckpointCaptureCut.sv")
}

/** One request at a time at the backend; AW and W can arrive in either order.
  * Unsupported AXI bursts are rejected without forwarding any operation. For a
  * malformed write burst, W beats are drained through LAST before one SLVERR.
  */
class CheckpointNastiAdapter(np: NastiParameters) extends Module {
  require(np.dataBits == 32 && np.addrBits >= 8)
  val io = IO(new Bundle {
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
  // No new transaction until its prior response has been consumed. AR may be
  // queued behind a partial write, but may not bypass that write's eventual W.
  // A pending read owns the backend request until it is accepted. An earlier
  // partial write must still be allowed to acquire its missing channel.
  val readOwnsRequest = haveAR && !haveAW && !haveW && !badW
  io.ctrl.aw.ready := !haveAW && !busy && !responseValid && !readOwnsRequest
  io.ctrl.w.ready := !haveW && !busy && !responseValid && !readOwnsRequest
  io.ctrl.ar.ready := !haveAR && !busy && !responseValid
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
  val candidate = !busy && !responseValid && (chooseWrite || chooseRead)
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
  io.response.ready := busy && !responseValid
  when(io.response.fire) {
    busy := false.B
    responseValid := true.B
    responseError := io.response.bits.error
    responseData := io.response.bits.data
  }
  io.ctrl.b.valid := responseValid && writing
  io.ctrl.b.bits.id := responseId
  io.ctrl.b.bits.resp := Mux(responseError, 2.U, 0.U)
  io.ctrl.b.bits.user := 0.U
  io.ctrl.r.valid := responseValid && !writing
  io.ctrl.r.bits.id := responseId
  io.ctrl.r.bits.data := responseData
  io.ctrl.r.bits.resp := Mux(responseError, 2.U, 0.U)
  io.ctrl.r.bits.last := errorReadRemaining === 0.U
  io.ctrl.r.bits.user := 0.U
  when(io.ctrl.b.fire || (io.ctrl.r.fire && errorReadRemaining === 0.U)) { responseValid := false.B }
  when(io.ctrl.r.fire && errorReadRemaining =/= 0.U) { errorReadRemaining := errorReadRemaining - 1.U }
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
  lazy val module = new WidgetImp(this) {
    val io = IO(new WidgetIO)
    // Exposes descriptor/bank/owners/freeze, never a target-token port. MMIO and
    // stream members are wired internally below rather than left as top ports.
    val capture = IO(new CheckpointCaptureSideband(cfg.ownerNames.size))
    val transport = Module(new CheckpointCaptureTransportBlackBox(cfg))
    val adapter = Module(new CheckpointNastiAdapter(p(CtrlNastiKey)))
    adapter.io.ctrl <> io.ctrl
    transport.io.clock := clock
    transport.io.reset := reset.asBool
    capture.connect(transport.io)
    capture.controllerReset := reset.asBool
    streamEnq.valid := transport.io.dma_valid
    streamEnq.bits := transport.io.dma_data
    transport.io.dma_ready := streamEnq.ready
    val invalid = RegInit(false.B)
    val producedFrames = RegInit(0.U(64.W))
    val producedSnapshot = RegInit(0.U(64.W))
    val hostDrainedFrames = RegInit(0.U(64.W))
    val counterOverflow = RegInit(false.B)
    // Cumulative counts cross capture boundaries. Abort cannot flush frames
    // already in the shared stream engine; the host must account for them all.
    when(streamEnq.fire) {
      when(producedFrames.andR) { counterOverflow := true.B; invalid := true.B }
        .otherwise { producedFrames := producedFrames + 1.U }
    }
    when(transport.io.dma_abort) { invalid := true.B }
    val req = adapter.io.request
    val isStatus = !req.bits.write && req.bits.address === 0x60.U
    val isAck = req.bits.write && req.bits.address === 0x64.U
    val blockedStart = req.bits.write && req.bits.address === 0.U && req.bits.data === 1.U && (invalid || counterOverflow)
    val producedLow = !req.bits.write && req.bits.address === 0x68.U
    val producedHigh = !req.bits.write && req.bits.address === 0x6c.U
    val drainedLow = req.bits.address === 0x70.U
    val drainedHigh = req.bits.address === 0x74.U
    val local = isStatus || isAck || blockedStart || producedLow || producedHigh || drainedLow || drainedHigh
    val localValid = RegInit(false.B)
    val localData = Reg(UInt(32.W))
    val localError = Reg(Bool())
    val ackAllowed = req.bits.data === 1.U && !transport.io.dma_abort &&
      !transport.io.freeze && !transport.io.dma_valid && !counterOverflow &&
      hostDrainedFrames === producedFrames
    req.ready := Mux(local, !localValid, transport.io.mmio_ready)
    transport.io.mmio_valid := req.valid && !local
    transport.io.mmio_write := req.bits.write
    transport.io.mmio_address := req.bits.address
    transport.io.mmio_wdata := req.bits.data
    when(req.fire && local) {
      localValid := true.B
      localData := MuxCase(0.U, Seq(
        isStatus -> Cat(0.U(30.W), counterOverflow, invalid),
        producedLow -> producedFrames(31, 0), producedHigh -> producedSnapshot(63, 32),
        (drainedLow && !req.bits.write) -> hostDrainedFrames(31, 0),
        (drainedHigh && !req.bits.write) -> hostDrainedFrames(63, 32)))
      when(producedLow) { producedSnapshot := producedFrames }
      when(drainedLow && req.bits.write) { hostDrainedFrames := Cat(hostDrainedFrames(63, 32), req.bits.data) }
      when(drainedHigh && req.bits.write) { hostDrainedFrames := Cat(req.bits.data, hostDrainedFrames(31, 0)) }
      localError := blockedStart || (isAck && !ackAllowed)
      when(isAck && ackAllowed) { invalid := false.B }
    }
    adapter.io.response.valid := localValid || transport.io.mmio_response_valid
    adapter.io.response.bits.data := Mux(localValid, localData, transport.io.mmio_rdata)
    adapter.io.response.bits.error := Mux(localValid, localError, transport.io.mmio_response_error)
    transport.io.mmio_response_ready := adapter.io.response.ready && !localValid
    when(localValid && adapter.io.response.ready) { localValid := false.B }
    override def genHeader(base: BigInt, memoryRegions: Map[String, BigInt], sb: StringBuilder): Unit = {
      sb.append(CheckpointCaptureABI.header(getWName.toUpperCase, base, cfg, toHostStreamIdx))
    }
  }
}

class CheckpointCaptureSideband(owners: Int) extends Bundle {
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
