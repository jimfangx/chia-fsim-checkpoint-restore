// See LICENSE for license details.
package midas.widgets

import chisel3._
import chisel3.util._
import chisel3.experimental.noPrefix
import firesim.lib.nasti._

/** Explicit opt-in endpoint restore. The transport, stream engine, shadow and
  * upstream master are separately owned and must remain parked at this epoch.
  * This interface cannot admit an image or release any of those owners.
  */
class CheckpointWidgetEndpointRestore extends Bundle {
  val hold = Input(Bool())
  val load = Input(Bool())
  val state = Input(UInt(CheckpointWidgetEndpoint.bits.W))
  val epoch = Input(UInt(64.W))
  val expectedEpoch = Input(UInt(64.W))
  val ownersQuiescent = Input(Bool())
  val ownerError = Input(Bool())
  val accepted = Output(Bool())
  val error = Output(Bool())
}

/** Shared elaboration of the real widget equations, in the caller's original
  * module. Production and component use the same eight registers, adapter,
  * MMIO response arbitration and DMA accounting; no parallel model is used.
  * Restore controls are absent from the production caller.
  */
object CheckpointWidgetEndpoint {
  val bits = CheckpointLiveWidgetSnapshot.localBits + CheckpointNastiState.bits
  def build(np: NastiParameters, ctrl: NastiIO, transport: CheckpointCaptureTransportIO,
      streamEnq: DecoupledIO[UInt], targetCycle: UInt, cutValid: Bool,
      restore: Option[CheckpointWidgetEndpointRestore] = None, resetActive: Bool = false.B): UInt = noPrefix {
    val adapter = Module(new CheckpointNastiAdapter(np, restore.nonEmpty)).suggestName("adapter")
    adapter.io.ctrl <> ctrl
    val running = restore.map(r => !r.hold && !r.load && !resetActive).getOrElse(true.B)
    streamEnq.valid := running && transport.dma_valid
    streamEnq.bits := transport.dma_data
    transport.dma_ready := running && streamEnq.ready
    restore.foreach { r =>
      val a = adapter.io.restore.get
      // Any attempted load parks the interface, even when the certificate is
      // invalid; only the shared accepted edge can write either register set.
      a.hold := r.hold || r.load || resetActive
      a.load := r.load && r.hold && r.ownersQuiescent && !r.ownerError &&
        r.epoch === r.expectedEpoch
      a.state := r.state(bits - 1, CheckpointLiveWidgetSnapshot.localBits)
      r.accepted := a.accepted
      r.error := r.load && !a.accepted
    }
    val invalid = RegInit(false.B).suggestName("invalid")
    val producedFrames = RegInit(0.U(64.W)).suggestName("producedFrames")
    val producedSnapshot = RegInit(0.U(64.W)).suggestName("producedSnapshot")
    val hostDrainedFrames = RegInit(0.U(64.W)).suggestName("hostDrainedFrames")
    val counterOverflow = RegInit(false.B).suggestName("counterOverflow")
    // Cumulative counts cross capture boundaries. Abort cannot flush frames
    // already in the shared stream engine; the host must account for them all.
    when(running && streamEnq.fire) {
      when(producedFrames.andR) { counterOverflow := true.B; invalid := true.B }
        .otherwise { producedFrames := producedFrames + 1.U }
    }
    when(running && transport.dma_abort) { invalid := true.B }
    val req = adapter.io.request
    val isStatus = !req.bits.write && req.bits.address === 0x60.U
    val isAck = req.bits.write && req.bits.address === 0x64.U
    val blockedStart = req.bits.write && req.bits.address === 0.U && req.bits.data === 1.U && (invalid || counterOverflow)
    val producedLow = !req.bits.write && req.bits.address === 0x68.U
    val producedHigh = !req.bits.write && req.bits.address === 0x6c.U
    val drainedLow = req.bits.address === 0x70.U
    val drainedHigh = req.bits.address === 0x74.U
    val isShadow = CheckpointLiveWidgetSnapshot.registers.map { case (_, address) =>
      req.bits.address === address.U }.reduce(_ || _)
    val local = isStatus || isAck || blockedStart || producedLow || producedHigh || drainedLow || drainedHigh || isShadow
    val localValid = RegInit(false.B).suggestName("localValid")
    val localData = Reg(UInt(32.W)).suggestName("localData")
    val localError = Reg(Bool()).suggestName("localError")
    val snapshot = Module(new CheckpointLiveWidgetSnapshot(
      transport.control_state.getWidth + CheckpointLiveWidgetSnapshot.localBits + CheckpointNastiState.bits)).suggestName("snapshot")
    snapshot.io.live := Cat(adapter.io.controlState, localError, localData, localValid, counterOverflow,
      hostDrainedFrames, producedSnapshot, producedFrames, invalid, transport.control_state)
    snapshot.io.cycle := targetCycle
    // A freeze request alone is insufficient: all separately stopped owners
    // and (when present) the physical post-stop sampler must acknowledge it.
    snapshot.io.cutValid := cutValid && !transport.dma_abort && !invalid && !counterOverflow &&
      CheckpointLiveWidgetSnapshot.acknowledgedCut(transport.control_state, targetCycle)
    snapshot.io.access := running && req.fire && isShadow
    snapshot.io.write := req.bits.write
    snapshot.io.address := req.bits.address
    snapshot.io.data := req.bits.data
    val ackAllowed = req.bits.data === 1.U && !transport.dma_abort &&
      !transport.freeze && !transport.dma_valid && !counterOverflow &&
      hostDrainedFrames === producedFrames
    req.ready := Mux(local, !localValid, transport.mmio_ready)
    transport.mmio_valid := req.valid && !local
    transport.mmio_write := req.bits.write
    transport.mmio_address := req.bits.address
    transport.mmio_wdata := req.bits.data
    when(running && req.fire && local) {
      localValid := true.B
      localData := Mux(isShadow, snapshot.io.response, MuxCase(0.U, Seq(
        isStatus -> Cat(0.U(30.W), counterOverflow, invalid),
        producedLow -> producedFrames(31, 0), producedHigh -> producedSnapshot(63, 32),
        (drainedLow && !req.bits.write) -> hostDrainedFrames(31, 0),
        (drainedHigh && !req.bits.write) -> hostDrainedFrames(63, 32))))
      when(producedLow) { producedSnapshot := producedFrames }
      when(drainedLow && req.bits.write) { hostDrainedFrames := Cat(hostDrainedFrames(63, 32), req.bits.data) }
      when(drainedHigh && req.bits.write) { hostDrainedFrames := Cat(req.bits.data, hostDrainedFrames(31, 0)) }
      localError := blockedStart || (isAck && !ackAllowed) || (isShadow && snapshot.io.error)
      when(isAck && ackAllowed) { invalid := false.B }
    }
    adapter.io.response.valid := localValid || transport.mmio_response_valid
    adapter.io.response.bits.data := Mux(localValid, localData, transport.mmio_rdata)
    adapter.io.response.bits.error := Mux(localValid, localError, transport.mmio_response_error)
    transport.mmio_response_ready := adapter.io.response.ready && !localValid
    when(running && localValid && adapter.io.response.ready) { localValid := false.B }
    val originals = Seq("invalid" -> invalid, "producedFrames" -> producedFrames,
      "producedSnapshot" -> producedSnapshot, "hostDrainedFrames" -> hostDrainedFrames,
      "counterOverflow" -> counterOverflow, "localValid" -> localValid,
      "localData" -> localData, "localError" -> localError)
    require(originals.map { case (name, value) => name -> value.getWidth } ==
      CheckpointLiveWidgetSnapshot.localFields, "changed original widget register schema")
    restore.foreach { r =>
      var offset = 0
      originals.foreach { case (_, value) =>
        val data = r.state(offset + value.getWidth - 1, offset)
        when(r.accepted) {
          value match {
            case b: Bool => b := data.asBool
            case u: UInt => u := data
          }
        }
        offset += value.getWidth
      }
    }
    Cat(adapter.io.controlState, Cat(originals.reverse.map(_._2.asUInt)))
  }
}
