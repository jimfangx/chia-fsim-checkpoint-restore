// See LICENSE for license details.
package midas.checkpoint

import chisel3._
import chisel3.util._
import firrtl.annotations.NoTargetAnnotation

/** Binding metadata is elaboration-time data, never simulator introspection. */
case class StateAccessEntry(stateID: String, bank: Int, slot: Int, width: Int, depth: Int) {
  require(stateID.matches("[0-9a-f]{64}"), "invalid StateID")
  require(bank >= 0 && bank < 256 && slot >= 0 && slot < 65536)
  require(width > 0 && width <= 64, "wide state requires chunking; unsupported")
  require(depth > 0, "empty memory unsupported")
}

/** Fail closed at the inventory boundary if an explicit client binding omits
  * state or disagrees with the pre-FAME semantic manifest.
  */
case class SemanticBankedAccessAnnotation(root: String, entries: Seq[StateAccessEntry])
    extends NoTargetAnnotation

class StateBankPort extends Bundle {
  val bank = Output(UInt(8.W))
  val slot = Output(UInt(16.W))
  val address = Output(UInt(32.W))
  val data = Input(UInt(64.W))
  val error = Input(Bool())
}

class StateAccessBeat extends Bundle {
  val entry = UInt(16.W)
  val address = UInt(32.W)
  val bitWidth = UInt(16.W)
  val data = UInt(64.W)
  val error = Bool()
  val last = Bool()
}

/** Level-2 capture controller. All target writes/resets must honor freeze.
  * Clients provide combinational logical reads, not physical RAM layout.
  * StateID order, then ascending memory addresses. Two-beat FIFO bounds the
  * combinational path; selectors advance only on enqueue. Freeze includes the
  * accepting request edge and lasts through consumption of the final beat.
  * Transport/controller registers belong outside the semantic DUT root.
  */
class BankedStateAccess(entries: Seq[StateAccessEntry]) extends Module {
  require(entries.nonEmpty && entries.size <= 65535)
  require(entries.map(_.stateID).distinct.size == entries.size, "duplicate StateID")
  require(entries.map(e => (e.bank, e.slot)).distinct.size == entries.size, "duplicate bank binding")
  require(entries.map(_.stateID) == entries.map(_.stateID).sorted, "bindings must use StateID order")
  val io = IO(new Bundle {
    val start = Input(Bool())
    val command = Input(UInt(2.W)) // 0 = complete semantic capture; others reject
    val clearError = Input(Bool())
    val startReady = Output(Bool())
    val busy = Output(Bool())
    val freeze = Output(Bool())
    val done = Output(Bool())
    val error = Output(UInt(3.W)) // bad command, request while busy, bank failure
    val cycle = Output(UInt(64.W)) // semantic edges, including functional reset
    val bank = new StateBankPort
    val stream = Decoupled(new StateAccessBeat)
  })
  val idle :: reading :: draining :: Nil = Enum(3)
  val mode = RegInit(idle)
  val index = RegInit(0.U(16.W))
  val address = RegInit(0.U(32.W))
  // The separate controller reset initializes edge zero. Its first edge is a
  // semantic edge too, so the following before-edge boundary has index one.
  // Functional target reset must not drive this reset or restart the epoch.
  val semanticCycle = RegInit(1.U(64.W))
  val captureCycle = RegInit(0.U(64.W))
  val errors = RegInit(0.U(3.W))
  val fifo = Module(new Queue(new StateAccessBeat, 2, pipe = false, flow = false))
  io.startReady := mode === idle && !reset.asBool
  io.busy := mode =/= idle
  val accept = io.start && io.startReady && io.command === 0.U
  io.freeze := mode =/= idle || accept
  when(!io.freeze) { semanticCycle := semanticCycle + 1.U }
  io.cycle := captureCycle
  io.done := fifo.io.deq.fire && fifo.io.deq.bits.last
  io.error := errors
  val badCommand = io.start && io.startReady && io.command =/= 0.U
  val busyRequest = io.start && mode =/= idle
  val bankFailure = mode === reading && fifo.io.enq.ready && io.bank.error
  val newErrors = Cat(bankFailure, busyRequest, badCommand)
  errors := Mux(io.clearError && mode === idle, 0.U, errors) | newErrors

  val banks = VecInit(entries.map(_.bank.U(8.W)))
  val slots = VecInit(entries.map(_.slot.U(16.W)))
  val widths = VecInit(entries.map(_.width.U(16.W)))
  val depths = VecInit(entries.map(_.depth.U(32.W)))
  io.bank.bank := banks(index)
  io.bank.slot := slots(index)
  io.bank.address := address
  val last = index === (entries.size - 1).U && address === depths(index) - 1.U
  fifo.io.enq.valid := mode === reading
  fifo.io.enq.bits.entry := index
  fifo.io.enq.bits.address := address
  fifo.io.enq.bits.bitWidth := widths(index)
  fifo.io.enq.bits.data := Mux(io.bank.error, 0.U, io.bank.data)
  fifo.io.enq.bits.error := io.bank.error
  fifo.io.enq.bits.last := last || io.bank.error
  io.stream <> fifo.io.deq

  when(accept) {
    mode := reading
    index := 0.U
    address := 0.U
    captureCycle := semanticCycle
  }
  when(fifo.io.enq.fire) {
    when(last || io.bank.error) { mode := draining }
      .elsewhen(address === depths(index) - 1.U) { index := index + 1.U; address := 0.U }
      .otherwise { address := address + 1.U }
  }
  when(io.done) { mode := idle }
}
