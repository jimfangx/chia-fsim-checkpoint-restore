// See LICENSE for license details.
package firesim.midasexamples

import chisel3._
import org.chipsalliance.cde.config.Parameters

/** Semantic replay fixture: arrays/SRAM and a pipelined memory response under backpressure. */
class InventoryFixtureLeaf extends Module {
  val io = IO(new Bundle {
    val data = Input(UInt(9.W))
    val value = Output(UInt(9.W))
  })
  val saved = RegInit(0.U(9.W))
  saved := io.data
  io.value := saved
}

class InventoryFixtureDUT extends Module {
  val io = IO(new Bundle {
    val data = Input(UInt(9.W))
    val addr = Input(UInt(3.W))
    val wen = Input(Bool())
    val reqValid = Input(Bool())
    val respReady = Input(Bool())
    val reqReady = Output(Bool())
    val respValid = Output(Bool())
    val respData = Output(UInt(9.W))
    val queued = Output(Bool())
    val outstanding = Output(Bool())
    val count = Output(UInt(9.W))
    val phase = Output(UInt(2.W))
    val array0 = Output(UInt(9.W))
    val array1 = Output(UInt(9.W))
    val array2 = Output(UInt(9.W))
    val left = Output(UInt(9.W))
    val right = Output(UInt(9.W))
    val ram = Output(UInt(9.W))
  })

  val counter = RegInit(0.U(9.W))
  val fsm = RegInit(0.U(2.W))
  val bank = Reg(Vec(3, UInt(9.W)))
  val sram = Mem(8, UInt(9.W))
  val a = Module(new InventoryFixtureLeaf)
  val b = Module(new InventoryFixtureLeaf)

  // One request queue, a two-cycle memory operation, and a held response.
  // These are ordinary target registers, never drained for a checkpoint.
  val qValid = RegInit(false.B)
  val qAddr = Reg(UInt(3.W))
  val busy = RegInit(false.B)
  val delay = Reg(UInt(2.W))
  val pendingData = Reg(UInt(9.W))
  val responseValid = RegInit(false.B)
  val responseData = Reg(UInt(9.W))
  io.reqReady := !qValid
  io.queued := qValid
  io.outstanding := busy
  io.respValid := responseValid
  io.respData := responseData
  when(io.reqValid && io.reqReady) {
    qValid := true.B
    qAddr := io.addr
  }
  when(qValid && !busy && !responseValid) {
    qValid := false.B
    busy := true.B
    delay := 2.U
    pendingData := sram.read(qAddr)
  }
  when(busy) {
    delay := delay - 1.U
    when(delay === 1.U) {
      busy := false.B
      responseValid := true.B
      responseData := pendingData
    }
  }
  when(responseValid && io.respReady) { responseValid := false.B }

  counter := counter + 1.U
  fsm := Mux(fsm === 2.U, 0.U, fsm + 1.U)
  for (i <- 0 until 3) { bank(i) := io.data + i.U }
  when(io.wen) { sram.write(io.addr, io.data) }
  a.io.data := io.data
  b.io.data := io.data + 1.U

  io.count := counter
  io.phase := fsm
  io.array0 := bank(0)
  io.array1 := bank(1)
  io.array2 := bank(2)
  io.left := a.io.value
  io.right := b.io.value
  io.ram := sram.read(io.addr)
}

class InventoryFixture(implicit p: Parameters)
    extends firesim.lib.testutils.PeekPokeHarness(() => new InventoryFixtureDUT)
