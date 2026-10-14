// See LICENSE for license details.
package firesim.midasexamples

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

/** Semantic replay fixture: arrays/SRAM and a pipelined memory response under backpressure. */
class InventoryFixtureLeaf(checkpointAccess: Boolean = false, forwardedClock: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val forwarded = if (forwardedClock) Some(Output(Clock())) else None
    val freeze = if (checkpointAccess) Some(Input(Bool())) else None
    val data = Input(UInt(9.W))
    val value = Output(UInt(9.W))
  })
  io.forwarded.foreach(_ := clock)
  val frozen = io.freeze.getOrElse(false.B)
  withReset(reset.asBool && !frozen) {
    val saved = RegInit(0.U(9.W))
    when(!frozen) { saved := io.data }
    io.value := saved
  }
}

/** Stateless clock forwarding tests must resolve through concrete definitions,
  * including nested and sibling outputs, rather than inventing a root clock.
  */
class InventoryClockForwarder extends RawModule {
  val clock = IO(Input(Clock()))
  val forwarded = IO(Output(Clock()))
  forwarded := clock
}

class InventoryClockIOCell extends RawModule {
  val pad = IO(Input(UInt(1.W)))
  val value = IO(Output(UInt(1.W)))
  value := pad
}

class InventoryClockRelay extends RawModule {
  val clock = IO(Input(Clock()))
  val forwarded = IO(Output(Clock()))
  val leaf = Module(new InventoryClockForwarder)
  val pin = Module(new InventoryClockIOCell)
  leaf.clock := clock
  pin.pad := leaf.forwarded.asUInt
  forwarded := pin.value.asBool.asClock
}

class InventoryFixtureDUT(checkpointAccess: Boolean = false, forwardedClocks: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val checkpoint = if (checkpointAccess) Some(new Bundle {
      val freeze = Input(Bool())
      val bank = Flipped(new midas.checkpoint.StateBankPort)
    }) else None
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

  val frozen = io.checkpoint.map(_.freeze).getOrElse(false.B)
  withReset(reset.asBool && !frozen) {
    val counter = RegInit(0.U(9.W))
    val fsm = RegInit(0.U(2.W))
    val bank = Reg(Vec(3, UInt(9.W)))
    val sram = Mem(8, UInt(9.W))
    val a = Module(new InventoryFixtureLeaf(checkpointAccess))
    val b = Module(new InventoryFixtureLeaf(checkpointAccess, forwardedClock = forwardedClocks))
    if (forwardedClocks) {
      val relay = Module(new InventoryClockRelay)
      relay.clock := clock
      b.clock := relay.forwarded
      a.clock := b.io.forwarded.get
    }

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
    val requestData = sram.read(qAddr)
    when(!frozen) {
      when(io.reqValid && io.reqReady) {
        qValid := true.B
        qAddr := io.addr
      }
      when(qValid && !busy && !responseValid) {
        qValid := false.B
        busy := true.B
        delay := 2.U
        pendingData := requestData
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
    } // all ordinary register writes honor freeze
    when(io.wen && !frozen) { sram.write(io.addr, io.data) }
    a.io.freeze.foreach(_ := frozen)
    b.io.freeze.foreach(_ := frozen)
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

    io.checkpoint.foreach { cp =>
      // Four logical banks: local registers, each repeated leaf, and SRAM.
      // These ports are ordinary synthesizable wires/read muxes. No hierarchy
      // references, VPI, shadow state, or scan chain participates in capture.
      val locals = Seq(counter, fsm, bank(0), bank(1), bank(2), qValid,
        qAddr, busy, delay, pendingData, responseValid, responseData)
      val localData = VecInit(locals.map(x => 0.U(64.W) | x.asUInt))
      val ramData = sram.read(cp.bank.address(2, 0))
      cp.bank.data := 0.U
      cp.bank.error := true.B
      switch(cp.bank.bank) {
        is(0.U) {
          when(cp.bank.slot < locals.size.U && cp.bank.address === 0.U) {
            cp.bank.data := localData(cp.bank.slot)
            cp.bank.error := false.B
          }
        }
        is(1.U, 2.U) {
          when(cp.bank.slot === 0.U && cp.bank.address === 0.U) {
            cp.bank.data := Mux(cp.bank.bank === 1.U, a.io.value, b.io.value)
            cp.bank.error := false.B
          }
        }
        is(3.U) {
          when(cp.bank.slot === 0.U && cp.bank.address < 8.U) {
            cp.bank.data := ramData
            cp.bank.error := false.B
          }
        }
      }
    }
  } // frozen reset domain
}

class InventoryFixture(implicit p: Parameters)
    extends firesim.lib.testutils.PeekPokeHarness(() => new InventoryFixtureDUT)

class ForwardedClockInventoryFixture(implicit p: Parameters)
    extends firesim.lib.testutils.PeekPokeHarness(() => new InventoryFixtureDUT(forwardedClocks = true))
