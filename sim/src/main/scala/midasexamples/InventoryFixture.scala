// See LICENSE for license details.
package firesim.midasexamples

import chisel3._
import org.chipsalliance.cde.config.Parameters

/** Executable counterpart of checkpoint/InventoryDUT.firrtl. No capture hardware. */
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
