// See LICENSE for license details.
package firesim.midasexamples

import chisel3._
import org.chipsalliance.cde.config.Parameters

/** Concrete pre-FAME memories with defined read policies and both supported
  * readwrite policies. Undefined-reader lowering is checked independently: GG
  * resolves its policy and changes its hidden owner, so the oracle rejects it.
  */
class MemoryPipelineFixtureDUT extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val write = Input(Bool())
    val address = Input(UInt(3.W))
    val data = Input(UInt(9.W))
    val readerOld = Output(UInt(9.W))
    val readerNew = Output(UInt(9.W))
    val readwriterOld = Output(UInt(9.W))
    val readwriterUndefined = Output(UInt(9.W))
    val count = Output(UInt(9.W))
  })
  val count = RegInit(0.U(9.W))
  count := count + 1.U
  io.count := count

  val old = SyncReadMem(8, UInt(9.W), SyncReadMem.ReadFirst)
  val fresh = SyncReadMem(8, UInt(9.W), SyncReadMem.WriteFirst)
  io.readerOld := old.read(io.address, io.enable)
  io.readerNew := fresh.read(io.address, io.enable)
  when(io.enable && io.write) {
    old.write(io.address, io.data)
    fresh.write(io.address, io.data)
  }
  val rwOld = SyncReadMem(8, UInt(9.W), SyncReadMem.ReadFirst)
  val rwUndefined = SyncReadMem(8, UInt(9.W), SyncReadMem.Undefined)
  io.readwriterOld := rwOld.readWrite(io.address, io.data, io.enable, io.write)
  io.readwriterUndefined := rwUndefined.readWrite(io.address, io.data, io.enable, io.write)
}

class MemoryPipelineFixture(implicit p: Parameters)
    extends firesim.lib.testutils.PeekPokeHarness(() => new MemoryPipelineFixtureDUT)
