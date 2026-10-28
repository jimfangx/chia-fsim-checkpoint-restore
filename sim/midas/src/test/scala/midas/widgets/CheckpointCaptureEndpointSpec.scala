// See LICENSE for license details.
package midas.widgets

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import firesim.lib.nasti._
import firrtl.{CircuitState, ChirrtlForm, Parser}
import firrtl.ir.{DefRegister, IntWidth, Statement, UIntType}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import java.nio.file.{Files, Paths}
import org.scalatest.flatspec.AnyFlatSpec

/** The actual production builder with an explicit environment interface.
  * Transport/controller/shadow/stream-engine state is deliberately not claimed.
  */
class CheckpointEndpointComponent(restoreEnabled: Boolean, idBits: Int) extends Module {
  val io = IO(new Bundle {
    val ctrl = Flipped(new NastiIO(NastiParameters(32, 32, idBits)))
    val restore = if (restoreEnabled) Some(new CheckpointWidgetEndpointRestore) else None
    val state = Output(UInt(CheckpointWidgetEndpoint.bits.W))
    val stream = Decoupled(UInt(512.W))
    val dmaValid = Input(Bool())
    val dmaData = Input(UInt(512.W))
    val dmaReady = Output(Bool())
    val dmaAbort = Input(Bool())
    val frozen = Input(Bool())
    val backendReady = Input(Bool())
    val backendValid = Output(Bool())
    val backendWrite = Output(Bool())
    val backendAddress = Output(UInt(8.W))
    val backendData = Output(UInt(32.W))
    val responseValid = Input(Bool())
    val responseReady = Output(Bool())
    val responseData = Input(UInt(32.W))
    val responseError = Input(Bool())
  })
  val transport = Wire(new CheckpointCaptureTransportIO(1))
  transport := DontCare
  transport.control_state := 0.U
  transport.dma_valid := io.dmaValid
  transport.dma_data := io.dmaData
  transport.dma_abort := io.dmaAbort
  transport.freeze := io.frozen
  transport.mmio_ready := io.backendReady
  transport.mmio_response_valid := io.responseValid
  transport.mmio_rdata := io.responseData
  transport.mmio_response_error := io.responseError
  io.state := CheckpointWidgetEndpoint.build(NastiParameters(32, 32, idBits), io.ctrl,
    transport, io.stream, 42.U(64.W), false.B, io.restore, reset.asBool)
  io.dmaReady := transport.dma_ready
  io.backendValid := transport.mmio_valid
  io.backendWrite := transport.mmio_write
  io.backendAddress := transport.mmio_address
  io.backendData := transport.mmio_wdata
  io.responseReady := transport.mmio_response_ready
}

class CheckpointCaptureEndpointSpec extends AnyFlatSpec {
  behavior.of("Original widget endpoint common epoch")
  it should "emit the shared original local equations and adapter with optional atomic restore" in {
    val root = Paths.get(sys.props("checkpoint.widget.out")).resolve("endpoint-state")
    for (idBits <- Seq(4, 12); restore <- Seq(false, true)) {
      val out = root.resolve(idBits.toString).resolve(if (restore) "restore" else "production"); Files.createDirectories(out)
      new ChiselStage().emitVerilog(new CheckpointEndpointComponent(restore, idBits), Array("--target-dir", out.toString))
      val low = new Compiler(Forms.LowForm).execute(CircuitState(
        Parser.parse(Files.readString(out.resolve("CheckpointEndpointComponent.fir"))), ChirrtlForm))
      val module = low.circuit.modules.collectFirst { case m: firrtl.ir.Module if m.name == "CheckpointEndpointComponent" => m }.get
      val registers = scala.collection.mutable.Map.empty[String, Int]
      def visit(s: Statement): Unit = {
        s match { case r: DefRegister => registers += r.name -> r.tpe.asInstanceOf[UIntType].width.asInstanceOf[IntWidth].width.toInt
          case _ => () }
        s.foreachStmt(visit)
      }
      visit(module.body)
      assert(registers.toMap == CheckpointLiveWidgetSnapshot.localFields.toMap,
        "endpoint must contain exactly all eight original local registers")
      assert(module.ports.exists(_.name == "io_restore_hold") == restore)
      assert(CheckpointWidgetEndpoint.bits == 758)
    }
  }
}
