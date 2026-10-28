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
class CheckpointEndpointComponent(restoreEnabled: Boolean, idBits: Int,
    shadowEnabled: Boolean = false, controlBits: Int = 411) extends Module {
  require(!shadowEnabled || restoreEnabled)
  val io = IO(new Bundle {
    val ctrl = Flipped(new NastiIO(NastiParameters(32, 32, idBits)))
    val restore = if (restoreEnabled) Some(new CheckpointWidgetEndpointRestore(
      if (shadowEnabled) controlBits + CheckpointWidgetEndpoint.bits else 0)) else None
    val control = if (shadowEnabled) Some(Input(UInt(controlBits.W))) else None
    val cutValid = if (shadowEnabled) Some(Input(Bool())) else None
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
  val transport = Wire(new CheckpointCaptureTransportIO(1, controlBits))
  transport := DontCare
  transport.control_state := io.control.getOrElse(0.U)
  transport.dma_valid := io.dmaValid
  transport.dma_data := io.dmaData
  transport.dma_abort := io.dmaAbort
  transport.freeze := io.frozen
  transport.mmio_ready := io.backendReady
  transport.mmio_response_valid := io.responseValid
  transport.mmio_rdata := io.responseData
  transport.mmio_response_error := io.responseError
  io.state := CheckpointWidgetEndpoint.build(NastiParameters(32, 32, idBits), io.ctrl,
    transport, io.stream, 42.U(64.W), io.cutValid.getOrElse(false.B), io.restore, reset.asBool)
  io.dmaReady := transport.dma_ready
  io.backendValid := transport.mmio_valid
  io.backendWrite := transport.mmio_write
  io.backendAddress := transport.mmio_address
  io.backendData := transport.mmio_wdata
  io.responseReady := transport.mmio_response_ready
}

class CheckpointEndpointShadowComponent(idBits: Int, controlBits: Int)
    extends CheckpointEndpointComponent(true, idBits, true, controlBits)

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
  it should "restore the original shadow bank at the same endpoint epoch without adding storage" in {
    val root = Paths.get(sys.props("checkpoint.widget.out")).resolve("endpoint-shadow")
    for ((idBits, controlBits) <- Seq(4 -> 411, 12 -> 487)) {
      val out = root.resolve(idBits.toString); Files.createDirectories(out)
      new ChiselStage().emitVerilog(new CheckpointEndpointShadowComponent(idBits, controlBits),
        Array("--target-dir", out.toString))
      val low = new Compiler(Forms.LowForm).execute(CircuitState(
        Parser.parse(Files.readString(out.resolve("CheckpointEndpointShadowComponent.fir"))), ChirrtlForm))
      val snapshot = low.circuit.modules.collectFirst {
        case m: firrtl.ir.Module if m.name == "CheckpointLiveWidgetSnapshot" => m }.get
      val regs = scala.collection.mutable.Map.empty[String, Int]
      def visit(s: Statement): Unit = {
        s match { case r: DefRegister => regs += r.name -> r.tpe.asInstanceOf[UIntType].width.asInstanceOf[IntWidth].width.toInt
          case _ => () }; s.foreachStmt(visit)
      }
      visit(snapshot.body)
      assert(regs.toMap == Map("occupied" -> 1, "saved" -> (controlBits + 758), "label" -> 64, "index" -> 32))
      assert(snapshot.ports.exists(_.name == "io_restore_state"))
    }
  }
}
