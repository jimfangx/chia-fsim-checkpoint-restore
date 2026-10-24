// See LICENSE for license details.
package midas.core

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations.ReferenceTarget
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import firesim.lib.bridgeutils.{BridgeIOAnnotation, RationalClock}
import midas.checkpoint.{GeneratedStateBankAccess, SemanticStateInventory}
import midas.passes.fame.{FAMEChannelConnectionAnnotation, TargetClockChannel}
import midas.widgets._
import org.scalatest.flatspec.AnyFlatSpec

/** An existing bridge stream must retain index zero when global capture is added.
  * This is only a fixture clock source and constant diagnostic stream.
  */
class CaptureTopFixtureClock(implicit p: Parameters)
    extends BridgeModule[ClockTokenVector] with StreamToHostCPU {
  val toHostCPUQueueDepth = 2
  lazy val module = new BridgeModuleImp(this) {
    val io = IO(new WidgetIO)
    val hPort = IO(new ClockTokenVector(1))
    hPort.clocks.valid := true.B
    hPort.clocks.bits(0) := true.B
    streamEnq.valid := true.B
    streamEnq.bits := BigInt("a55ac33c", 16).U
    genROReg(0.U(32.W), "fixture")
    genCRFile()
    def genHeader(base: BigInt, memoryRegions: Map[String, BigInt], sb: StringBuilder): Unit = ()
  }
}

class CaptureTopFixtureStorage extends BlackBox {
  override def desiredName = "CaptureTopStorage"
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val addr = Input(UInt(5.W))
    val data = Input(UInt(32.W))
    val wen = Input(Bool())
    val count = Output(UInt(64.W))
    val ram = Output(UInt(32.W))
    val ckpt_freeze = Input(Bool())
    val ckpt_bank = Input(UInt(8.W))
    val ckpt_slot = Input(UInt(16.W))
    val ckpt_address = Input(UInt(32.W))
    val ckpt_match = Output(Bool())
    val ckpt_data = Output(UInt(64.W))
    val ckpt_reset = Input(Bool())
    val ckpt_req_valid = Input(Bool())
    val ckpt_req_ready = Output(Bool())
    val ckpt_resp_valid = Output(Bool())
    val ckpt_resp_ready = Input(Bool())
    val ckpt_resp_data = Output(UInt(64.W))
    val ckpt_resp_error = Output(Bool())
  })
}

class CheckpointCaptureTopSpec extends AnyFlatSpec {
  private val source = """circuit CaptureTopStorage :
  module CaptureTopStorage :
    input clock : Clock
    input reset : UInt<1>
    input addr : UInt<5>
    input data : UInt<32>
    input wen : UInt<1>
    output count : UInt<64>
    output ram : UInt<32>
    reg counter : UInt<64>, clock with :
      reset => (reset, UInt<64>(0))
    counter <= add(counter, UInt<1>(1))
    count <= counter
    mem sram :
      data-type => UInt<32>
      depth => 32
      reader => r
      writer => w
      read-latency => 0
      write-latency => 1
      read-under-write => undefined
    sram.r.clk <= clock
    sram.r.en <= UInt<1>(1)
    sram.r.addr <= addr
    sram.w.clk <= clock
    sram.w.en <= wen
    sram.w.addr <= addr
    sram.w.data <= data
    sram.w.mask <= UInt<1>(1)
    ram <= sram.r.data
"""
  private val state = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(source), ChirrtlForm)).copy(form = LowForm)
  private val manifest = SemanticStateInventory.discover(state, state.circuit.main)
  private val generated = GeneratedStateBankAccess.generate(state, manifest)
  private val descriptors = CheckpointCaptureDescriptors.fromGenerated(generated, manifest)
  private val cfg = CheckpointCaptureWidgetParams(descriptors.entries.size, descriptors.manifestSha256,
    Seq("fixture.storage", "fixture.host"), queueDepth = 2, descriptors = Some(descriptors))
  private val out = sys.props.get("checkpoint.widget.out").map(Paths.get(_).resolve("top"))
    .getOrElse(Files.createTempDirectory("checkpoint-top-"))
  Files.createDirectories(out)

  private class FixtureBinding extends CheckpointCaptureTopBinding {
    val params = cfg
    def connect(top: FPGATopImp, capture: CheckpointCaptureSideband): Unit = {
      val functionalReset = IO(Input(Bool())).suggestName("fixtureReset")
      val addr = IO(Input(UInt(5.W))).suggestName("fixtureAddr")
      val data = IO(Input(UInt(32.W))).suggestName("fixtureData")
      val wen = IO(Input(Bool())).suggestName("fixtureWen")
      val hostPresent = IO(Input(Bool())).suggestName("fixtureHostPresent")
      val hostFrozen = IO(Input(Bool())).suggestName("fixtureHostFrozen")
      val hostError = IO(Input(Bool())).suggestName("fixtureHostError")
      val corrupt = IO(Input(Bool())).suggestName("fixtureCorrupt")
      val count = IO(Output(UInt(64.W))).suggestName("fixtureCount")
      val ram = IO(Output(UInt(32.W))).suggestName("fixtureRam")
      val frozen = IO(Output(Bool())).suggestName("fixtureFreeze")
      val storage = Module(new CaptureTopFixtureStorage)
      storage.io.clock := top.clock
      storage.io.reset := functionalReset
      storage.io.addr := addr
      storage.io.data := data
      storage.io.wen := wen
      storage.io.ckpt_reset := capture.controllerReset
      storage.io.ckpt_freeze := capture.freeze
      storage.io.ckpt_bank := capture.bank
      storage.io.ckpt_slot := capture.slot
      storage.io.ckpt_address := capture.address
      storage.io.ckpt_req_valid := capture.bankRequestValid
      storage.io.ckpt_resp_ready := capture.bankResponseReady
      capture.bankRequestReady := storage.io.ckpt_req_ready
      capture.bankResponseValid := storage.io.ckpt_resp_valid
      capture.bankResponseData := storage.io.ckpt_resp_data ^ corrupt.asUInt
      capture.bankResponseError := storage.io.ckpt_resp_error
      capture.targetCycle := storage.io.count
      capture.ownerPresent := Cat(hostPresent, true.B)
      capture.ownerFrozen := Cat(hostFrozen && capture.freeze, capture.freeze)
      capture.ownerError := Cat(hostError, false.B)
      // Internal ROM owns the descriptor inputs in registered top-level mode.
      capture.descriptorValid := false.B
      capture.descriptorBank := 0.U; capture.descriptorSlot := 0.U
      capture.descriptorWidth := 0.U; capture.descriptorDepth := 0.U
      count := storage.io.count; ram := storage.io.ram; frozen := capture.freeze
    }
  }

  private def parameters(binding: Option[CheckpointCaptureTopBinding]): Parameters = {
    val toCPU = new StreamAllocator
    val fromCPU = new StreamAllocator
    val ref = ReferenceTarget("Fixture", "Target", Nil, "clock_bits", Nil)
    val clock = FAMEChannelConnectionAnnotation("clock", TargetClockChannel(Seq(RationalClock("base", 1, 1)), Seq(1)),
      None, None, Some(Seq(ref)))
    val bridge = BridgeIOAnnotation(ReferenceTarget("Fixture", "Target", Nil, "clockbridge", Nil),
      Map.empty[String, String], None, classOf[CaptureTopFixtureClock].getName)
    (new midas.F2Config).toInstance.alterPartial {
      case SimWrapperKey => SimWrapperConfig(Seq(clock, bridge), Map.empty)
      case CheckpointCaptureTopKey => binding
      case ToCPUStreamAllocatorKey => toCPU
      case FromCPUStreamAllocatorKey => fromCPU
    }
  }

  behavior.of("FPGATop global checkpoint registration")

  it should "reject physical mode with a functional-only immutable descriptor ROM" in {
    val error = intercept[IllegalArgumentException] { cfg.copy(physicalClockGates = 1) }
    assert(error.getMessage.contains("bank-255 CE descriptors"))
  }

  it should "preserve stock opt-out and existing bridge stream allocation" in {
    implicit val p: Parameters = parameters(None)
    val top = LazyModule(new FPGATop)
    val rtl = (new ChiselStage).emitVerilog(top.module, Array("--target-dir", out.resolve("disabled").toString))
    assert(top.checkpointCapture.isEmpty && top.toCPUStreamParams.map(_.idx) == Seq(0))
    assert(!rtl.contains("CheckpointCaptureTransport"))
    assert((new midas.F2Config).toInstance(CheckpointCaptureTopKey).isEmpty)
  }

  it should "reject top-level capture without immutable generated descriptors" in {
    val noROM = new FixtureBinding { override val params = cfg.copy(descriptors = None) }
    implicit val p: Parameters = parameters(Some(noROM))
    // Diplomacy pushes a constructor before its body executes. A deliberately
    // rejected constructor cannot reach the normal LazyModule() scope pop.
    val error = try intercept[IllegalArgumentException] { LazyModule(new FPGATop) }
      finally {
        while (org.chipsalliance.diplomacy.lazymodule.LazyModule.getScope.nonEmpty) {
          LazyModule(org.chipsalliance.diplomacy.lazymodule.LazyModule.getScope.get)
        }
      }
    assert(error.getMessage.contains("manifest-bound descriptor ROM"))
  }

  it should "reject an unwired owner/bank binding during hardware initialization checking" in {
    val empty = new CheckpointCaptureTopBinding {
      val params = cfg
      def connect(top: FPGATopImp, capture: CheckpointCaptureSideband): Unit = ()
    }
    implicit val p: Parameters = parameters(Some(empty))
    val top = LazyModule(new FPGATop)
    val error = intercept[firrtl.passes.PassExceptions] {
      (new ChiselStage).emitVerilog(top.module, Array("--target-dir", out.resolve("unwired").toString))
    }
    assert(error.getMessage.contains("not fully initialized"))
  }

  it should "wire generated banks through the actual MMIO interconnect and CPU DMA stream engine" in {
    implicit val p: Parameters = parameters(Some(new FixtureBinding))
    val top = LazyModule(new FPGATop)
    val enabled = out.resolve("enabled")
    val rtl = (new ChiselStage).emitVerilog(top.module, Array("--target-dir", enabled.toString))
    assert(top.bridgesWithToHostCPUStreams.map(_.toHostStreamIdx).toSeq == Seq(0))
    assert(top.checkpointCapture.get.toHostStreamIdx == 1)
    assert(top.toCPUStreamParams.map(_.idx) == Seq(0, 1))
    assert(top.streamingEngine.get.params.toCPUParams == top.toCPUStreamParams)
    assert(rtl.contains("module CheckpointCaptureWidget") && rtl.contains("module CPUManagedStreamEngine"))
    assert(rtl.contains("module NastiRecursiveInterconnect"))
    val header = new StringBuilder
    top.genHeader(header)
    Files.write(enabled.resolve("top_constants.h"), header.toString.getBytes(UTF_8))
    assert(header.toString.contains("TO_HOST_STREAM_IDX 1U"))
    assert(header.toString.contains(descriptors.cppManifest))
    val emitted = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(generated.state.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter]))))
    Files.write(enabled.resolve("generated-bank.sv"), emitted.annotations.collectFirst {
      case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value }.get.getBytes(UTF_8))
    Files.write(enabled.resolve("generated-bank.fir"), generated.state.circuit.serialize.getBytes(UTF_8))
    Files.write(enabled.resolve("generated-bank-input.fir"), state.circuit.serialize.getBytes(UTF_8))
    Files.write(enabled.resolve("generated-bank-semantic.json"), manifest.getBytes(UTF_8))
    Files.write(enabled.resolve("generated-bank-binding.json"), generated.binding.getBytes(UTF_8))
    Files.write(enabled.resolve("manifest.inc"), descriptors.cppManifest.getBytes(UTF_8))
    println(s"CHECKPOINT_TOP_ELABORATED out=$enabled streams=2 checkpoint_stream=1")
  }
}
