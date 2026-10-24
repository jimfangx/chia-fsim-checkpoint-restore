// See LICENSE for license details.
package midas.widgets

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import chisel3.stage.ChiselStage
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import firesim.lib.nasti.NastiParameters
import org.scalatest.flatspec.AnyFlatSpec

class CheckpointCaptureWidgetSpec extends AnyFlatSpec {
  private val cfg = CheckpointCaptureWidgetParams(3, "1234567890abcdef" * 4,
    Seq("simulator.network", "host.memory"))
  private val out = sys.props.get("checkpoint.widget.out").map(Paths.get(_))
    .getOrElse(Files.createTempDirectory("checkpoint-widget-"))
  Files.createDirectories(out)

  behavior.of("Global checkpoint widget")

  it should "reject missing manifests, descriptor networks and unnamed or duplicate owners" in {
    intercept[IllegalArgumentException] { cfg.copy(entries = 0) }
    intercept[IllegalArgumentException] { cfg.copy(manifestSha256 = "0" * 64) }
    intercept[IllegalArgumentException] { cfg.copy(manifestSha256 = "x" * 64) }
    intercept[IllegalArgumentException] { cfg.copy(ownerNames = Seq.empty) }
    intercept[IllegalArgumentException] { cfg.copy(ownerNames = Seq("same", "same")) }
    intercept[IllegalArgumentException] { cfg.copy(ownerNames = Seq("bad\nname")) }
    intercept[IllegalArgumentException] { cfg.copy(physicalClockGates = -1) }
    intercept[IllegalArgumentException] { cfg.copy(physicalClockGates = 65536) }
    intercept[IllegalArgumentException] { cfg.copy(physicalClockGates = cfg.entries) }
  }

  it should "emit manifest, stream allocation, MMIO addresses and exact RTL provenance" in {
    val header = CheckpointCaptureABI.header("CHECKPOINTCAPTUREWIDGET_0", BigInt(0x1000), cfg, 7)
    assert(header.contains("COMMAND_ADDR 4096ULL"))
    assert(header.contains("STREAM_DRAIN_ACK_ADDR 4196ULL"))
    assert(header.contains("TO_HOST_STREAM_IDX 7U"))
    assert(header.contains(cfg.manifestSha256))
    assert(header.contains("CheckpointCaptureTransport.sv SHA256:"))
    assert(header.contains("ABORT_REQUIRES_HOST_DRAIN 1U"))
    assert(CheckpointCaptureABI.registers.map(_._2).distinct.size == CheckpointCaptureABI.registers.size)
    Files.write(out.resolve("checkpoint_widget_constants.h"), header.getBytes(UTF_8))
  }

  it should "elaborate the real 32-bit NASTI adapter and preserve its protocol channels" in {
    val rtl = (new ChiselStage).emitVerilog(new CheckpointNastiAdapter(NastiParameters(32, 32, 4)),
      Array("--target-dir", out.toString))
    assert(rtl.contains("module CheckpointNastiAdapter"))
    assert(rtl.contains("io_ctrl_aw_ready") && rtl.contains("io_ctrl_w_ready"))
    assert(rtl.contains("io_request_bits_address") && rtl.contains("io_response_ready"))
    val in = getClass.getResourceAsStream("/checkpoint/checkpoint_nasti_adapter_tb.sv")
    require(in != null)
    val tb = try in.readAllBytes() finally in.close()
    Files.write(out.resolve("checkpoint_nasti_adapter_tb.sv"), tb)
  }

  it should "elaborate a host-clock global widget with actual SV transport and 512-bit stream" in {
    implicit val p: Parameters = Parameters.empty.alterPartial {
      case CtrlNastiKey => NastiParameters(32, 32, 4)
    }
    val widget = LazyModule(new CheckpointCaptureWidget(cfg))
    val rtl = (new ChiselStage).emitVerilog(widget.module, Array("--target-dir", out.resolve("widget").toString))
    assert("(?s)CheckpointCaptureTransport\\s+#\\(".r.findFirstIn(rtl).nonEmpty)
    assert(rtl.contains("streamToHostCPU_bits") && rtl.contains("capture_ownerPresent"))
    assert(rtl.contains("capture_controllerReset"))
    assert(!rtl.contains("hPort"))
    val in = getClass.getResourceAsStream("/checkpoint/checkpoint_capture_widget_tb.sv")
    require(in != null)
    val tb = try in.readAllBytes() finally in.close()
    Files.write(out.resolve("widget/checkpoint_capture_widget_tb.sv"), tb)
    println(s"CHECKPOINT_WIDGET_ELABORATED out=$out stream=${widget.toHostStreamIdx}")
  }

  it should "expose the physical cut through the real MMIO and stream widget without duplicate gates" in {
    implicit val p: Parameters = Parameters.empty.alterPartial {
      case CtrlNastiKey => NastiParameters(32, 32, 4)
    }
    val physicalCfg = cfg.copy(entries = 5, physicalClockGates = 3, manifestSha256 = "0123456789abcdef" * 4)
    val widget = LazyModule(new CheckpointCaptureWidget(physicalCfg))
    val destination = out.resolve("physical-widget")
    val rtl = (new ChiselStage).emitVerilog(widget.module, Array("--target-dir", destination.toString))
    assert("(?s)CheckpointPhysicalCaptureTransport\\s+#\\(".r.findFirstIn(rtl).nonEmpty)
    assert(rtl.contains(".EXTERNAL_CLOCK_NETWORK(1)"))
    assert(rtl.contains("capture_physical_logicalCE") && rtl.contains("capture_physical_clockMask"))
    assert(rtl.contains("capture_physical_readEnable") && rtl.contains("capture_physical_capturedCE"))
    assert(!rtl.contains("hPort"))
    val header = CheckpointCaptureABI.header("PHYSICAL", BigInt(0x1000), physicalCfg, widget.toHostStreamIdx)
    assert(header.contains("PHYSICAL_CLOCK_GATES 3U"))
    assert(header.contains("EXTERNAL_CLOCK_NETWORK 1U"))
    assert(header.contains("CheckpointPhysicalClockCut.sv SHA256:"))
    assert(header.contains("CheckpointPhysicalCaptureTransport.sv SHA256:"))
    Files.write(destination.resolve("physical_constants.h"), header.getBytes(UTF_8))
    println(s"CHECKPOINT_PHYSICAL_WIDGET_ELABORATED out=$destination")
  }
}
