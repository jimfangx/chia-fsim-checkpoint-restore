// See LICENSE for license details.
package midas.widgets

import java.nio.file.{Files, Paths}
import chisel3.stage.ChiselStage
import org.scalatest.flatspec.AnyFlatSpec

class CheckpointCaptureSnapshotSpec extends AnyFlatSpec {
  behavior.of("Live checkpoint widget shadow staging")
  it should "emit both concrete control/local widths and reject an empty source" in {
    intercept[IllegalArgumentException] { new ChiselStage().emitVerilog(new CheckpointLiveWidgetSnapshot(0)) }
    val root = Paths.get(sys.props("checkpoint.widget.out")).resolve("live-snapshot")
    Seq(639, 712).foreach { bits =>
      val out = root.resolve(bits.toString)
      Files.createDirectories(out)
      val rtl = new ChiselStage().emitVerilog(new CheckpointLiveWidgetSnapshot(bits),
        Array("--target-dir", out.toString))
      assert(rtl.contains(s"reg [${bits - 1}:0] saved"))
      assert(rtl.contains("io_cutValid") && rtl.contains("io_error"))
    }
  }
}
