// See LICENSE for license details.
package firesim

import chisel3._
import midas.core._
import midas.widgets.CheckpointCaptureSideband
import org.chipsalliance.cde.config.Config

/** Compile the exact Rocket simulator with its real registered MMIO/DMA
  * reservation. This TEST-ONLY overlay is deliberately incapable of capture:
  * cycle authority is absent and every external owner reports an error.
  * The regional compiler probe must retain image-emission rejection.
  *
  * Entry/gate counts are checked against the newly reconstructed executions,
  * never accepted as proof of coverage. No stock recipe enables this overlay.
  */
class WithRocketRegionCompilerProbeReservation extends Config((site, here, up) => {
  case CheckpointCaptureTopKey =>
    require(sys.props.get("checkpoint.rocket.region.compiler.probe").contains("true"),
      "Rocket regional reservation is a test-only, nondeployable compiler diagnostic")
    Some(new NativeCheckpointWidgetReservationBinding(10910, 3,
      Seq("unsupported.functional-memory", "unsupported.bridge-host", "unsupported.reset-external-writers")) {
      protected def connectOwners(top: FPGATopImp, capture: CheckpointCaptureSideband): Unit = {
        // Explicit missing-owner diagnostics, never default acknowledgements.
        // This has no authoritative target cut and cannot emit a valid image.
        capture.targetCycle := 0.U
        capture.ownerPresent := 0.U
        capture.ownerFrozen := 0.U
        capture.ownerError := 7.U
      }
    })
})
