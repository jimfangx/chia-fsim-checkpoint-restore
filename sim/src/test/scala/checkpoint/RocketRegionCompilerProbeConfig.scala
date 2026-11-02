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
private[firesim] class RocketRegionCompilerProbeReservation(observerDomains: Int) extends Config((site, here, up) => {
  case CheckpointCaptureTopKey =>
    require(sys.props.get("checkpoint.rocket.region.compiler.probe").contains("true"),
      "Rocket regional reservation is a test-only, nondeployable compiler diagnostic")
    val baseEntries = if (sys.props.get("checkpoint.rocket.shell.compiler.probe").contains("true")) 11093 else 10910
    // The opt-in FAME observer adds completed_steps/overflow and two counters
    // per model clock domain. Pending flags already belong to the base DUT.
    // This is an elaboration reservation only: hookup still reconstructs and
    // checks every descriptor and gate from the actual compiler source.
    val observerEntries = if (observerDomains == 0) 0 else 2 + 2 * observerDomains
    val entries = baseEntries + observerEntries +
      (if (sys.props.get("checkpoint.rocket.queue.compiler.probe").contains("true")) 20 else 0)
    Some(new NativeCheckpointWidgetReservationBinding(entries, 3,
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

class WithRocketRegionCompilerProbeReservation extends RocketRegionCompilerProbeReservation(0)

/** Exact QuadRocket has two FAME model clock domains. A different model/domain
  * count fails the unchanged source-reconstructed descriptor check. */
class WithRocketFAMERegionCompilerProbeReservation extends RocketRegionCompilerProbeReservation(2)
