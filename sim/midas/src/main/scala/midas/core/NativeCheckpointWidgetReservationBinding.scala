// See LICENSE for license details.
package midas.core

import chisel3._
import chisel3.experimental.{annotate, ChiselAnnotation}
import firrtl.annotations.InstanceTarget
import midas.checkpoint._
import midas.widgets._

/** Reserve the REAL widget/MMIO/stream before inventory. Only its readout wires
  * have placeholders; target cycle and ALL owner signals have no defaults.
  * The compiler must consume the reservation after lowering. This never embeds
  * an enclosing simulator and never acknowledges a missing freeze owner.
  */
abstract class NativeCheckpointWidgetReservationBinding(entries: Int, gates: Int,
    owners: Seq[String], queueDepth: Int = 64) extends CheckpointCaptureTopBinding {
  final val params = CheckpointCaptureWidgetParams(entries, NativeCheckpointWidgetReservation.digest,
    owners, queueDepth, None, gates, deferredGraft = true)

  /** Production selects top.sim. A test may select another existing execution.
    * Every owner, reset/external writer and cycle source needs explicit wiring.
    */
  protected def connectOwners(top: FPGATopImp, capture: CheckpointCaptureSideband): Unit

  protected def selectedExecution(top: FPGATopImp): InstanceTarget =
    top.sim.toAbsoluteTarget.asInstanceOf[InstanceTarget]

  final def connect(top: FPGATopImp, capture: CheckpointCaptureSideband): Unit = {
    val physical = capture.physical.get
    val pins: Seq[(String, Data)] = Seq(
      "reset" -> capture.controllerReset, "clock_mask" -> physical.clockMask,
      "read_enable" -> physical.readEnable, "captured_ce" -> physical.capturedCE,
      "logical_ce" -> physical.logicalCE, "descriptor_entry" -> capture.descriptorEntry,
      "descriptor_valid" -> capture.descriptorValid, "descriptor_bank" -> capture.descriptorBank,
      "descriptor_slot" -> capture.descriptorSlot, "descriptor_width" -> capture.descriptorWidth,
      "descriptor_depth" -> capture.descriptorDepth, "request_valid" -> capture.bankRequestValid,
      "request_ready" -> capture.bankRequestReady, "bank" -> capture.bank, "slot" -> capture.slot,
      "address" -> capture.address, "response_valid" -> capture.bankResponseValid,
      "response_ready" -> capture.bankResponseReady, "response_data" -> capture.bankResponseData,
      "response_error" -> capture.bankResponseError)
    val directions = NativeCheckpointInPlaceGraft.capturePorts(gates).map(p => p.name -> p.direction).toMap
    pins.foreach { case (role, value) =>
      val wire = Wire(chiselTypeOf(value)).suggestName("checkpoint_widget_" + role)
      if (directions("capture_" + role) == firrtl.ir.Input) wire := value
      else { wire := 0.U.asTypeOf(wire); value := wire }
      dontTouch(wire)
      annotate(new ChiselAnnotation {
        def toFirrtl = NativeCheckpointWidgetPinAnnotation(wire.toAbsoluteTarget, role)
      })
    }
    connectOwners(top, capture)
    annotate(new ChiselAnnotation {
      def toFirrtl = NativeCheckpointWidgetExecutionAnnotation(selectedExecution(top), entries, gates, owners, queueDepth)
    })
  }
}
