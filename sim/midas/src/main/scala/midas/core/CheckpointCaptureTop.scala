// See LICENSE for license details.
package midas.core

import org.chipsalliance.cde.config.Field
import midas.widgets.{CheckpointCaptureSideband, CheckpointCaptureWidgetParams}

/** Explicit elaboration-time connection to the generated state network.
  *
  * A global transport is not a target bridge. Its MMIO and stream engine must
  * continue running during capture. The binding runs in FPGATopImp's module
  * context, after SimWrapper and bridges exist, so it can connect generated
  * bank ports, the authoritative cut counter and every actual freeze owner.
  * There are deliberately no default owner acknowledgements or bank tieoffs.
  * Missing connections fail FIRRTL initialization checking.
  *
  * This interface alone grants no CPU checkpoint admission. Complete simulator,
  * memory and software ownership still needs independent coverage validation.
  */
trait CheckpointCaptureTopBinding {
  def params: CheckpointCaptureWidgetParams
  def connect(top: FPGATopImp, capture: CheckpointCaptureSideband): Unit
}

/** No stock recipe enables capture merely by selecting an FPGA platform. */
case object CheckpointCaptureTopKey extends Field[Option[CheckpointCaptureTopBinding]](None)
