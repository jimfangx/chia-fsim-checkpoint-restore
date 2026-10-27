// See LICENSE for license details.

package midas.stage

import firrtl.annotations._
import firrtl.options.Shell
import midas.targetutils.FAMEAnnotation

case class ExtractModuleNameAnnotation(name: String) extends NoTargetAnnotation with FAMEAnnotation
case class RemoveModuleNameAnnotation(name: String) extends NoTargetAnnotation with FAMEAnnotation

trait GoldenGateCli { this: Shell =>
  parser.note("Golden Gate Compiler Options")
  Seq(
    ConfigPackageAnnotation,
    ConfigStringAnnotation,
    OutputBaseFilenameAnnotation,
    midas.checkpoint.SemanticCheckpointRootAnnotation,
    midas.checkpoint.SemanticBridgeCheckpointRootAnnotation,
    midas.checkpoint.SemanticMultiClockInventoryAnnotation,
    midas.checkpoint.SemanticFAMEStateMapAnnotation,
    midas.checkpoint.SimulatorCheckpointInventoryAnnotation,
    midas.checkpoint.SimulatorBankProbeAnnotation,
    midas.checkpoint.SimulatorFPGASynthesisConstantsAnnotation,
    midas.checkpoint.PhysicalClockCutPlanAnnotation,
    midas.checkpoint.PhysicalClockCutWiringProbeAnnotation,
    midas.checkpoint.NativeFunctionalBankProbeAnnotation,
    midas.checkpoint.NativeCheckpointRegionCompilerProbeAnnotation,
    firrtl.stage.FirrtlFileAnnotation,
    firrtl.stage.FirrtlSourceAnnotation,
    firrtl.transforms.NoCircuitDedupAnnotation,
    firrtl.stage.AllowUnrecognizedAnnotations,
  )
    .map(_.addOptions(parser))
}
