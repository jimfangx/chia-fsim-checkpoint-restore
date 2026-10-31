// See LICENSE for license details.
package midas.stage

import firrtl.{AnnotationSeq, CircuitState, Transform}
import firrtl.annotations.ModuleTarget
import firrtl.options.Dependency
import midas.HostTransforms
import midas.stage.phases.ConfigParametersAnnotation
import midas.targetutils.xdc.XDCPathToCircuitAnnotation
import org.chipsalliance.cde.config.Parameters

private class EraseFAMEClockProgressRoles extends Transform {
  def inputForm = firrtl.LowForm
  def outputForm = firrtl.LowForm
  def execute(s: CircuitState): CircuitState = s.copy(annotations = s.annotations.filterNot {
    case _: midas.checkpoint.FAMEClockProgressSignalAnnotation | _: midas.checkpoint.FAMEClockProgressModelAnnotation => true
    case _ => false
  })
}

/** Test access to the same post-mapping compiler entry as a real FPGA build. */
private[midas] object FAMEClockProgressPipelineFixture {
  def eraseRoles(state: CircuitState): AnnotationSeq = compile(state, Seq(new EraseFAMEClockProgressRoles))
  def compile(state: CircuitState, transforms: Seq[Transform] = Nil): AnnotationSeq = {
    implicit val p: Parameters = Parameters.empty.alterPartial {
      case HostTransforms => transforms.map(Dependency.fromTransform(_))
    }
    val root = ModuleTarget(state.circuit.main, state.circuit.main)
    // SimulationMapping consumes these pre-mapping FAME port contracts at its
    // real link boundary. This small sibling enters immediately after that
    // boundary, preserving every observer and native physical annotation.
    val annotations = state.annotations.filterNot(_.isInstanceOf[midas.targetutils.FAMEAnnotation])
    new GoldenGateCompilerPhase().compileSimulator(state.copy(annotations = annotations ++ Seq(
      ConfigParametersAnnotation(p), midas.passes.HostClockSource(root.ref("hostClock")),
      XDCPathToCircuitAnnotation(None, None),
      firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter]))))
  }
}
