// See LICENSE for license details.
package midas.passes

import firrtl.{CircuitState, DependencyAPIMigration, Transform}
import firrtl.ir.{ClockType, Input, Module}
import midas.checkpoint.{GeneratedExternalOwners, NativeCheckpointRegionCompilerProbe}

/** Preserve the actual elaboration clock label for the regional compiler
  * diagnostic through HostClockWiring's documented retaining API. The native
  * wiring and ordinary execute path are unchanged. This label identifies an
  * existing clock input; it acknowledges no checkpoint or freeze owner.
  */
object CheckpointRegionHostClockWiring extends Transform with DependencyAPIMigration {
  override def prerequisites = HostClockWiring.prerequisites
  override def optionalPrerequisites = HostClockWiring.optionalPrerequisites
  override def optionalPrerequisiteOf = HostClockWiring.optionalPrerequisiteOf
  override def invalidates(transform: Transform): Boolean = HostClockWiring.invalidates(transform)

  private def source(state: CircuitState): HostClockSource = {
    def check(ok: Boolean, why: String): Unit = require(ok, "regional host clock wiring: " + why)
    val sources = state.annotations.collect { case a: HostClockSource => a }
    check(sources.size == 1, "one actual elaboration clock source required")
    val a = sources.head
    val t = a.target
    check(t.circuit == state.circuit.main && t.path.isEmpty && t.component.isEmpty,
      "source must identify a direct clock input in this circuit")
    val executions = GeneratedExternalOwners.instances(state, state.circuit.main).filter(_._2.name == t.module)
    check(executions.size == 1, "source definition must have one concrete execution")
    check(executions.head._2 match {
      case m: Module => m.ports.exists(p => p.name == t.ref && p.direction == Input && p.tpe == ClockType)
      case _ => false
    }, "source must remain an existing scalar Clock input")
    a
  }

  def execute(state: CircuitState): CircuitState = {
    require(NativeCheckpointRegionCompilerProbe.requested(state.annotations),
      "regional host clock wiring: explicit regional compiler diagnostic required")
    NativeCheckpointRegionCompilerProbe.validateCardinality(state.annotations)
    val original = source(state)
    val result = HostClockWiring(state)
    require(source(result) == original, "regional host clock wiring: native source changed during wiring")
    result
  }
}
