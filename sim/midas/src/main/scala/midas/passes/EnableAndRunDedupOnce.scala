// See LICENSE for license details.

package midas.passes

import firrtl.{CircuitState, DependencyAPIMigration, Transform}

/** Aggressive deduplication during phases of the Golden Gate flow where the target design is still being transformed
  * can cause issues with bridge annotations that define connectivity. To work around this issue, the compiler relies on
  * a global NoCircuitDedupAnnotation; however, this transform ignores the annotation to run dedup a single time after
  * target transformation completes rather than several times as it would if enabled globally.
  */
class EnableAndRunDedupOnce extends Transform with DependencyAPIMigration {
  override def prerequisites          = firrtl.stage.Forms.Resolved
  override def optionalPrerequisites  = Nil
  override def optionalPrerequisiteOf = Nil
  override def invalidates(a: Transform) = false

  override def execute(state: CircuitState): CircuitState = {
    // The constraints have been enforced and ordinary dedup remains disabled.
    // Retire module targets now, before FAME/inline removes definitions; never
    // keep stale module constraints as if they still targeted live storage.
    val result = GoldenGateDedupGroups.run(state)
    if (result.annotations.exists(_.isInstanceOf[DedupGroupPolicyAnnotation]))
      GoldenGateDedupGroups.retire(result)
    else result
  }
}
