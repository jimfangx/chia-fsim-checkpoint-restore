package midas.passes

import firrtl._
import firrtl.ir._

package object xilinx {
  object DefineBUFGCE extends Transform with NoAnalysisPass {
    val blackboxName = "BUFGCE"
    val blackbox     = DefineAbstractClockGate.blackbox.copy(name = blackboxName, defname = blackboxName)
    val transformer = { c: Circuit => c.copy(modules = c.modules :+ blackbox) }
  }

  object ReplaceAbstractClockGates extends Transform {
    def inputForm     = MidForm
    def outputForm    = MidForm
    override def name = "[Golden Gate] Xilinx ReplaceAbstractClockGates"

    def execute(state: CircuitState): CircuitState = {
      ClockGateRetirement(state)
    }
  }

  object HostSpecialization extends SeqTransform {
    val inputForm  = LowForm
    val outputForm = LowForm
    val transforms = Seq(DefineBUFGCE, ReplaceAbstractClockGates)
  }
}
