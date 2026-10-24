// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import firrtl.ir._
import firrtl.transforms.{BlackBoxInlineAnno, BlackBoxPathAnno, BlackBoxResourceAnno}
import midas.passes.xilinx.DefineBUFGCE
import org.json4s._

/** The platform's unparameterized BUFGCE has a vendor-owned transparent-low
  * CE latch (CE_TYPE=SYNC). Public pin/instance constraints may name it, but
  * accepting those constraints cannot turn the metasim O register into a
  * synthesized state owner. Keep these two contracts separate and fail closed
  * at bank generation until a physical latch/cut adapter actually exists.
  */
private[checkpoint] object PhysicalClockGateContract {
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Physical clock-gate contract: $why")

  def validate(state: CircuitState, m: ExtModule): Unit = {
    val expected = DefineBUFGCE.blackbox
    if (m.defname != expected.defname || m.params.nonEmpty ||
        m.ports.map(p => (p.name, p.direction, p.tpe)) !=
          expected.ports.map(p => (p.name, p.direction, p.tpe)))
      fail(s"not the exact default SYNC BUFGCE definition: ${m.name}")
    // A same-named supplied model is not the native primitive, even if its
    // ports are identical. No source annotation may override this identity.
    if (state.annotations.exists {
      case a: BlackBoxInlineAnno => a.target.name == m.name
      case a: BlackBoxPathAnno => a.target.name == m.name
      case a: BlackBoxResourceAnno => a.target.name == m.name
      case _ => false
    }) fail(s"source override on native BUFGCE: ${m.name}")
  }

  def binding(state: CircuitState, m: ExtModule): JValue = {
    validate(state, m)
    JObject(List(
      "contract" -> JString("xilinx-bufgce-default-sync-v1"),
      "definition_sha256" -> JString(SemanticStateInventory.sha256(m.serialize)),
      "ce_type" -> JString("SYNC"), "input_inverted" -> JBool(false),
      "enable_inverted" -> JBool(false), "state" -> JString("transparent_low_ce_latch"),
      "public_pins" -> JArray(List("I", "CE", "O").map(JString(_))),
      "direct_state_readout" -> JBool(false),
      "physical_capture_binding" -> JString("unsupported"),
      "required_cut" -> JString("common_host_low_phase_all_owners_with_enable_latch_mapping"),
      "metasim_state_equivalent" -> JBool(false)))
  }

  def requireCaptureOwner(state: CircuitState): Unit = {
    GeneratedExternalOwners.instances(state, state.circuit.main).foreach {
      case (path, m: ExtModule) if m.defname == "BUFGCE" =>
        validate(state, m)
        fail(s"physical BUFGCE CE latch at $path requires a synthesized capture owner and common low-phase cut; metasim O is not that owner")
      case _ => ()
    }
  }
}
