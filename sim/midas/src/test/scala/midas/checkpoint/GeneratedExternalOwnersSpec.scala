// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, Parser}
import firrtl.annotations.{CircuitName, ModuleName}
import firrtl.ir._
import firrtl.transforms.BlackBoxInlineAnno
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class GeneratedExternalOwnersSpec extends AnyFlatSpec {
  private val source = """circuit Top :
  extmodule Physical :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    defname = BUFGCE
  extmodule Unused :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    defname = AbstractClockGate
  module Child :
    input clock : Clock
    input ce : UInt<1>
    output out : Clock
    inst gate of Physical
    gate.I <= clock
    gate.CE <= ce
    out <= gate.O
  module Top :
    input clock : Clock
    input ce : UInt<1>
    output a : Clock
    output b : Clock
    inst left of Child
    inst right of Child
    left.clock <= clock
    left.ce <= ce
    right.clock <= clock
    right.ce <= ce
    a <= left.out
    b <= right.out
"""
  private def state = CircuitState(Parser.parse(source), LowForm)

  behavior.of("Generated external-owner admission audit")
  it should "separate repeated physical instances from dead definitions without claiming capture" in {
    val input = state.copy(annotations = Seq(BlackBoxInlineAnno(ModuleName("Unused", CircuitName("Top")),
      "unused.v", "opaque unused implementation")))
    val doc = parse(GeneratedExternalOwners.audit(input, "Top"))
    assert((doc \ "reference_sha256") == JString(SemanticStateInventory.sha256(input.circuit.serialize)))
    assert((doc \ "complete_fpga_capture") == JBool(false) && (doc \ "deployable_capture_image") == JBool(false))
    val rows = (doc \ "reachable_external_instances").asInstanceOf[JArray].arr
    assert(rows.map(_ \ "hierarchy") == List(JString("Top.left.gate"), JString("Top.right.gate")))
    assert(rows.forall(row => (row \ "physical_primitive") == JBool(true) &&
      (row \ "physical_capture_binding") == JString("unsupported") && (row \ "inline_sources") == JArray(Nil)))
    val unused = (doc \ "unused_external_definitions").asInstanceOf[JArray].arr
    assert(unused.size == 1 && (unused.head \ "module") == JString("Unused"))
    assert((unused.head \ "inline_sources").asInstanceOf[JArray].arr.size == 1)
    assert(GeneratedExternalOwners.audit(input, "Top") == GeneratedExternalOwners.audit(input, "Top"))
    assert((parse(GeneratedExternalOwners.audit(input, "Child")) \ "reachable_external_instances")
      .asInstanceOf[JArray].arr.map(_ \ "hierarchy") == List(JString("Child.gate")))
  }
  it should "reject duplicate definitions missing children recursive graphs and duplicate paths" in {
    val input = state
    val root = input.circuit.modules.last.asInstanceOf[Module]
    val bad = Seq(input.circuit.copy(modules = input.circuit.modules :+ root),
      input.circuit.copy(modules = input.circuit.modules.filterNot(_.name == "Physical")),
      input.circuit.copy(modules = input.circuit.modules.map {
        case m: Module if m.name == "Child" => m.copy(body = firrtl.WDefInstance("loop", "Top"))
        case m => m
      }), input.circuit.copy(modules = input.circuit.modules.map {
        case m if m.name == "Top" => root.copy(body = Block(Seq(
          firrtl.WDefInstance("same", "Child"), firrtl.WDefInstance("same", "Child"))))
        case m => m
      }))
    bad.foreach(c => intercept[IllegalArgumentException] { GeneratedExternalOwners.audit(input.copy(circuit = c), "Top") })
    intercept[IllegalArgumentException] { GeneratedExternalOwners.audit(input, "Missing") }
  }
}
