// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, Parser}
import firrtl.ir._
import firrtl.transforms.BlackBoxInlineAnno
import midas.passes.DefineAbstractClockGate
import org.json4s._

/** Materialize only the exact MIDAS registered-enable gate. This is not a
  * generic clock-gate rewrite: its enable changes on posedge I, including the
  * resulting delta-cycle behavior of O. Never replace O by I or CE by a clock
  * enable on downstream storage. The ordinary bank rewrite freezes enable.
  */
private[checkpoint] object GeneratedClockGateAdapter {
  val sourceSHA256 = SemanticStateInventory.sha256(DefineAbstractClockGate.impl)
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Generated checkpoint bank unsupported: $why")
  private val template = Parser.parse("""circuit Gate :
  module Gate :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    reg enable : UInt<1>, I
    enable <= CE
    O <= asClock(and(asUInt(I), enable))
""").modules.head.asInstanceOf[Module]

  def prepare(input: CircuitState): (CircuitState, Set[String]) = {
    val inline = input.annotations.collect { case a: BlackBoxInlineAnno => a }
    val reachable = GeneratedExternalOwners.instances(input, input.circuit.main).map(_._2.name).toSet
    val accepted = input.circuit.modules.collect { case m: ExtModule if
      reachable(m.name) && m.defname == DefineAbstractClockGate.blackbox.defname =>
      val sources = inline.filter(_.target.name == m.name)
      if (m.params.nonEmpty || m.ports.map(p => (p.name, p.direction, p.tpe)) !=
          DefineAbstractClockGate.blackbox.ports.map(p => (p.name, p.direction, p.tpe)) ||
          sources.size != 1 || sources.head.target.circuit.name != input.circuit.main ||
          sources.head.text != DefineAbstractClockGate.impl)
        fail(s"no exact registered-enable clock-gate adapter for ${m.name}")
      m.name
    }.toSet
    // No unrecognized inline implementation is discarded by this adapter.
    val prepared = input.copy(circuit = input.circuit.copy(modules = input.circuit.modules.map {
      case m: ExtModule if accepted(m.name) => template.copy(name = m.name, info = m.info)
      case m => m
    }), annotations = input.annotations.filterNot {
      case a: BlackBoxInlineAnno if accepted(a.target.name) => true
      case _ => false
    })
    (prepared, accepted)
  }

  /** Validate the actual specialized implementation independently of metadata.
    * LowForm introduces nodes/casts; expand only local combinational aliases.
    * The enable owner, its clock/reset/next value, and O are all checked.
    */
  def validate(m: Module, path: String, instrumented: Boolean): Unit = {
    val all = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(s: Statement): Unit = { all += s; s.foreachStmt(visit) }; visit(m.body)
    val drivers = all.collect { case c: Connect => c.loc.serialize -> c.expr;
      case n: DefNode => n.name -> n.value }.toMap
    val aliases = all.collect { case n: DefNode => n.name; case w: DefWire => w.name }.toSet
    def expand(e: Expression, seen: Set[String] = Set.empty): Expression = {
      val name = e.serialize
      if (seen(name)) fail(s"clock-gate alias cycle at $path.$name")
      if (aliases(name)) expand(drivers.getOrElse(name, fail(s"undriven gate alias at $path.$name")), seen + name)
      else e.mapExpr(x => expand(x, seen + name)) match {
        case Mux(UIntLiteral(v, IntWidth(w)), t, f, _) if w == 1 && (v == 0 || v == 1) =>
          if (v == 0) f else t
        case x => x
      }
    }
    val regs = all.collect { case r: DefRegister if !r.name.startsWith("ckpt_") => r }
    if (m.ports.filterNot(_.name.startsWith("ckpt_")).map(p => (p.name, p.direction, p.tpe)) !=
          DefineAbstractClockGate.blackbox.ports.map(p => (p.name, p.direction, p.tpe)) ||
        regs.size != 1 || regs.head.name != "enable" || regs.head.tpe != UIntType(IntWidth(1)) ||
        expand(regs.head.clock).serialize != "I" || regs.head.reset.serialize != "UInt<1>(\"h0\")" ||
        all.exists { case _: DefMemory | _: firrtl.WDefInstance | _: Print | _: Stop | _: Verification => true; case _ => false })
      fail(s"altered clock-gate storage at $path")
    val expected = if (instrumented) "mux(ckpt_freeze, enable, CE)" else "CE"
    val nextEnable = expand(drivers.getOrElse("enable", fail(s"missing gate enable at $path"))).serialize
    val output = expand(drivers.getOrElse("O", fail(s"missing gate output at $path"))).serialize
    if (nextEnable != expected || output !=
          "asClock(and(asUInt(I), enable))")
      fail(s"altered registered-enable clock-gate behavior at $path: next=$nextEnable output=$output expected=$expected")
  }

  def binding(paths: Set[String]): JArray = JArray(paths.toSeq.sorted.map { path => JObject(List(
    "hierarchy" -> JString(path), "adapter" -> JString("abstract-clock-gate-v1"),
    "source_sha256" -> JString(sourceSHA256), "state" -> JString("enable"),
    "update_edge" -> JString("posedge I"), "output" -> JString("I & enable"))) }.toList)
}
