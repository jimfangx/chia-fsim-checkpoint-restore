// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, WRef}
import firrtl.annotations.{Annotation, ModuleTarget, NoTargetAnnotation}
import firrtl.ir._
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import firrtl.transforms.DontTouchAnnotation

case object FAMEClockProgressAnnotation extends NoTargetAnnotation with Unserializable with HasShellOptions {
  val options = Seq(new ShellOption[Unit](
    longOption = "simulator-fame-clock-progress",
    toAnnotationSeq = (_: Unit) => Seq(FAMEClockProgressAnnotation),
    helpText = "Preserve consumed-token and launched-edge observations at the actual FAME rules; requires simulator inventory, does not authorize capture.",
  ))
}

/** Observe the FAME rules at their construction site, before optimization can
  * erase the distinction between the next clock token and the pending token.
  * No bridge-source/host counter is used as a proxy for target time.
  *
  * These are ordinary synthesizable host-clock registers, included in the
  * generated inventory. They are NOT a global semantic-cut acknowledgement:
  * satellite progress, channel positions, reset and all capture owners still
  * need a common-cut protocol. There is deliberately no authoritative scalar
  * target-cycle output here. No existing functional signal is driven/changed.
  */
object FAMEClockProgress {
  val prefix = "checkpoint_fame_"
  case class Domain(name: String, incoming: Expression, pending: Expression, launch: Expression,
      gateInstance: String)
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException("FAME clock progress unsupported: " + why)

  def requested(state: CircuitState): Boolean = {
    val count = state.annotations.count(_ == FAMEClockProgressAnnotation)
    if (count == 0) return false
    if (count != 1 || state.annotations.count(_.isInstanceOf[SimulatorCheckpointInventoryAnnotation]) != 1)
      fail("one progress request and one simulator inventory required")
    true
  }

  def instrument(circuit: String, module: Module, hostClock: Expression, hostReset: Expression,
      finishing: Expression, domains: Seq[Domain]): (Module, Seq[Annotation]) = {
    val bit = UIntType(IntWidth(1)); val word = UIntType(IntWidth(64))
    if (hostClock.tpe != ClockType || hostReset.tpe != bit || finishing.tpe != bit || domains.isEmpty ||
        domains.exists(d => !d.name.matches("[A-Za-z_][A-Za-z0-9_$]*") ||
          !d.gateInstance.matches("[A-Za-z_][A-Za-z0-9_$]*") ||
          Seq(d.incoming, d.pending, d.launch).exists(_.tpe != bit)) ||
        domains.map(_.name).distinct.size != domains.size)
      fail("resolved host clock/reset, completion and distinct one-bit clock domains required")
    def check(s: Statement): Unit = {
      s match { case d: IsDeclaration if d.name.startsWith(prefix) => fail("reserved observer identifier collision")
        case _ => () }
      s.foreachStmt(check)
    }
    if (module.ports.exists(_.name.startsWith(prefix))) fail("reserved observer identifier collision")
    check(module.body)
    val decls = scala.collection.mutable.ArrayBuffer.empty[Statement]
    val connects = scala.collection.mutable.ArrayBuffer.empty[Statement]
    val kept = scala.collection.mutable.ArrayBuffer.empty[String]
    val overflows = scala.collection.mutable.ArrayBuffer.empty[Expression]
    def op(p: PrimOp, xs: Expression*): Expression = DoPrim(p, xs, Nil, bit)
    def and(a: Expression, b: Expression): Expression = op(firrtl.PrimOps.And, a, b)
    val notReset = op(firrtl.PrimOps.Not, hostReset)
    val advance = and(finishing, notReset)
    val maximum = UIntLiteral((BigInt(1) << 64) - 1, IntWidth(64))
    def wire(name: String, expr: Expression): Expression = {
      val n = prefix + name; kept += n
      decls += DefWire(NoInfo, n, bit); connects += Connect(NoInfo, WRef(n, bit), expr)
      WRef(n, bit)
    }
    def counter(name: String, enable: Expression): Unit = {
      val n = prefix + name; kept += n
      val reg = DefRegister(NoInfo, n, word, hostClock, hostReset, UIntLiteral(0, IntWidth(64)))
      val ref = WRef(reg)
      decls += reg
      val full = op(firrtl.PrimOps.Eq, ref, maximum)
      overflows += and(enable, full)
      // Explicit width truncation; saturating avoids an apparently valid cut
      // after wraparound. Sticky overflow is separately retained below.
      val sum = DoPrim(firrtl.PrimOps.Add, Seq(ref, UIntLiteral(1, IntWidth(64))), Nil, UIntType(IntWidth(65)))
      val incremented = DoPrim(firrtl.PrimOps.Bits, Seq(sum), Seq(BigInt(63), BigInt(0)), word)
      connects += Connect(NoInfo, ref, Mux(and(enable, op(firrtl.PrimOps.Not, full)), incremented, ref, word))
    }
    counter("completed_steps", advance)
    domains.foreach { d =>
      val consumed = wire(d.name + "_consume", and(advance, d.incoming))
      // launch is exactly the CE expression used by the generated gate. It
      // already includes !hostReset, and refers to the OLD enabled register.
      val launched = wire(d.name + "_launch", d.launch)
      wire(d.name + "_pending", d.pending)
      counter(d.name + "_consumed", consumed)
      counter(d.name + "_applied", launched)
    }
    val overflowName = prefix + "overflow"; kept += overflowName
    val overflow = DefRegister(NoInfo, overflowName, bit, hostClock, hostReset, UIntLiteral(0, IntWidth(1)))
    decls += overflow
    val anyOverflow = overflows.reduce((a, b) => op(firrtl.PrimOps.Or, a, b))
    connects += Connect(NoInfo, WRef(overflow), op(firrtl.PrimOps.Or, WRef(overflow), anyOverflow))
    val observed = module.copy(body = Block(decls.toSeq ++ Seq(module.body) ++ connects.toSeq))
    val target = ModuleTarget(circuit, module.name)
    val identity = target.serialize // historical identity; only ReferenceTargets are renamed
    val bindings = scala.collection.mutable.ArrayBuffer.empty[FAMEClockProgressSignalAnnotation]
    def bind(domain: String, role: String, ref: firrtl.annotations.ReferenceTarget): Unit =
      bindings += FAMEClockProgressSignalAnnotation(identity, domain, role, ref)
    def original(expr: Expression): firrtl.annotations.ReferenceTarget = expr match {
      case r: firrtl.WRef => target.ref(r.name)
      case _ => fail("host clock/reset, completion and pending roles require exact local references")
    }
    bind("", "host_clock", original(hostClock)); bind("", "host_reset", original(hostReset))
    bind("", "finishing", original(finishing))
    bind("", "completed_steps", target.ref(prefix + "completed_steps"))
    bind("", "overflow", target.ref(overflowName))
    domains.foreach { d =>
      Seq("consume", "launch", "pending", "consumed", "applied").foreach { role =>
        bind(d.name, role, target.ref(prefix + d.name + "_" + role))
      }
      bind(d.name, "pending_flag", original(d.pending))
      bind(d.name, "gate_ce", target.ref(d.gateInstance).field("CE"))
      bind(d.name, "gate_output", target.ref(d.gateInstance).field("O"))
    }
    val preserved = (kept.map(target.ref) ++ bindings.map(_.target).filter(_.component.isEmpty)).distinct
    (observed, preserved.map(DontTouchAnnotation(_)).toSeq ++ bindings.toSeq :+
      FAMEClockProgressModelAnnotation(identity, domains.map(_.name)))
  }
}
