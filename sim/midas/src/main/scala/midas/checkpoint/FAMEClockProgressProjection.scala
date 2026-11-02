// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, WDefInstance, WRef, WSubField}
import firrtl.ir._
import org.json4s._

/** Authenticate the bounded compiler rewrite against original executable IR.
  * This grants no freeze acknowledgement, target-time or image authority.
  * Ancestor sideband routing is authenticated by native-region/in-place
  * independent regeneration, not by this local equation projection. */
private[checkpoint] object FAMEClockProgressProjection {
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "FAME clock progress freeze projection unsupported: " + why)
  private def flat(s: Statement): Seq[Statement] = s match {
    case Block(xs) => xs.flatMap(flat)
    case EmptyStmt => Nil
    case other => Seq(other)
  }
  def validate(source: CircuitState, actual: CircuitState, before: Seq[JValue], after: Seq[JValue]): Unit = {
    def identity(r: JValue) = Seq("model_identity", "domain_identity", "role").map(r \ _)
    val originals = before.map(r => identity(r) -> r).toMap
    check(originals.size == before.size && after.size == before.size, "role cardinality changed")
    after.foreach { row =>
      val old = originals.getOrElse(identity(row), throw new IllegalArgumentException("unknown projected FAME role"))
      Seq("rtl_path", "type", "storage").foreach(k => check((old \ k) == (row \ k), "changed concrete role " + k))
    }
    val sourceInstances = GeneratedExternalOwners.instances(source, source.circuit.main).toMap
    val actualInstances = GeneratedExternalOwners.instances(actual, actual.circuit.main).toMap
    val sourceModules = source.circuit.modules.map(m => m.name -> m).toMap
    val actualModules = actual.circuit.modules.map(m => m.name -> m).toMap
    val paths = before.map(r => (r \ "rtl_path").asInstanceOf[JString].s.split('.').dropRight(1).mkString("."))
      .filter(p => sourceInstances.get(p).exists(_.isInstanceOf[Module])).distinct
    val bit = UIntType(IntWidth(1))
    def ref(n: String): Expression = WRef(n, bit)
    def not(e: Expression): Expression = DoPrim(firrtl.PrimOps.Not, Seq(e), Nil, bit)
    def and(a: Expression, b: Expression): Expression = DoPrim(firrtl.PrimOps.And, Seq(a, b), Nil, bit)
    paths.foreach { path =>
      val original = sourceInstances(path).asInstanceOf[Module]
      val generated = actualInstances.getOrElse(path,
        throw new IllegalArgumentException("missing projected FAME execution")).asInstanceOf[Module]
      check(generated.ports.take(original.ports.size) == original.ports,
        "changed original FAME owner interface: " + path)
      val old = flat(original.body); val now = flat(generated.body)
      val decls = now.collect { case d: IsDeclaration => d.name -> d }.groupBy(_._1)
      val drivers = now.collect { case c: Connect => c.loc.serialize -> c.expr }.groupBy(_._1)
      def driver(name: String): Expression = {
        val ds = drivers.getOrElse(name, Nil)
        check(ds.size == 1, "missing or multiple generated writers: " + path + "." + name)
        ds.head._2
      }
      def declaration(name: String): Statement = {
        val ds = decls.getOrElse(name, Nil)
        check(ds.size == 1, "missing or multiple generated declarations: " + path + "." + name)
        ds.head._2.asInstanceOf[Statement]
      }
      val banked = generated.ports.exists(_.name == "ckpt_freeze")
      if (banked) check(!original.ports.exists(_.name == "ckpt_freeze") &&
        generated.ports.count(p => p.name == "ckpt_freeze" && p.direction == Input && p.tpe == bit) == 1,
        "invalid local bank freeze port")
      val rootGate = PhysicalClockCutWiring.rootGate
      val maskedRoot = now.exists { case i: WDefInstance => i.name == rootGate; case _ => false }
      val rawClock = if (!maskedRoot) "" else {
        check(!old.exists { case d: IsDeclaration => d.name == rootGate; case _ => false }, "original root mask collision")
        val gate = declaration(rootGate).asInstanceOf[WDefInstance]
        actualModules(gate.module) match {
          case e: ExtModule if e.defname == "BUFGCE" => PhysicalClockGateContract.validate(actual, e)
          case _ => check(false, "root mask is not native BUFGCE")
        }
        check(driver(rootGate + ".CE").serialize == not(ref(PhysicalClockCutWiring.mask)).serialize,
          "changed root mask CE")
        val clock = driver(rootGate + ".I").serialize
        check(original.ports.exists(p => p.name == clock && p.direction == Input && p.tpe == ClockType),
          "root mask lacks original raw input")
        clock
      }
      def physical(e: Expression): Expression = e match {
        case r: WRef if maskedRoot && r.name == rawClock => WSubField(WRef(rootGate), "O", ClockType)
        case x => x.mapExpr(physical)
      }
      val regs = old.collect { case r: DefRegister => r.name -> r }.toMap
      val mems = old.collect { case m: DefMemory => m.name -> m }.toMap
      val gates = old.collect { case i: WDefInstance if sourceModules(i.module).isInstanceOf[ExtModule] &&
        sourceModules(i.module).asInstanceOf[ExtModule].defname == "BUFGCE" => i.name }.toSet
      val masked = generated.ports.exists(p => p.name == PhysicalClockCutWiring.mask && p.direction == Input && p.tpe == bit)
      check(!banked || masked, "banked FAME owner lacks physical mask input")
      check(!maskedRoot || banked && masked, "root mask lacks bank freeze ownership")
      if (banked) check(maskedRoot == !generated.ports.exists(p => p.name == PhysicalClockCutWiring.raw &&
        p.direction == Input && p.tpe == ClockType), "missing or altered root/raw clock mask owner")
      def live(e: Expression): Expression = if (banked) and(e, not(ref("ckpt_freeze"))) else e
      old.foreach {
        case c: Connect =>
          val name = c.loc.serialize
          val expected = if (masked && gates.exists(g => name == g + ".CE")) {
            val actualCE = driver(name)
            actualCE match {
              case DoPrim(firrtl.PrimOps.And, Seq(w: WRef, enable), Nil, _) =>
                check(w.name.startsWith(PhysicalClockCutWiring.prefix + "ce_") &&
                  declaration(w.name).isInstanceOf[DefWire] && enable.serialize == not(ref(PhysicalClockCutWiring.mask)).serialize,
                  "changed native gate mask")
                check(driver(w.name).serialize == physical(c.expr).serialize, "changed original logical CE")
              case _ => check(false, "missing exact native gate mask")
            }
            actualCE
          } else if (masked && gates.exists(g => name == g + ".I")) {
            WRef(if (maskedRoot) rawClock else PhysicalClockCutWiring.raw, ClockType)
          } else if (banked && regs.contains(name)) {
            val r = regs(name)
            physical(Mux(ref("ckpt_freeze"), WRef(r.name, r.tpe), Mux(r.reset, r.init, c.expr, r.tpe), r.tpe))
          } else if (banked && mems.values.exists(m => m.writers.exists(p => name == m.name + "." + p + ".en")))
            physical(live(c.expr))
          else physical(c.expr)
          check(driver(name).serialize == expected.serialize, "changed original driver: " + path + "." + name)
        case i: WDefInstance =>
          val emitted = declaration(i.name)
          check(emitted.isInstanceOf[WDefInstance], "changed original instance kind")
          if (gates(i.name)) check(emitted.asInstanceOf[WDefInstance].module == i.module, "changed original native gate")
        case r: DefRegister =>
          val expected = if (!banked) r else r.copy(reset = UIntLiteral(0, IntWidth(1)), init = WRef(r.name, r.tpe))
          check(declaration(r.name).serialize == expected.mapExpr(physical).serialize, "changed register owner: " + path + "." + r.name)
        case m: DefMemory =>
          val expected = if (banked) m.copy(readers = m.readers :+ "ckpt_read") else m
          check(declaration(m.name).serialize == expected.serialize, "changed original memory declaration")
        case d: IsDeclaration =>
          check(declaration(d.name).serialize == d.mapExpr(physical).serialize, "changed original observation declaration: " + d.name)
        case p: Print => check(now.exists(_.serialize == p.copy(en = live(p.en)).mapExpr(physical).serialize), "changed print")
        case s: Stop => check(now.exists(_.serialize == s.copy(en = live(s.en)).mapExpr(physical).serialize), "changed stop")
        case v: Verification => check(now.exists(_.serialize == v.copy(en = live(v.en)).mapExpr(physical).serialize), "changed verification")
        case s => check(now.exists(_.serialize == s.mapExpr(physical).serialize), "changed original statement")
      }
    }
  }
}
