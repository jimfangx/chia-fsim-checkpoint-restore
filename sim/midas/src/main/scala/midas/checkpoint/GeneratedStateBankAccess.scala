// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, LowForm, Parser, WDefInstance}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Opt-in generator for actual lowered storage, not a simulator backdoor.
  * The supplied complete inventory describes the input circuit, before this
  * generator adds its own transport-owned response registers. All reachable
  * modules are specialized per instance. No opaque or hidden state is guessed.
  *
  * ckpt_freeze must include the capture-accepting edge. ckpt_reset is a separate
  * endpoint reset, never the functional target reset. An accepted read latches
  * its complete response; releasing freeze/aborting cannot modify a held beat.
  * Added asynchronous memory read ports may change FPGA RAM inference/cost.
  * This is not an all-owner simulator cut or a FireSim bridge integration.
  */
object GeneratedStateBankAccess {
  case class Result(state: CircuitState, entries: Seq[StateAccessEntry], binding: String)
  private def fail(message: String): Nothing =
    throw new IllegalArgumentException(s"Generated checkpoint bank unsupported: $message")
  private def stmts(s: Statement): Seq[Statement] = {
    val out = mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  private def snippet(text: String): Seq[Statement] = {
    val m = Parser.parse("circuit Scratch :\n  module Scratch :\n" +
      text.linesIterator.map("    " + _).mkString("\n") + "\n").modules.head.asInstanceOf[Module]
    m.body match { case Block(xs) => xs; case x => Seq(x) }
  }

  /** manifest must be the exact current semantic or generated-simulator
    * inventory rooted at circuit.main. Optional bindings are checked completely.
    */
  def generate(input: CircuitState, manifest: String,
               bindings: Option[Seq[StateAccessEntry]] = None): Result = {
    if (input.form != LowForm) fail("resolved LowForm required")
    // Specialization invalidates module targets. CombinationalPath is the
    // compiler's derived path cache; the Compiler below recomputes it for the
    // specialized hierarchy. Every other annotation needs explicit translation,
    // especially memory initialization, preservation and clock metadata.
    val unsupportedAnnotations = input.annotations.filterNot(_.isInstanceOf[firrtl.transforms.CombinationalPath])
    if (unsupportedAnnotations.nonEmpty) fail("input annotations require explicit clone/rename translation: " +
      unsupportedAnnotations.map(_.getClass.getName).distinct.mkString(", "))
    val doc = parse(manifest)
    val root = input.circuit.main
    val actual = (doc \ "manifest_type") match {
      case JString("fsckpt-generated") => SimulatorStateInventory.discover(input)
      case _ => SemanticStateInventory.discover(input, root)
    }
    if (SemanticStateInventory.canonical(doc) != actual) fail("inventory does not match complete current root")
    val rows = (doc \ "state") match { case JArray(xs) => xs; case _ => fail("missing state inventory") }
    def str(row: JValue, key: String): String = (row \ key).asInstanceOf[JString].s
    def num(row: JValue, key: String): Int = {
      val n = (row \ key).asInstanceOf[JInt].num
      if (!n.isValidInt) fail(s"oversized $key")
      n.toInt
    }
    val expected = rows.map { row =>
      val width = num(row, "width"); val depth = num(row, "depth")
      if (width < 1 || width > 64 || depth < 1 || depth > 65535) fail("wide/deep state requires explicit chunking")
      (str(row, "state_id"), width, depth)
    }
    if (expected.isEmpty || expected.size > 65535) fail("empty inventory or descriptor index overflow")
    val entries = bindings.getOrElse(expected.zipWithIndex.map { case ((id, w, d), index) =>
      StateAccessEntry(id, index / 65536, index % 65536, w, d)
    })
    if (entries.map(e => (e.stateID, e.width, e.depth)) != expected ||
        entries.map(e => (e.bank, e.slot)).distinct.size != entries.size) fail("incomplete/reordered/reshaped/duplicate bindings")
    val byPath = rows.zip(entries).map { case (r, e) => str(r, "hierarchy") -> e }.toMap
    val modules = input.circuit.modules.map(m => m.name -> m).toMap
    val generated = mutable.ArrayBuffer.empty[Module]
    val storage = mutable.ArrayBuffer.empty[JValue]
    val covered = mutable.Set.empty[String]
    val seenNames = mutable.Set.empty[String] ++ modules.keys
    var serial = 0
    def fresh(): String = {
      serial += 1
      val n = s"CheckpointStorage_$serial"
      if (seenNames(n)) fresh() else { seenNames += n; n }
    }
    def build(moduleName: String, path: String, emittedPath: String, isRoot: Boolean): String = {
      val m = modules(moduleName) match {
        case x: Module => x
        case _ => fail(s"opaque module at $path")
      }
      val all = stmts(m.body)
      // Non-storage effects require their own cut contract. Leaving a printf,
      // stop/assertion, or analog attachment active would repeat or externally
      // advance effects while the original storage is frozen.
      if (m.ports.exists(_.tpe.isInstanceOf[AnalogType]) || all.exists {
        case _: Print | _: Stop | _: Verification | _: Attach => true
        case _ => false
      }) fail(s"ungated side effect/analog attachment at $path")
      if ((m.ports.map(_.name) ++ all.collect { case d: IsDeclaration => d.name }).exists(_.startsWith("ckpt_")))
        fail(s"reserved ckpt_ identifier at $path")
      val clocks = m.ports.filter(_.tpe == ClockType)
      if (clocks.size != 1 || clocks.head.direction != Input) fail(s"one direct input clock required at $path")
      val clock = clocks.head.name
      val regs = all.collect { case r: DefRegister => r.name -> r }.toMap
      val mems = all.collect { case mem: DefMemory => mem.name -> mem }.toMap
      regs.values.foreach { r =>
        if (r.clock.serialize != clock || r.reset.tpe != UIntType(IntWidth(1)))
          fail(s"derived clock or asynchronous reset at $path.${r.name}")
      }
      mems.values.foreach { mem =>
        if (mem.readLatency != 0 || mem.writeLatency != 1 || mem.readwriters.nonEmpty)
          fail(s"hidden synchronous/readwrite memory pipeline at $path.${mem.name}")
        if ((mem.readers ++ mem.writers).contains("ckpt_read")) fail(s"reserved memory reader at $path.${mem.name}")
        val clockDrivers = all.collect { case c: Connect if
          (mem.readers ++ mem.writers).exists(p => c.loc.serialize == s"${mem.name}.$p.clk") => c.expr.serialize }
        if (clockDrivers.size != mem.readers.size + mem.writers.size || clockDrivers.exists(_ != clock))
          fail(s"derived memory port clock at $path.${mem.name}")
      }
      val nextName = if (isRoot) root else fresh()
      val childInstances = all.collect { case i: WDefInstance => i }
      val children = childInstances.map(i => i.name -> build(i.module, s"$path.${i.name}",
        s"$emittedPath.${i.name}", false)).toMap
      val readers = mutable.ArrayBuffer.empty[(String, String)]
      (regs.keys ++ mems.keys).toSeq.sorted.foreach { name =>
        val p = s"$path.$name"
        val e = byPath.getOrElse(p, fail(s"state omitted from inventory: $p"))
        if (!covered.add(p)) fail(s"duplicate state: $p")
        val index = readers.size
        val hit = s"ckpt_hit_$index"
        val value = if (mems.contains(name)) s"$name.ckpt_read.data" else name
        readers += hit -> s"asUInt($value)"
        storage += JObject(List("state_id" -> JString(e.stateID), "hierarchy" -> JString(p),
          "storage_hierarchy" -> JString(s"$emittedPath.$name"), "bank" -> JInt(e.bank),
          "slot" -> JInt(e.slot), "width" -> JInt(e.width), "depth" -> JInt(e.depth)))
      }
      def rewrite(s: Statement): Statement = s match {
        case r: DefRegister => r.copy(reset = UIntLiteral(0, IntWidth(1)), init = firrtl.WRef(r.name, r.tpe))
        case mem: DefMemory => mem.copy(readers = mem.readers :+ "ckpt_read")
        case i: WDefInstance => i.copy(module = children(i.name))
        case c: Connect if regs.contains(c.loc.serialize) =>
          val r = regs(c.loc.serialize)
          c.copy(expr = Mux(firrtl.WRef("ckpt_freeze", UIntType(IntWidth(1))),
            firrtl.WRef(r.name, r.tpe), Mux(r.reset, r.init, c.expr, r.tpe), r.tpe))
        case c: Connect if mems.values.exists(mem => mem.writers.exists(p => c.loc.serialize == s"${mem.name}.$p.en")) =>
          c.copy(expr = DoPrim(firrtl.PrimOps.And, Seq(c.expr,
            DoPrim(firrtl.PrimOps.Not, Seq(firrtl.WRef("ckpt_freeze", UIntType(IntWidth(1)))), Nil,
              UIntType(IntWidth(1)))), Nil, UIntType(IntWidth(1))))
        case _: Conditionally => fail(s"non-lowered conditional at $path")
        case x => x.mapStmt(rewrite)
      }
      val readStatements = mutable.ArrayBuffer.empty[String]
      (regs.keys ++ mems.keys).toSeq.sorted.zipWithIndex.foreach { case (name, index) =>
        val e = byPath(s"$path.$name")
        readStatements += s"node ckpt_hit_$index = and(and(eq(ckpt_bank, UInt<8>(${e.bank})), eq(ckpt_slot, UInt<16>(${e.slot}))), lt(ckpt_address, UInt<32>(${e.depth})))"
        mems.get(name).foreach { mem =>
          val bits = math.max(1, (mem.depth - 1).bitLength)
          readStatements ++= Seq(s"$name.ckpt_read.clk <= $clock", s"$name.ckpt_read.en <= UInt<1>(1)",
            s"$name.ckpt_read.addr <= bits(ckpt_address, ${bits - 1}, 0)")
        }
      }
      childInstances.foreach { i =>
        Seq("freeze", "bank", "slot", "address").foreach(n => readStatements += s"${i.name}.ckpt_$n <= ckpt_$n")
        readers += s"${i.name}.ckpt_match" -> s"${i.name}.ckpt_data"
      }
      val hit = readers.foldLeft("UInt<1>(0)") { case (acc, (h, _)) => s"or($acc, $h)" }
      val data = readers.foldRight("UInt<64>(0)") { case ((h, d), acc) => s"mux($h, pad($d, 64), $acc)" }
      readStatements ++= Seq(s"ckpt_match <= $hit", s"ckpt_data <= $data")
      val ports = Seq(("freeze", Input, 1), ("bank", Input, 8), ("slot", Input, 16),
        ("address", Input, 32), ("match", Output, 1), ("data", Output, 64)) ++
        (if (isRoot) Seq(("reset", Input, 1), ("req_valid", Input, 1), ("req_ready", Output, 1),
          ("resp_valid", Output, 1), ("resp_ready", Input, 1), ("resp_data", Output, 64), ("resp_error", Output, 1)) else Nil)
      val endpoint = if (!isRoot) Nil else snippet(s"""reg ckpt_pending : UInt<1>, $clock with :
  reset => (ckpt_reset, UInt<1>(0))
reg ckpt_saved_data : UInt<64>, $clock
reg ckpt_saved_error : UInt<1>, $clock
node ckpt_accept = and(ckpt_req_valid, ckpt_req_ready)
ckpt_pending <= mux(ckpt_accept, UInt<1>(1), mux(ckpt_resp_ready, UInt<1>(0), ckpt_pending))
ckpt_saved_data <= mux(ckpt_accept, mux(and(ckpt_freeze, ckpt_match), ckpt_data, UInt<64>(0)), ckpt_saved_data)
ckpt_saved_error <= mux(ckpt_accept, not(and(ckpt_freeze, ckpt_match)), ckpt_saved_error)
ckpt_req_ready <= and(not(ckpt_pending), not(ckpt_reset))
ckpt_resp_valid <= and(ckpt_pending, not(ckpt_reset))
ckpt_resp_data <= ckpt_saved_data
ckpt_resp_error <= ckpt_saved_error""")
      generated += m.copy(name = nextName,
        ports = m.ports ++ ports.map { case (n, dir, w) => Port(NoInfo, s"ckpt_$n", dir, UIntType(IntWidth(w))) },
        body = Block(Seq(rewrite(m.body)) ++ snippet(readStatements.mkString("\n")) ++ endpoint))
      nextName
    }
    build(root, root, root, true)
    if (covered.toSet != byPath.keySet) fail("inventory contains unsupported/hidden/unreachable state")
    // Re-resolve instance signatures and types after adding ports; compilation
    // preserves the specialized module boundaries and lowers synchronous reset.
    val circuit = input.circuit.copy(modules = generated.toSeq)
    val resolved = new Compiler(Forms.LowForm).execute(
      CircuitState(Parser.parse(circuit.serialize), firrtl.ChirrtlForm)).copy(form = LowForm)
    val binding = SemanticStateInventory.canonical(JObject(List(
      "access_version" -> JInt(1), "protocol" -> JString("latched-banked64-v1"),
      "manifest_sha256" -> JString(SemanticStateInventory.sha256(actual)),
      "reference_sha256" -> JString(SemanticStateInventory.sha256(input.circuit.serialize)),
      "instrumented_sha256" -> JString(SemanticStateInventory.sha256(resolved.circuit.serialize)),
      "scope" -> JString("all_reachable_firrtl_state_only"),
      "state" -> JArray(storage.sortBy(e => (e \ "state_id").values.toString).toList))))
    Result(resolved, entries, binding)
  }
}
