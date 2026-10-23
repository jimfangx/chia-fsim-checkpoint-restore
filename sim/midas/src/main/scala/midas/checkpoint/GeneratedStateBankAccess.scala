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
  def validateAnnotationBinding(result: Result): Unit = {
    GeneratedStateBankAnnotations.preflight(result.state)
    val binding = parse(result.binding) \ "annotation_translation"
    require((binding \ "translation_version") == JInt(1) &&
      (binding \ "translated_sha256") == JString(GeneratedStateBankAnnotations.translatedHash(result.state)),
      "checkpoint descriptors: missing or altered translated annotations")
  }
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

  /** Expose exactly the sequential owners used by FIRRTL 1.6's passthrough
    * memory emitter. Address-register readers implement new/undefined reads;
    * old readers and readwriters store data. In particular the emitter updates
    * RW rdata on *every* enabled edge, including write edges. Lowering with a
    * generic memory-delay pass would change those owners and their StateIDs.
    */
  private def materializePipelines(m: Module, path: String): (Module, Map[String, String]) = {
    val all = stmts(m.body)
    val memories = all.collect { case mem: DefMemory if mem.readLatency == 1 => mem }
    if (memories.isEmpty) return (m, Map.empty)
    val drivers = all.collect { case c: Connect => c.loc.serialize -> c.expr }.toMap
    val names = mutable.Set.empty[String] ++ m.ports.map(_.name) ++
      all.collect { case d: IsDeclaration => d.name }
    val replacements = mutable.Map.empty[String, Expression]
    val removed = mutable.Set.empty[String]
    val declarations = mutable.ArrayBuffer.empty[String]
    val connections = mutable.ArrayBuffer.empty[() => String]
    val normalized = mutable.Map.empty[String, DefMemory]
    val logicalNames = mutable.Map.empty[String, String]
    def expr(e: Expression): Expression = replacements.getOrElse(e.serialize, e.mapExpr(expr))
    def driver(mem: DefMemory, port: String, field: String): String = {
      val key = s"${mem.name}.$port.$field"
      expr(drivers.getOrElse(key, fail(s"missing memory driver at $path.$key"))).serialize
    }
    def pipeline(mem: DefMemory, port: String, field: String): String = {
      val ref = firrtl.passes.MemPortUtils.memPortField(mem, port, field)
      val logicalName = firrtl.passes.LowerTypes.loweredName(ref)
      // An explicit register cannot share an emitter-lowered port wire name.
      // Bind a distinct physical name to the original semantic pipeline owner.
      val name = s"CheckpointPipeline_$logicalName"
      if (!names.add(name)) fail(s"memory pipeline identifier collision at $path.$name")
      logicalNames += name -> logicalName
      declarations += s"reg $name : ${ref.tpe.serialize}, ${drivers(s"${mem.name}.$port.clk").serialize}"
      if (field != "addr") replacements += ref.serialize -> firrtl.WRef(name, ref.tpe)
      name
    }
    memories.foreach { mem =>
      if (mem.writeLatency != 1 || (mem.readwriters.nonEmpty && mem.readUnderWrite == ReadUnderWrite.New))
        fail(s"unsupported synchronous/readwrite memory pipeline at $path.${mem.name}")
      if ((mem.readers ++ mem.writers ++ mem.readwriters).exists(_.startsWith("ckpt_")))
        fail(s"reserved memory port at $path.${mem.name}")
      mem.readers.foreach { r =>
        val addressPipeline = mem.readUnderWrite != ReadUnderWrite.Old
        val saved = pipeline(mem, r, if (addressPipeline) "addr" else "data")
        Seq("clk", "en", "addr").foreach(f => removed += s"${mem.name}.$r.$f")
        connections += (() => s"${mem.name}.$r.clk <= ${driver(mem, r, "clk")}")
        connections += (() => s"${mem.name}.$r.en <= ${driver(mem, r, "en")}")
        connections += (() => s"${mem.name}.$r.addr <= ${if (addressPipeline) saved else driver(mem, r, "addr")}")
        connections += (() => s"$saved <= mux(${driver(mem, r, "en")}, ${if (addressPipeline) driver(mem, r, "addr") else s"${mem.name}.$r.data"}, $saved)")
      }
      val rwPorts = mem.readwriters.zipWithIndex.map { case (rw, index) =>
        val r = s"ckpt_rw_${index}_r"; val w = s"ckpt_rw_${index}_w"
        val saved = pipeline(mem, rw, "rdata")
        Seq("clk", "en", "addr", "wmode", "wmask", "wdata").foreach(f => removed += s"${mem.name}.$rw.$f")
        Seq(r, w).foreach { p =>
          connections += (() => s"${mem.name}.$p.clk <= ${driver(mem, rw, "clk")}")
          connections += (() => s"${mem.name}.$p.addr <= ${driver(mem, rw, "addr")}")
        }
        connections += (() => s"${mem.name}.$r.en <= ${driver(mem, rw, "en")}")
        connections += (() => s"${mem.name}.$w.en <= and(${driver(mem, rw, "en")}, ${driver(mem, rw, "wmode")})")
        connections += (() => s"${mem.name}.$w.mask <= ${driver(mem, rw, "wmask")}")
        connections += (() => s"${mem.name}.$w.data <= ${driver(mem, rw, "wdata")}")
        connections += (() => s"$saved <= mux(${driver(mem, rw, "en")}, ${mem.name}.$r.data, $saved)")
        (r, w)
      }
      normalized += mem.name -> mem.copy(readLatency = 0, readwriters = Nil,
        readers = mem.readers ++ rwPorts.map(_._1), writers = mem.writers ++ rwPorts.map(_._2))
    }
    def rewrite(s: Statement): Statement = s match {
      case mem: DefMemory if normalized.contains(mem.name) => normalized(mem.name)
      case c: Connect if removed(c.loc.serialize) => EmptyStmt
      case c: Connect => c.copy(expr = expr(c.expr))
      case x => x.mapStmt(rewrite).mapExpr(expr)
    }
    (m.copy(body = Block(snippet(declarations.mkString("\n")) ++ Seq(rewrite(m.body)) ++
      snippet(connections.map(_()).mkString("\n")))), logicalNames.toMap)
  }

  /** manifest must be the exact current semantic or generated-simulator
    * inventory rooted at circuit.main. Optional bindings are checked completely.
    */
  def generate(input: CircuitState, manifest: String,
               bindings: Option[Seq[StateAccessEntry]] = None): Result = {
    if (input.form != LowForm) fail("resolved LowForm required")
    GeneratedStateBankAnnotations.preflight(input)
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
      if (width < 1 || depth < 1) fail("empty state")
      val chunks = (width.toLong + 63) / 64
      val words = chunks * depth
      // StateAccessEntry is Int-based even though the physical address is 32 bits.
      // Check before narrowing so oversized storage can never wrap or alias.
      if (words > Int.MaxValue) fail("flattened state exceeds supported 31-bit transfer depth")
      (str(row, "state_id"), math.min(width, 64), words.toInt)
    }
    if (expected.isEmpty || expected.size > 65535) fail("empty inventory or descriptor index overflow")
    val entries = bindings.getOrElse(expected.zipWithIndex.map { case ((id, w, d), index) =>
      StateAccessEntry(id, index / 65536, index % 65536, w, d)
    })
    if (entries.map(e => (e.stateID, e.width, e.depth)) != expected ||
        entries.map(e => (e.bank, e.slot)).distinct.size != entries.size) fail("incomplete/reordered/reshaped/duplicate bindings")
    val byPath = rows.zip(entries).map { case (r, e) => str(r, "hierarchy") -> e }.toMap
    val logicalByPath = rows.map(r => str(r, "hierarchy") -> r).toMap
    val chunked = rows.exists(r => num(r, "width") > 64)
    val modules = input.circuit.modules.map(m => m.name -> m).toMap
    val generated = mutable.ArrayBuffer.empty[Module]
    val clones = mutable.ArrayBuffer.empty[GeneratedStateBankAnnotations.Clone]
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
      val (m, pipelineNames) = modules(moduleName) match {
        case x: Module => materializePipelines(x, path)
        case _ => fail(s"opaque module at $path")
      }
      def logicalPath(name: String): String = s"$path.${pipelineNames.getOrElse(name, name)}"
      val all = stmts(m.body)
      // Clocked effects share the storage cut. Analog connections have no
      // such synchronous contract and must still reject before emission.
      if (m.ports.exists(_.tpe.isInstanceOf[AnalogType]) || all.exists(_.isInstanceOf[Attach]))
        fail(s"analog attachment at $path")
      if ((m.ports.map(_.name) ++ all.collect { case d: IsDeclaration => d.name }).exists(_.startsWith("ckpt_")))
        fail(s"reserved ckpt_ identifier at $path")
      val clocks = m.ports.filter(_.tpe == ClockType)
      val hasLocalClockedState = all.exists {
        case _: DefRegister | _: DefMemory | _: Print | _: Stop | _: Verification => true
        case _ => false
      }
      if (clocks.size > 1 || clocks.exists(_.direction != Input) ||
          ((isRoot || hasLocalClockedState) && clocks.isEmpty))
        fail(s"one direct input clock required at $path")
      // Clockless helpers may route the combinational bank network. They must
      // not hide sequential children; child clock connections are checked below.
      val clock = clocks.headOption.map(_.name).getOrElse("")
      all.foreach {
        case p: Print => checkEffect(p.clk, p.en)
        case s: Stop => checkEffect(s.clk, s.en)
        case v: Verification => checkEffect(v.clk, v.en)
        case _ => ()
      }
      def checkEffect(clk: Expression, en: Expression): Unit = {
        if (clk.serialize != clock || en.tpe != UIntType(IntWidth(1)))
          fail(s"derived clock or unresolved clocked effect at $path")
      }
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
      clones += GeneratedStateBankAnnotations.Clone(moduleName, path, nextName, m)
      val childInstances = all.collect { case i: WDefInstance => i }
      val drivers = all.collect { case c: Connect => c.loc.serialize -> c.expr }.toMap
      childInstances.foreach { i =>
        modules(i.module).ports.filter(_.tpe == ClockType).foreach { port =>
          if (port.direction != Input || clock.isEmpty ||
              !drivers.get(s"${i.name}.${port.name}").exists(_.serialize == clock))
            fail(s"derived or independent child clock at $path.${i.name}.${port.name}")
        }
      }
      val children = childInstances.map(i => i.name -> build(i.module, s"$path.${i.name}",
        s"$emittedPath.${i.name}", false)).toMap
      val readers = mutable.ArrayBuffer.empty[(String, String)]
      (regs.keys ++ mems.keys).toSeq.sorted.foreach { name =>
        val p = logicalPath(name)
        val e = byPath.getOrElse(p, fail(s"state omitted from inventory: $p"))
        if (!covered.add(p)) fail(s"duplicate state: $p")
        val index = readers.size
        val hit = s"ckpt_hit_$index"
        val value = if (mems.contains(name)) s"$name.ckpt_read.data" else name
        val row = logicalByPath(p)
        val width = num(row, "width"); val depth = num(row, "depth")
        val chunks = ((width.toLong + 63) / 64).toInt
        // Little-endian chunks, address-major: at = logical_address * chunks + chunk.
        // Explicit fixed slices keep signed words bit-exact and zero the final padding.
        val sliced = if (chunks == 1) s"asUInt($value)" else {
          (0 until chunks).foldRight("UInt<64>(0)") { case (chunk, acc) =>
            val low = chunk * 64
            val high = math.min(width.toLong - 1, low.toLong + 63).toInt
            s"mux(eq(rem(ckpt_address, UInt<32>($chunks)), UInt<32>($chunk)), pad(bits(asUInt($value), $high, $low), 64), $acc)"
          }
        }
        readers += hit -> sliced
        storage += JObject(List("state_id" -> JString(e.stateID), "hierarchy" -> JString(p),
          "storage_hierarchy" -> JString(s"$emittedPath.$name"), "bank" -> JInt(e.bank),
          "slot" -> JInt(e.slot), "width" -> JInt(width), "depth" -> JInt(depth)) ++
          (if (chunked) List("chunks" -> JInt(chunks), "transfer_width" -> JInt(e.width),
            "transfer_depth" -> JInt(e.depth)) else Nil))
      }
      def liveEnable(en: Expression): Expression = DoPrim(firrtl.PrimOps.And, Seq(en,
        DoPrim(firrtl.PrimOps.Not, Seq(firrtl.WRef("ckpt_freeze", UIntType(IntWidth(1)))), Nil,
          UIntType(IntWidth(1)))), Nil, UIntType(IntWidth(1)))
      def rewrite(s: Statement): Statement = s match {
        case p: Print => p.copy(en = liveEnable(p.en))
        case s: Stop => s.copy(en = liveEnable(s.en))
        case v: Verification => v.copy(en = liveEnable(v.en))
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
        val e = byPath(logicalPath(name))
        readStatements += s"node ckpt_hit_$index = and(and(eq(ckpt_bank, UInt<8>(${e.bank})), eq(ckpt_slot, UInt<16>(${e.slot}))), lt(ckpt_address, UInt<32>(${e.depth})))"
        mems.get(name).foreach { mem =>
          val bits = math.max(1, (mem.depth - 1).bitLength)
          readStatements ++= Seq(s"$name.ckpt_read.clk <= $clock", s"$name.ckpt_read.en <= UInt<1>(1)",
            s"$name.ckpt_read.addr <= bits(${if (num(logicalByPath(logicalPath(name)), "width") <= 64) "ckpt_address"
              else s"div(ckpt_address, UInt<32>(${(num(logicalByPath(logicalPath(name)), "width").toLong + 63) / 64}))"}, ${bits - 1}, 0)")
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
    val (translated, annotationBinding) = GeneratedStateBankAnnotations.translate(input, clones.toSeq)
    val resolved = new Compiler(Forms.LowForm).execute(
      CircuitState(Parser.parse(circuit.serialize), firrtl.ChirrtlForm, translated)).copy(form = LowForm)
    val binding = SemanticStateInventory.canonical(JObject(List(
      "access_version" -> JInt(if (chunked) 2 else 1),
      "protocol" -> JString(if (chunked) "latched-banked64-chunked-v2" else "latched-banked64-v1"),
      "manifest_sha256" -> JString(SemanticStateInventory.sha256(actual)),
      "reference_sha256" -> JString(SemanticStateInventory.sha256(input.circuit.serialize)),
      "instrumented_sha256" -> JString(SemanticStateInventory.sha256(resolved.circuit.serialize)),
      "scope" -> JString("all_reachable_firrtl_state_only"),
      "annotation_translation" -> annotationBinding,
      "state" -> JArray(storage.sortBy(e => (e \ "state_id").values.toString).toList))))
    Result(resolved, entries, binding)
  }
}
