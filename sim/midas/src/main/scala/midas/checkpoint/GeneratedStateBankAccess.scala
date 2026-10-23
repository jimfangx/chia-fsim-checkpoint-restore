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
  * separateAccessClock adds an always-running ckpt_clock for the root response
  * endpoint. Functional storage retains its original clocks. The caller must
  * establish and hold the global freeze before access and keep it asserted
  * until all responses drain; this option supplies no clock-domain handshake.
  * Multiple functional input clocks are supported only with that separate
  * endpoint. Transparent clock routing through concrete hierarchy is resolved
  * to named root inputs. Exact MIDAS registered-enable gates retain their own
  * enable owner and explicit gate-chain provenance; other derived/gated clocks
  * and opaque owners still reject.
  * This is not an all-owner simulator cut or a FireSim bridge integration.
  */
object GeneratedStateBankAccess {
  case class Result(state: CircuitState, entries: Seq[StateAccessEntry], binding: String)
  def validateAnnotationBinding(result: Result): Unit = {
    GeneratedStateBankAnnotations.preflight(result.state)
    val doc = parse(result.binding)
    val binding = doc \ "annotation_translation"
    require((binding \ "translation_version") == JInt(1) &&
      (binding \ "translated_sha256") == JString(GeneratedStateBankAnnotations.translatedHash(result.state)),
      "checkpoint descriptors: missing or altered translated annotations")
    val root = result.state.circuit.modules.collectFirst {
      case m: Module if m.name == result.state.circuit.main => m
    }.getOrElse(throw new IllegalArgumentException("checkpoint descriptors: missing access-clock root"))
    val accessClock = doc \ "access_clock"
    require(root.ports.exists(p => p.direction == Input && p.tpe == ClockType &&
      accessClock == JString(p.name)) &&
      stmts(root.body).collect { case r: DefRegister if
        Set("ckpt_pending", "ckpt_saved_data", "ckpt_saved_error")(r.name) => r.clock.serialize
      }.map(JString(_)) == Seq.fill(3)(accessClock) &&
      (doc \ "freeze_contract") == JString("externally_held_all_functional_owners_before_access_until_drain"),
      "checkpoint descriptors: missing or altered access clock/freeze contract")
    val gates = (doc \ "clock_gate_adapters") match {
      case JNothing => Set.empty[String]
      case JArray(rows) => rows.map(row => (row \ "hierarchy") match {
        case JString(path) => path
        case _ => fail("malformed clock-gate adapter hierarchy")
      }).toSet
      case _ => fail("malformed clock-gate adapters")
    }
    if (gates.nonEmpty) require((doc \ "clock_gate_adapters") == GeneratedClockGateAdapter.binding(gates),
      "checkpoint descriptors: altered clock-gate adapter binding")
    require((doc \ "functional_clock_domains") == clockDomains(result.state.circuit,
      accessClock == JString("ckpt_clock"), gates, instrumented = true),
      "checkpoint descriptors: missing or altered functional clock domains")
  }
  private def fail(message: String): Nothing =
    throw new IllegalArgumentException(s"Generated checkpoint bank unsupported: $message")
  private def stmts(s: Statement): Seq[Statement] = {
    val out = mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }

  /** Resolve transparent hierarchy in the defining scope. Never use a register's
    * next-state driver as an alias, or assume an opaque output is a clock wire.
    * Lazy per-pin resolution permits acyclic sibling forwarding even when the
    * forwarding module's unrelated inputs depend on its consumer's outputs.
    */
  private case class ClockOrigin(root: Option[String], constant: Option[BigInt] = None,
                                 gates: Seq[String] = Nil, invalid: Option[String] = None,
                                 data: Option[(String, String)] = None) {
    def functional(path: String): ClockOrigin = {
      if (constant.nonEmpty) fail(s"constant clock drives functional storage/effect at $path")
      if (invalid.nonEmpty) fail(s"invalid clock drives functional storage/effect at $path (origin ${invalid.get})")
      if (data.nonEmpty) fail(s"computed clock data drives functional storage/effect at $path (origin ${data.get._1})")
      this
    }
    def gated(path: String): ClockOrigin = {
      functional(path)
      copy(gates = gates :+ path)
    }
    def row(path: String): JObject = JObject(List("clock_hierarchy" -> JString(path)) ++
      root.map(v => "root_clock" -> JString(v)).toList ++
      constant.map(v => "constant_clock" -> JInt(v)).toList ++
      invalid.map(v => "invalid_clock" -> JString(v)).toList ++
      data.map { case (scope, expression) => "data_clock" -> JObject(List(
        "hierarchy" -> JString(scope), "expression" -> JString(expression))) }.toList ++
      (if (gates.isEmpty) Nil else List("gate_chain" -> JArray(gates.map(JString(_)).toList))))
  }
  private class ClockProvenance(circuit: Circuit, hasAccessClock: Boolean,
                                gatePaths: Set[String] = Set.empty, instrumented: Boolean = false) {
    private val modules = circuit.modules.map(m => m.name -> m).toMap
    private def concrete(name: String, path: String): Module = modules(name) match {
      case m: Module => m
      case _ => fail(s"opaque functional clock owner at $path")
    }
    private def carrier(t: Type): Boolean = t == ClockType || t == UIntType(IntWidth(1))
    class Scope(val m: Module, val path: String, incoming: (String, Set[String]) => ClockOrigin,
                ancestors: Set[String]) {
      if (ancestors(m.name)) fail(s"recursive clock hierarchy at $path")
      val all = stmts(m.body)
      val drivers = mutable.Map.empty[String, Expression]
      val invalids = mutable.Set.empty[String]
      val aliases = all.collect { case d: DefNode => d.name; case d: DefWire => d.name }.toSet
      all.foreach {
        case c: Connect =>
          if (drivers.contains(c.loc.serialize) || invalids(c.loc.serialize))
            fail(s"multiple clock/net drivers at $path.${c.loc.serialize}")
          drivers(c.loc.serialize) = c.expr
        case i: IsInvalid =>
          if (drivers.contains(i.expr.serialize) || !invalids.add(i.expr.serialize))
            fail(s"multiple clock/net drivers at $path.${i.expr.serialize}")
        case n: DefNode => drivers(n.name) = n.value
        case _ => ()
      }
      val inputs = m.ports.filter(p => p.direction == Input && carrier(p.tpe)).map(_.name).toSet
      val outputs = m.ports.filter(p => p.direction == Output && carrier(p.tpe)).map(_.name).toSet
      val instances = all.collect { case i: WDefInstance => i.name -> i }.toMap
      if (gatePaths(path)) GeneratedClockGateAdapter.validate(m, path, instrumented)
      private val children = mutable.Map.empty[String, Scope]
      private def drivenClock(name: String, seen: Set[String]): ClockOrigin = {
        if (invalids(name)) ClockOrigin(None, invalid = Some(s"$path.$name"))
        else clock(drivers.getOrElse(name, fail(s"missing child clock at $path.$name")), seen)
      }
      def child(i: WDefInstance): Scope = children.getOrElseUpdate(i.name,
        new Scope(concrete(i.module, s"$path.${i.name}"), s"$path.${i.name}", (pin, seen) =>
          drivenClock(s"${i.name}.$pin", seen),
          ancestors + m.name))
      def clock(e: Expression, seen: Set[String] = Set.empty): ClockOrigin = {
        val name = e.serialize
        val key = s"$path::$name"
        if (seen(key)) fail(s"clock alias cycle at $path.$name")
        val next = seen + key
        if (!carrier(e.tpe)) fail(s"non-clock alias at $path.$name")
        if (gatePaths(path) && name == "O") clock(firrtl.WRef("I", ClockType), next).gated(path)
        else if (inputs(name)) incoming(name, next)
        // Preserve an explicit FIRRTL invalid origin through transparent
        // metadata routes. Missing drivers still reject. Every sequential
        // consumer is audited separately and cannot use this origin.
        else if (invalids(name) && (aliases(name) || outputs(name)))
          ClockOrigin(None, invalid = Some(s"$path.$name"))
        else e match {
          // Static metadata ports remain explicit in the domain binding. They
          // are never root clocks and may not drive any sequential consumer.
          case UIntLiteral(value, IntWidth(width)) if width == 1 && (value == 0 || value == 1) =>
            ClockOrigin(None, Some(value))
          case DoPrim(firrtl.PrimOps.AsClock, Seq(value), Nil, ClockType)
              if value.tpe == UIntType(IntWidth(1)) => clock(value, next)
          case DoPrim(firrtl.PrimOps.AsUInt, Seq(value), Nil, UIntType(IntWidth(width)))
              if width == 1 && value.tpe == ClockType => clock(value, next)
          case DoPrim(firrtl.PrimOps.Bits, Seq(value), Seq(hi, lo), UIntType(IntWidth(width)))
              if width == 1 && hi == 0 && lo == 0 && value.tpe == UIntType(IntWidth(1)) => clock(value, next)
          // FAME carries target clock tokens as Clock-typed *data*. Preserve
          // the first concrete one-bit computation through transparent routes,
          // without inferring an oscillator/root from its data dependencies.
          // Storage/effect/gate consumers must still call functional(). Missing
          // drivers, opaque outputs and bare storage references do not qualify.
          case _: DoPrim | _: Mux | _: ValidIf if e.tpe == UIntType(IntWidth(1)) =>
            ClockOrigin(None, data = Some(path -> e.serialize))
          case _ => name.split("\\.", -1).toSeq match {
            case Seq(instance, pin) if instances.contains(instance) =>
              val scope = child(instances(instance))
              val port = scope.m.ports.find(p => p.name == pin && carrier(p.tpe) && p.tpe == e.tpe)
                .getOrElse(fail(s"reshaped/non-clock child pin at $path.$name"))
              if (port.direction == Input) drivenClock(name, next)
              else scope.clock(firrtl.WRef(pin, port.tpe), next)
            case _ => drivers.get(name) match {
              case Some(value) if value.tpe == e.tpe && (aliases(name) || outputs(name)) => clock(value, next)
              case _ => fail(s"derived, gated, or undriven clock/child clock at $path.$name")
            }
          }
        }
      }
      def memoryClock(e: Expression, enable: Expression, seen: Set[String] = Set.empty): ClockOrigin = {
        def identity(value: Expression, visited: Set[String] = Set.empty): String = {
          val name = value.serialize
          if (visited(name)) fail(s"memory enable alias cycle at $path.$name")
          value match {
            case Mux(cond, UIntLiteral(one, IntWidth(w1)), UIntLiteral(zero, IntWidth(w0)), _)
                if one == 1 && zero == 0 && w1 == 1 && w0 == 1 => identity(cond, visited + name)
            case _ if aliases(name) && value.tpe == UIntType(IntWidth(1)) && drivers.contains(name) =>
              identity(drivers(name), visited + name)
            case _ => name
          }
        }
        val name = e.serialize
        if (seen(name)) fail(s"memory clock alias cycle at $path.$name")
        e match {
          // Chisel conditionally writes with validif(enable, clock). It does
          // not describe an independent gated clock when that exact enable
          // also guards this same memory port. Keep the original expression.
          case ValidIf(cond, value, ClockType) if identity(cond) == identity(enable) => clock(value).functional(s"$path.${e.serialize}")
          case _ if aliases(name) && e.tpe == ClockType && drivers.contains(name) =>
            memoryClock(drivers(name), enable, seen + name)
          case _ => clock(e).functional(s"$path.${e.serialize}")
        }
      }
      // Descriptor revalidation must reject a static pin reconnected to state,
      // even when the attacker also updates the instrumented-circuit hash.
      // Memory validif guards are checked separately during generation; here
      // follow their clock value only to reconstruct the sequential origin.
      def validateConsumers(): Unit = {
        def memoryOrigin(e: Expression, seen: Set[String] = Set.empty): ClockOrigin = {
          val key = e.serialize
          if (seen(key)) fail(s"memory clock alias cycle at $path.$key")
          e match {
            case ValidIf(_, value, ClockType) => memoryOrigin(value, seen + key)
            case _ if aliases(key) && e.tpe == ClockType && drivers.contains(key) =>
              memoryOrigin(drivers(key), seen + key)
            case _ => clock(e)
          }
        }
        all.foreach {
          case r: DefRegister if !(instrumented && path == circuit.main &&
              Set("ckpt_pending", "ckpt_saved_data", "ckpt_saved_error")(r.name)) =>
            (if (instrumented) memoryOrigin(r.clock) else clock(r.clock)).functional(s"$path.${r.name}")
          case p: Print => clock(p.clk).functional(s"$path.print")
          case s: Stop => clock(s.clk).functional(s"$path.stop")
          case v: Verification => clock(v.clk).functional(s"$path.verification")
          case mem: DefMemory =>
            (mem.readers ++ mem.writers ++ mem.readwriters).filterNot(p => instrumented && p == "ckpt_read").foreach { port =>
              val pin = s"${mem.name}.$port.clk"
              memoryOrigin(drivers.getOrElse(pin, fail(s"missing memory clock at $path.$pin"))).functional(s"$path.$pin")
            }
          case _ => ()
        }
      }
    }
    val root = new Scope(concrete(circuit.main, circuit.main), circuit.main, (pin, _) => {
      if (!modules(circuit.main).ports.exists(p => p.name == pin && p.direction == Input && p.tpe == ClockType) ||
          (hasAccessClock && pin == "ckpt_clock")) fail(s"non-functional root clock at ${circuit.main}.$pin")
      ClockOrigin(Some(pin))
    }, Set.empty)
    private val scopes = mutable.Map(circuit.main -> root)
    def scope(path: String): Scope = scopes.getOrElseUpdate(path, {
      val split = path.lastIndexOf('.')
      val parent = scope(path.take(split))
      parent.child(parent.instances(path.drop(split + 1)))
    })
    def domains: JArray = {
      val rows = mutable.ArrayBuffer.empty[JValue]
      def walk(s: Scope): Unit = {
        s.validateConsumers()
        s.m.ports.filter(p => p.tpe == ClockType &&
          !(hasAccessClock && s.path == circuit.main && p.name == "ckpt_clock")).foreach { p =>
          rows += s.clock(firrtl.WRef(p.name, p.tpe)).row(s"${s.path}.${p.name}")
        }
        s.instances.values.toSeq.sortBy(_.name).foreach(i => walk(s.child(i)))
      }
      walk(root)
      if (!gatePaths.subsetOf(scopes.keySet.toSet ++ collectPaths(root)))
        fail("unreachable clock-gate adapter hierarchy")
      JArray(rows.sortBy(v => (v \ "clock_hierarchy").values.toString).toList)
    }
    private def collectPaths(s: Scope): Set[String] = Set(s.path) ++
      s.instances.values.flatMap(i => collectPaths(s.child(i))).toSet
  }
  /** Reconstruct provenance from the actual emitted circuit, independently of
    * input bindings. This proves wiring only, not CDC or atomic-cut ownership.
    */
  private def clockDomains(circuit: Circuit, hasAccessClock: Boolean,
                           gates: Set[String], instrumented: Boolean): JArray =
    new ClockProvenance(circuit, hasAccessClock, gates, instrumented).domains
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
  private def materializePipelines(m: Module, path: String): (Module, Map[String, String], Map[String, Expression]) = {
    val all = stmts(m.body)
    val memories = all.collect { case mem: DefMemory if mem.readLatency == 1 => mem }
    if (memories.isEmpty) return (m, Map.empty, Map.empty)
    val drivers = all.collect { case c: Connect => c.loc.serialize -> c.expr }.toMap
    val names = mutable.Set.empty[String] ++ m.ports.map(_.name) ++
      all.collect { case d: IsDeclaration => d.name }
    val replacements = mutable.Map.empty[String, Expression]
    val removed = mutable.Set.empty[String]
    val declarations = mutable.ArrayBuffer.empty[String]
    val connections = mutable.ArrayBuffer.empty[() => String]
    val normalized = mutable.Map.empty[String, DefMemory]
    val logicalNames = mutable.Map.empty[String, String]
    val pipelineClocks = mutable.Map.empty[String, Expression]
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
      // A port clock may use a local node declared after the memory. Declare
      // a clock wire first and connect it after the original body, so newly
      // materialized pipeline registers never create forward references.
      val clockName = s"${name}_clock"
      if (!names.add(clockName)) fail(s"memory pipeline clock collision at $path.$clockName")
      val portClock = drivers(s"${mem.name}.$port.clk")
      pipelineClocks += clockName -> portClock
      declarations += s"wire $clockName : Clock"
      declarations += s"reg $name : ${ref.tpe.serialize}, $clockName"
      connections += (() => s"$clockName <= ${portClock.serialize}")
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
      snippet(connections.map(_()).mkString("\n")))), logicalNames.toMap, pipelineClocks.toMap)
  }

  /** manifest must be the exact current semantic or generated-simulator
    * inventory rooted at circuit.main. Optional bindings are checked completely.
    */
  def generate(input: CircuitState, manifest: String,
               bindings: Option[Seq[StateAccessEntry]] = None,
               separateAccessClock: Boolean = false): Result = {
    if (input.form != LowForm) fail("resolved LowForm required")
    val (prepared, gateModules) = GeneratedClockGateAdapter.prepare(input)
    GeneratedStateBankAnnotations.preflight(prepared)
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
    val modules = prepared.circuit.modules.map(m => m.name -> m).toMap
    val gatePaths = mutable.Set.empty[String]
    def findGates(name: String, path: String, ancestors: Set[String]): Unit = {
      if (ancestors(name)) fail(s"recursive hierarchy at $path")
      if (gateModules(name)) gatePaths += path
      modules(name) match {
        case m: Module => stmts(m.body).collect { case i: WDefInstance => i }
          .foreach(i => findGates(i.module, s"$path.${i.name}", ancestors + name))
        case _ => () // The existing concrete-owner check rejects reachable opaque owners.
      }
    }
    findGates(root, root, Set.empty)
    // Resolve the materialized gates before inspecting typed clocks.
    val resolvedPrepared = if (gateModules.isEmpty) prepared else new Compiler(Forms.LowForm).execute(
      CircuitState(Parser.parse(prepared.circuit.serialize), firrtl.ChirrtlForm,
        (prepared.annotations :+ firrtl.transforms.NoCircuitDedupAnnotation).distinct)).copy(form = LowForm)
    val concreteModules = resolvedPrepared.circuit.modules.map(m => m.name -> m).toMap
    val clockProvenance = new ClockProvenance(resolvedPrepared.circuit, false, gatePaths.toSet)
    val originalDomains = clockProvenance.domains
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
      val (m, pipelineNames, pipelineClocks) = concreteModules(moduleName) match {
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
      val clocks = m.ports.filter(p => p.tpe == ClockType && p.direction == Input)
      val hasLocalClockedState = all.exists {
        case _: DefRegister | _: DefMemory | _: Print | _: Stop | _: Verification => true
        case _ => false
      }
      if ((!separateAccessClock && clocks.size > 1) ||
          ((isRoot || hasLocalClockedState) && clocks.isEmpty))
        fail(s"direct input clocks required (multiple clocks need separate access clock) at $path")
      // Clockless helpers may route the combinational bank network. They must
      // not hide sequential children; child clock connections are checked below.
      val clock = clocks.headOption.map(_.name).getOrElse("")
      val clockScope = clockProvenance.scope(path)
      // Pipeline declarations/connections are parsed snippets until the final
      // resolver. Recover only their exact original, already typed clock
      // expressions; never guess a type for a newly introduced clock.
      val typedClocks = clockScope.all.flatMap {
        case r: DefRegister => Seq(r.clock)
        case c: Connect if c.expr.tpe == ClockType => Seq(c.expr)
        case p: Print => Seq(p.clk)
        case s: Stop => Seq(s.clk)
        case v: Verification => Seq(v.clk)
        case _ => Nil
      }.map(e => e.serialize -> e).toMap
      val originalMemoryClocks = clockScope.all.collect { case mem: DefMemory =>
        (mem.readers ++ mem.writers ++ mem.readwriters).map { port =>
          val prefix = s"${mem.name}.$port"
          val clk = clockScope.drivers.getOrElse(s"$prefix.clk", fail(s"missing memory clock at $path.$prefix"))
          val en = clockScope.drivers.getOrElse(s"$prefix.en", fail(s"missing memory enable at $path.$prefix"))
          clk.serialize -> clockScope.memoryClock(clk, en)
        }
      }.flatten.toMap
      def typedClock(e: Expression): Expression = if (e.tpe == UnknownType) typedClocks.getOrElse(e.serialize,
        fail(s"unresolved materialized clock at $path.${e.serialize}")) else e
      def functionalClock(e: Expression): ClockOrigin = if (pipelineClocks.contains(e.serialize)) {
        val original = pipelineClocks(e.serialize)
        originalMemoryClocks.getOrElse(original.serialize, fail(s"unowned pipeline clock at $path.${e.serialize}"))
      } else clockScope.clock(typedClock(e)).functional(s"$path.${e.serialize}")
      all.foreach {
        case p: Print => checkEffect(p.clk, p.en)
        case s: Stop => checkEffect(s.clk, s.en)
        case v: Verification => checkEffect(v.clk, v.en)
        case _ => ()
      }
      def checkEffect(clk: Expression, en: Expression): Unit = {
        functionalClock(clk)
        if (en.tpe != UIntType(IntWidth(1)))
          fail(s"derived clock or unresolved clocked effect at $path")
      }
      val regs = all.collect { case r: DefRegister => r.name -> r }.toMap
      val mems = all.collect { case mem: DefMemory => mem.name -> mem }.toMap
      regs.values.foreach { r =>
        functionalClock(r.clock)
        if (r.reset.tpe != UIntType(IntWidth(1)))
          fail(s"derived clock or asynchronous reset at $path.${r.name}")
      }
      mems.values.foreach { mem =>
        if (mem.readLatency != 0 || mem.writeLatency != 1 || mem.readwriters.nonEmpty)
          fail(s"hidden synchronous/readwrite memory pipeline at $path.${mem.name}")
        if ((mem.readers ++ mem.writers).contains("ckpt_read")) fail(s"reserved memory reader at $path.${mem.name}")
        val clockDrivers = all.collect { case c: Connect if
          (mem.readers ++ mem.writers).exists(p => c.loc.serialize == s"${mem.name}.$p.clk") => c.expr }
        clockDrivers.foreach(e => originalMemoryClocks.getOrElse(typedClock(e).serialize,
          fail(s"unowned materialized memory clock at $path.${e.serialize}")))
        if (clockDrivers.size != mem.readers.size + mem.writers.size)
          fail(s"derived memory port clock at $path.${mem.name}")
      }
      val nextName = if (isRoot) root else fresh()
      clones += GeneratedStateBankAnnotations.Clone(moduleName, path, nextName, m)
      val childInstances = all.collect { case i: WDefInstance => i }
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
      val endpointClock = if (separateAccessClock) "ckpt_clock" else clock
      val endpoint = if (!isRoot) Nil else snippet(s"""reg ckpt_pending : UInt<1>, $endpointClock with :
  reset => (ckpt_reset, UInt<1>(0))
reg ckpt_saved_data : UInt<64>, $endpointClock
reg ckpt_saved_error : UInt<1>, $endpointClock
node ckpt_accept = and(ckpt_req_valid, ckpt_req_ready)
ckpt_pending <= mux(ckpt_accept, UInt<1>(1), mux(ckpt_resp_ready, UInt<1>(0), ckpt_pending))
ckpt_saved_data <= mux(ckpt_accept, mux(and(ckpt_freeze, ckpt_match), ckpt_data, UInt<64>(0)), ckpt_saved_data)
ckpt_saved_error <= mux(ckpt_accept, not(and(ckpt_freeze, ckpt_match)), ckpt_saved_error)
ckpt_req_ready <= and(not(ckpt_pending), not(ckpt_reset))
ckpt_resp_valid <= and(ckpt_pending, not(ckpt_reset))
ckpt_resp_data <= ckpt_saved_data
ckpt_resp_error <= ckpt_saved_error""")
      generated += m.copy(name = nextName,
        ports = m.ports ++ ports.map { case (n, dir, w) => Port(NoInfo, s"ckpt_$n", dir, UIntType(IntWidth(w))) } ++
          (if (isRoot && separateAccessClock) Seq(Port(NoInfo, "ckpt_clock", Input, ClockType)) else Nil),
        body = Block(Seq(rewrite(m.body)) ++ snippet(readStatements.mkString("\n")) ++ endpoint))
      nextName
    }
    build(root, root, root, true)
    if (covered.toSet != byPath.keySet) fail("inventory contains unsupported/hidden/unreachable state")
    // Re-resolve instance signatures and types after adding ports; compilation
    // preserves the specialized module boundaries and lowers synchronous reset.
    val circuit = input.circuit.copy(modules = generated.toSeq)
    val (translated, annotationBinding) = GeneratedStateBankAnnotations.translate(resolvedPrepared, clones.toSeq)
    val resolved = new Compiler(Forms.LowForm).execute(
      CircuitState(Parser.parse(circuit.serialize), firrtl.ChirrtlForm, translated)).copy(form = LowForm)
    val domains = originalDomains
    require(domains == clockDomains(resolved.circuit, separateAccessClock, gatePaths.toSet, instrumented = true),
      "checkpoint bank changed functional clock provenance")
    val binding = SemanticStateInventory.canonical(JObject(List(
      "access_version" -> JInt(if (chunked) 2 else 1),
      "protocol" -> JString(if (chunked) "latched-banked64-chunked-v2" else "latched-banked64-v1"),
      "manifest_sha256" -> JString(SemanticStateInventory.sha256(actual)),
      "reference_sha256" -> JString(SemanticStateInventory.sha256(input.circuit.serialize)),
      "instrumented_sha256" -> JString(SemanticStateInventory.sha256(resolved.circuit.serialize)),
      "scope" -> JString("all_reachable_firrtl_state_only"),
      "access_clock" -> JString(if (separateAccessClock) "ckpt_clock" else
        input.circuit.modules.find(_.name == root).get.ports.find(_.tpe == ClockType).get.name),
      "freeze_contract" -> JString("externally_held_all_functional_owners_before_access_until_drain"),
      "functional_clock_domains" -> domains,
      "annotation_translation" -> annotationBinding,
      "state" -> JArray(storage.sortBy(e => (e \ "state_id").values.toString).toList)) ++
      (if (gatePaths.isEmpty) Nil else List("clock_gate_adapters" -> GeneratedClockGateAdapter.binding(gatePaths.toSet)))))
    Result(resolved, entries, binding)
  }
}
