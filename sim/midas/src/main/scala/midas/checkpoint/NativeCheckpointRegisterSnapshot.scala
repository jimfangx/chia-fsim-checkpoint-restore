// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, Parser, WRef}
import firrtl.ir._
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._

/** Coherent staging of a concrete register/async-memory live subtree.
  * Original declarations/equations/clocks keep running, including the control
  * bus used for extraction. Added observation is combinational until the one
  * raw-clock staging edge. Cut/reset/host certificates are external obligations,
  * and original storage never gains a restore mapping. Whole staging-owner
  * restore is explicit opt-in and requires separately parked original owners.
  */
object NativeCheckpointRegisterSnapshot {
  import SemanticStateInventory.{canonical, sha256}
  val prefix = "checkpoint_register_snapshot_"
  val observation = prefix + "live"
  val instance = prefix + "adapter"
  val definition = "CheckpointNativeStateSnapshot"
  case class Result(source: CircuitState, state: CircuitState, path: Seq[String], rawClock: String,
      binding: String, files: Seq[GoldenGateOutputFileAnnotation], includeAsyncMemories: Boolean = false,
      restoreStaging: Boolean = false)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "native register snapshot: " + why)
  private def all(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  def generate(source: CircuitState, path: Seq[String], rawClock: String,
      includeAsyncMemories: Boolean = false, restoreStaging: Boolean = false): Result = {
    check(source.form == LowForm && path.nonEmpty && path.head == source.circuit.main &&
      path.forall(_.matches("[A-Za-z_][A-Za-z0-9_$]*")), "rooted concrete path required")
    val modules = source.circuit.modules.map(m => m.name -> m).toMap
    check(modules.size == source.circuit.modules.size, "duplicate definitions")
    val live = GeneratedExternalOwners.instances(source, source.circuit.main).toMap
    val selectedPath = path.mkString(".")
    check(live.contains(selectedPath), "stale execution")
    def local(m: DefModule): Module = m match {
      case x: Module => x
      case _ => throw new IllegalArgumentException("native register snapshot: opaque execution " + m.name)
    }
    val selected = local(live(selectedPath))
    val chain = path.indices.map(i => local(live(path.take(i + 1).mkString("."))))
    check(chain.map(_.name).distinct.size == chain.size, "recursive selection")
    def statements(m: Module) = all(m.body)
    def children(m: Module) = statements(m).collect { case i: firrtl.WDefInstance => i }.sortBy(_.name)
    def registers(m: Module) = statements(m).collect { case r: DefRegister => r }.sortBy(_.name)
    def memories(m: Module) = statements(m).collect { case r: DefMemory => r }.sortBy(_.name)
    def readers(m: DefMemory): Seq[String] = (0 until m.depth.toInt).map(i => prefix + "read_" + i)
    def memoryBits(m: DefMemory): Int = (firrtl.bitWidth(m.dataType) * m.depth).toInt
    def clockInput(m: Module): String = {
      val clocks = m.ports.filter(p => p.direction == Input && p.tpe == ClockType)
      check(clocks.size == 1, "one raw clock input required: " + m.name)
      clocks.head.name
    }
    def clock(m: Module, expression: Expression, seen: Set[String] = Set.empty): String = {
      check(!seen(expression.serialize), "clock alias cycle")
      expression match {
        case r: WRef if r.tpe == ClockType && m.ports.exists(p =>
            p.name == r.name && p.direction == Input && p.tpe == ClockType) => r.name
        case r: WRef if r.tpe == ClockType =>
          val drivers = statements(m).collect {
            case n: DefNode if n.name == r.name => n.value
            case c: Connect if c.loc.serialize == r.name => c.expr
          }
          check(drivers.size == 1, "ambiguous raw clock alias")
          clock(m, drivers.head, seen + expression.serialize)
        case _ => throw new IllegalArgumentException("native register snapshot: nontransparent raw clock " + expression.serialize)
      }
    }
    def childClock(parent: Module, child: String, childModule: Module): Unit = {
      val pin = child + "." + clockInput(childModule)
      val drivers = statements(parent).collect { case c: Connect if c.loc.serialize == pin => c.expr }
      check(drivers.size == 1 && clock(parent, drivers.head) == clockInput(parent),
        "child does not share raw clock: " + pin)
    }
    def memoryClock(m: Module, expression: Expression, enable: Expression,
        seen: Set[String] = Set.empty): String = {
      def driver(e: Expression): Option[Expression] = e match {
        case r: WRef =>
          val ds = statements(m).collect {
            case n: DefNode if n.name == r.name => n.value
            case c: Connect if c.loc.serialize == r.name => c.expr
          }
          check(ds.size <= 1, "ambiguous memory clock/enable alias")
          ds.headOption
        case _ => None
      }
      def identity(e: Expression, visited: Set[String] = Set.empty): String = {
        check(!visited(e.serialize), "memory enable alias cycle")
        e match {
          case Mux(cond, UIntLiteral(one, _), UIntLiteral(zero, _), _) if one == 1 && zero == 0 =>
            identity(cond, visited + e.serialize)
          case _ => driver(e).map(d => identity(d, visited + e.serialize)).getOrElse(e.serialize)
        }
      }
      check(!seen(expression.serialize), "memory clock alias cycle")
      expression match {
        case ValidIf(cond, value, _) =>
          check(identity(cond) == identity(enable), "memory clock validity guard differs from enable")
          clock(m, value)
        case _ => driver(expression).map(d => memoryClock(m, d, enable, seen + expression.serialize))
          .getOrElse(clock(m, expression))
      }
    }
    check(clockInput(chain.head) == rawClock, "wrong root clock authority")
    chain.dropRight(1).indices.foreach(i => childClock(chain(i), path(i + 1), chain(i + 1)))
    val widths = scala.collection.mutable.LinkedHashMap.empty[String, Int]
    def width(m: Module, ancestors: Set[String]): Int = {
      check(!ancestors(m.name), "recursive subtree")
      if (widths.contains(m.name)) return widths(m.name)
      val ss = statements(m)
      val names = m.ports.map(_.name) ++ ss.collect { case d: IsDeclaration => d.name }
      check(names.distinct.size == names.size, "duplicate declaration")
      check(!ss.exists {
        case _: DefMemory => !includeAsyncMemories
        case _: firrtl.CDefMemory | _: firrtl.CDefMPort | _: firrtl.WDefInstanceConnector => true
        case _ => false
      }, "memory or unresolved storage requires its own adapter: " + m.name)
      val regs = registers(m)
      val registerBits = regs.map { r =>
        check(clock(m, r.clock) == clockInput(m), "register does not share raw clock: " + m.name + "." + r.name)
        r.tpe match {
          case UIntType(IntWidth(w)) if w > 0 && w <= 1048576 => w
          case SIntType(IntWidth(w)) if w > 0 && w <= 1048576 => w
          case _ => throw new IllegalArgumentException("native register snapshot: unsupported register shape")
        }
      }.sum
      val ramBits = memories(m).map { ram =>
        check(includeAsyncMemories && ram.depth > 0 && ram.depth <= 1024 &&
          ram.readLatency == 0 && ram.writeLatency == 1 && ram.writers.size == 1 &&
          ram.readwriters.isEmpty && ram.readUnderWrite == ReadUnderWrite.Undefined,
          "only bounded asynchronous single-writer memories are supported: " + m.name + "." + ram.name)
        check(ram.dataType match {
          case UIntType(IntWidth(w)) => w > 0 && w <= 1048576
          case SIntType(IntWidth(w)) => w > 0 && w <= 1048576
          case _ => false
        }, "unsupported memory element type")
        check(!(ram.readers ++ ram.writers).exists(_.startsWith(prefix)), "reserved memory port collision")
        (ram.readers ++ ram.writers).foreach { p =>
          val pin = ram.name + "." + p + ".clk"
          val drivers = ss.collect { case c: Connect if c.loc.serialize == pin => c.expr }
          val enables = ss.collect { case c: Connect if c.loc.serialize == ram.name + "." + p + ".en" => c.expr }
          check(drivers.size == 1 && enables.size == 1 &&
            memoryClock(m, drivers.head, enables.head) == clockInput(m),
            "memory port does not share raw clock: " + m.name + "." + pin)
        }
        val count = firrtl.bitWidth(ram.dataType) * ram.depth
        check(count <= 1048576, "memory exceeds bounded vector width")
        count
      }.sum
      val own = registerBits + ramBits
      val childBits = children(m).map { i =>
        val child = local(modules(i.module))
        val bits = width(child, ancestors + m.name)
        if (bits > 0) childClock(m, i.name, child)
        BigInt(bits)
      }.sum
      check(own + childBits <= 1048576, "observation exceeds bounded vector width")
      val result = (own + childBits).toInt
      widths(m.name) = result; result
    }
    val bits = width(selected, Set.empty)
    check(bits > 0, "empty register subtree")
    val changedNames = widths.collect { case (n,w) if w > 0 => n }.toSet ++ chain.map(_.name)
    changedNames.foreach { name =>
      val m = local(modules(name))
      check(!(m.ports.map(_.name) ++ statements(m).collect { case d: IsDeclaration => d.name })
        .exists(_.startsWith(prefix)), "reserved identifier collision: " + name)
    }
    check(!source.circuit.modules.exists {
      case m: ExtModule => m.name == definition || m.defname == definition
      case m => m.name == definition
    }, "staging definition collision")
    var offset = 0
    def rows(m: Module, concrete: String): Seq[JValue] = {
      val own = registers(m).map { r =>
        val w = firrtl.bitWidth(r.tpe).toInt
        val drivers = statements(m).collect { case c: Connect if c.loc.serialize == r.name => c }
        check(drivers.size == 1, "register requires one resolved next-state equation")
        val hierarchy = concrete + "." + r.name
        val row = JObject(List("state_id" -> JString(SimulatorStateInventory.stateID(hierarchy, "register")),
          "hierarchy" -> JString(hierarchy), "module" -> JString(m.name), "width" -> JInt(w),
          "offset" -> JInt(offset), "signed" -> JBool(r.tpe.isInstanceOf[SIntType]),
          "declaration_sha256" -> JString(sha256(r.serialize)),
          "driver_sha256" -> JString(sha256(drivers.head.serialize))))
        offset += w; row
      }
      val ram = memories(m).map { r =>
        val hierarchy = concrete + "." + r.name
        val row = JObject(List("state_id" -> JString(SimulatorStateInventory.stateID(hierarchy, "memory")),
          "hierarchy" -> JString(hierarchy), "module" -> JString(m.name), "width" -> JInt(memoryBits(r)),
          "offset" -> JInt(offset), "signed" -> JBool(r.dataType.isInstanceOf[SIntType]),
          "kind" -> JString("memory"), "depth" -> JInt(r.depth),
          "element_width" -> JInt(firrtl.bitWidth(r.dataType)),
          "declaration_sha256" -> JString(sha256(r.serialize))))
        offset += memoryBits(r); row
      }
      own ++ ram ++ children(m).flatMap(i => rows(local(modules(i.module)), concrete + "." + i.name))
    }
    val layout = rows(selected, selectedPath)
    check(offset == bits, "incomplete register layout")
    val restoreInputs = if (restoreStaging) Seq("restore_hold" -> 1, "restore_load" -> 1,
      "restore_state" -> (bits + 71)) else Nil
    val restoreOutputs = if (restoreStaging) Seq("restore_ready" -> 1) else Nil
    val inputs = Seq("reset" -> 1, "request" -> 1, "release_snapshot" -> 1,
      "owners_present" -> 1, "functional_frozen" -> 1, "target_cycle" -> 64, "word_address" -> 32)
    val outputs = Seq("request_ready" -> 1, "valid" -> 1, "failed" -> 1, "word_data" -> 64,
      "word_error" -> 1, "cut_cycle" -> 64, "error" -> 6, "control_state" -> (bits + 71))
    def port(n: String, d: Direction, w: Int) = Port(NoInfo, n, d, UIntType(IntWidth(w)))
    val ext = ExtModule(NoInfo, definition, Seq(Port(NoInfo, "clock", Input, ClockType)) ++
      (inputs ++ Seq("restore_hold" -> 1, "restore_load" -> 1, "restore_state" -> (bits + 71),
        "reader_idle" -> 1, "borrow_attempt" -> 1, "live_state" -> bits)).map { case (n,w) => port(n, Input, w) } ++
      (outputs ++ Seq("restore_ready" -> 1, "borrow_enable" -> 1, "snapshot_state" -> bits))
        .map { case (n,w) => port(n, Output, w) }, definition,
      Seq(IntParam("BITS", bits), IntParam("RESTORE_ENABLE", if (restoreStaging) 1 else 0)))
    def parsed(lines: Seq[String]): Statement = Parser.parse("circuit W :\n  module W :\n" +
      lines.map("    " + _ + "\n").mkString).modules.head.asInstanceOf[Module].body
    val changed = source.circuit.modules.map {
      case m: Module if changedNames(m.name) =>
        val expressions = if (widths.getOrElse(m.name, 0) > 0) {
          registers(m).map(r => if (r.tpe.isInstanceOf[SIntType]) s"asUInt(${r.name})" else r.name) ++
            memories(m).map { ram =>
              readers(ram).map { p =>
                val value = ram.name + "." + p + ".data"
                if (ram.dataType.isInstanceOf[SIntType]) s"asUInt($value)" else value
              }.reverse.reduceLeft((a,b) => s"cat($a, $b)")
            } ++
            children(m).filter(i => widths(i.module) > 0).map(i => i.name + "." + observation)
        } else Nil
        val index = chain.indexWhere(_.name == m.name)
        // An ancestor cannot also occur inside the selected subtree.
        check(index < 0 || index == chain.size - 1 || !widths.contains(m.name), "overlapping ancestor")
        val vector = if (expressions.nonEmpty) expressions.reverse.reduceLeft((a,b) => s"cat($a, $b)")
          else path(index + 1) + "." + observation
        val obsBits = if (expressions.nonEmpty) widths(m.name) else bits
        val root = m.name == source.circuit.main
        val attach = if (!root) Nil else Seq(s"inst $instance of $definition",
          s"$instance.clock <= $rawClock", s"$instance.live_state <= $observation",
          s"$instance.reader_idle <= UInt<1>(1)",
          s"$instance.borrow_attempt <= UInt<1>(0)") ++
          (if (restoreStaging) restoreInputs.map { case (n,_) => s"$instance.$n <= $prefix$n" }
           else Seq(s"$instance.restore_hold <= UInt<1>(0)", s"$instance.restore_load <= UInt<1>(0)",
             s"$instance.restore_state <= UInt<${bits + 71}>(0)")) ++
          restoreOutputs.map { case (n,_) => s"$prefix$n <= $instance.$n" } ++
          inputs.map { case (n,_) => s"$instance.$n <= $prefix$n" } ++
          outputs.map { case (n,_) => s"$prefix$n <= $instance.$n" }
        def addReaders(s: Statement): Statement = s match {
          case ram: DefMemory if includeAsyncMemories && widths.getOrElse(m.name, 0) > 0 =>
            ram.copy(readers = ram.readers ++ readers(ram))
          case other => other.mapStmt(addReaders)
        }
        val memoryRoutes = (if (includeAsyncMemories && widths.getOrElse(m.name, 0) > 0) memories(m) else Nil)
          .flatMap { ram => readers(ram).zipWithIndex.flatMap { case (p,i) =>
          Seq(s"${ram.name}.$p.addr <= UInt<${math.max(1, (ram.depth - 1).bitLength)}>($i)",
            s"${ram.name}.$p.en <= UInt<1>(1)", s"${ram.name}.$p.clk <= ${clockInput(m)}")
        }}
        m.copy(ports = (m.ports :+ port(observation, Output, obsBits)) ++
          (if (root) (inputs ++ restoreInputs).map { case (n,w) => port(prefix + n, Input, w) } ++
            (outputs ++ restoreOutputs).map { case (n,w) => port(prefix + n, Output, w) } else Nil),
          body = Block(Seq(addReaders(m.body), parsed(memoryRoutes ++ Seq(s"$observation <= $vector") ++ attach))))
      case m => m
    } :+ ext
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(
      firrtl.passes.ToWorkingIR.run(source.circuit.copy(modules = changed))))
    val resolved = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(resolved); firrtl.passes.CheckFlows.run(resolved)
    val resource = scala.io.Source.fromResource("CheckpointNativeStateSnapshot.sv")
    val body = try resource.mkString finally resource.close()
    val files = Seq(GoldenGateOutputFileAnnotation(body, ".register-snapshot.sv"))
    val binding = canonical(JObject(List("snapshot_version" -> JInt(if (restoreStaging) 3 else if (includeAsyncMemories) 2 else 1),
      "scope" -> JString(if (includeAsyncMemories) "concrete_raw_clock_register_async_memory_subtree_staging_only"
        else "concrete_raw_clock_register_subtree_staging_only"),
      "selected_execution" -> JString(selectedPath), "raw_clock" -> JString(rawClock),
      "source_sha256" -> JString(sha256(source.circuit.serialize)),
      "source_annotations_sha256" -> JString(sha256(firrtl.annotations.JsonProtocol.serialize(source.annotations))),
      "instrumented_sha256" -> JString(sha256(resolved.serialize)), "bits" -> JInt(bits),
      "words" -> JInt((bits + 63) / 64), "state" -> JArray(layout.toList),
      "staging_source_sha256" -> JString(sha256(body)), "staging_owner_bits" -> JInt(bits + 71),
      "sampling_contract" -> JString(if (includeAsyncMemories)
        "all_original_registers_and_async_memory_words_before_one_positive_raw_clock_edge"
        else "all_original_registers_before_one_positive_raw_clock_edge"),
      "source_registers_keep_running" -> JBool(true), "cut_certificates_bound" -> JBool(false),
      "host_owner_bound" -> JBool(false), "original_restore_supported" -> JBool(false),
      "staging_restore_supported" -> JBool(restoreStaging), "complete_fpga_capture" -> JBool(false),
      "deployable_capture_image" -> JBool(false), "metasim_restore_supported" -> JBool(false)) ++
      (if (restoreStaging) List("include_async_memories" -> JBool(includeAsyncMemories),
        "staging_restore_contract" -> JString("whole_owner_on_raw_clock_original_and_target_owners_parked_separately")) else Nil)))
    Result(source, source.copy(circuit = resolved), path, rawClock, binding, files, includeAsyncMemories, restoreStaging)
  }
  def validate(result: Result): Unit = check(result == generate(result.source, result.path, result.rawClock,
    result.includeAsyncMemories, result.restoreStaging),
    "changed source, equations, observation routes, layout or staging owner")
}
