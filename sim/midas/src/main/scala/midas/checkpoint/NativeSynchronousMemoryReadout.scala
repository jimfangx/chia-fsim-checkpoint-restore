// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, LowForm, Parser, WRef}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** A local write-first queue adapter. The existing synchronous RAM reader is
  * borrowed only under hold. Its original address owner remains separately
  * readable, and its public data is shadowed until the native port is primed.
  * Native RAM uses a data-register/read-first template with registered full-word
  * collision forwarding to preserve the original write-first functional result.
  * Its derived data/forwarding pipelines have no complete capture ownership.
  * Access and functional clocks must be the same running physical clock; this
  * is a caller obligation, not a CDC adapter or a complete capture owner.
  */
object NativeSynchronousMemoryReadout {
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "Native synchronous memory readout: " + why)
  private def statements(s: Statement): Seq[Statement] = {
    val out = mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  private def snippet(body: String): Seq[Statement] = {
    val m = Parser.parse("circuit Adapter :\n  module Adapter :\n" +
      body.linesIterator.map("    " + _).mkString("\n") + "\n").modules.head.asInstanceOf[Module]
    m.body match { case Block(xs) => xs; case x => Seq(x) }
  }
  val endpointRegisters: Set[String] = Set("ckpt_native_phase", "ckpt_native_bank", "ckpt_native_slot",
    "ckpt_native_address", "ckpt_native_active", "ckpt_native_request")

  private def policy(memories: Seq[String]): JObject = JObject(List(
    "adapter_version" -> JInt(2), "original_ram_ports_retained" -> JBool(true),
    "access_clock_contract" -> JString("same_running_original_raw_clock"),
    "response_latency_edges" -> JInt(3), "original_reader_reprimed_before_response" -> JBool(true),
    "cancel_reprime_before_ready" -> JBool(true), "public_reader_shadowed" -> JBool(true),
    "native_read_under_write" -> JString("old"),
    "original_read_under_write" -> JString("new"),
    "write_first_forwarding" -> JString("registered_hit_and_data_at_original_ram_edge"),
    "derived_readout_state_complete_capture_owner" -> JBool(false),
    "memories" -> JArray(memories.sorted.map(JString(_)).toList)))

  def validateBinding(result: GeneratedStateBankAccess.Result): Unit = {
    val doc = parse(result.binding)
    val root = result.state.circuit.modules.find(_.name == result.state.circuit.main).get.asInstanceOf[Module]
    val all = statements(root.body)
    val endpoints = all.collect { case r: DefRegister if endpointRegisters(r.name) => r }
    val present = doc \ "synchronous_memory_readout"
    if (present != JNothing || endpoints.nonEmpty) {
      val memories = all.collect { case m: DefMemory => m }
      check(memories.nonEmpty && memories.forall(m => m.readLatency == 1 && m.writeLatency == 1 &&
        m.readUnderWrite == ReadUnderWrite.Old && m.readers.size == 1 && m.writers.size == 1 && m.readwriters.isEmpty),
        "changed native RAM shape")
      check(present == policy(memories.map(_.name)) &&
        (doc \ "instrumented_sha256") == JString(SemanticStateInventory.sha256(result.state.circuit.serialize)) &&
        endpoints.map(_.name).toSet == endpointRegisters && endpoints.size == endpointRegisters.size &&
        endpoints.forall(_.clock.serialize == "ckpt_clock"), "changed adapter policy, endpoint clock or circuit")
      val clocks = root.ports.filter(p => p.direction == Input && p.tpe == ClockType && p.name != "ckpt_clock")
      val registers = all.collect { case r: DefRegister => r.name -> r }.toMap
      check(clocks.size == 1 && memories.forall { m =>
        Seq(("CheckpointNativeForwardHit_" + m.name, UIntType(IntWidth(1))),
          ("CheckpointNativeForwardData_" + m.name, m.dataType)).forall { case (name, tpe) =>
          registers.get(name).exists(r => r.tpe == tpe && r.clock.serialize == clocks.head.name)
        }
      }, "missing or changed registered write-first forwarding")
    }
  }

  /** Opt in only original write-first synchronous local RAMs. Other historical
    * shell profiles retain their existing bank implementation byte for byte.
    * Once opted in, unsupported shapes and clocks fail rather than falling back
    * to a combinational checkpoint reader.
    */
  def needed(reference: CircuitState): Boolean = reference.circuit.modules.exists {
    case m: Module => statements(m.body).exists {
      case mem: DefMemory => mem.readLatency == 1 && mem.readUnderWrite == ReadUnderWrite.New
      case _ => false
    }
    case _ => false
  }

  def generate(reference: CircuitState, banks: GeneratedStateBankAccess.Result): GeneratedStateBankAccess.Result = {
    check(reference.form == LowForm && reference.circuit.modules.size == 1 &&
      banks.state.circuit.modules.size == 1, "one local concrete module required")
    val original = reference.circuit.modules.head.asInstanceOf[Module]
    val banked = banks.state.circuit.modules.head.asInstanceOf[Module]
    val all = statements(original.body)
    val drivers = all.collect { case c: Connect => c.loc.serialize -> c.expr; case n: DefNode => n.name -> n.value }.toMap
    val aliases = all.collect { case n: DefNode => n.name; case w: DefWire => w.name }.toSet
    val clockPorts = original.ports.filter(p => p.direction == Input && p.tpe == ClockType)
    check(clockPorts.size == 1, "one original functional clock required")
    val clock = clockPorts.head.name
    def constantOne(e: Expression, seen: Set[String] = Set.empty): Boolean = {
      check(!seen(e.serialize), "enable alias cycle")
      e match {
        case UIntLiteral(v, _) => v == 1
        case Mux(cond, t, f, _) => if (constantOne(cond, seen + e.serialize)) constantOne(t, seen + e.serialize)
          else if (cond == UIntLiteral(0, IntWidth(1))) constantOne(f, seen + e.serialize) else false
        case _ if aliases(e.serialize) && drivers.contains(e.serialize) => constantOne(drivers(e.serialize), seen + e.serialize)
        case _ => false
      }
    }
    def clockRoot(e: Expression, enable: Expression, seen: Set[String] = Set.empty): String = {
      check(!seen(e.serialize), "clock alias cycle")
      e match {
        case r: WRef if r.name == clock => clock
        case ValidIf(cond, value, ClockType) =>
          // GeneratedStateBankAccess already proves the port enable guard;
          // require the same exact original connection here as well.
          def unalias(v: Expression, seen: Set[String] = Set.empty): Expression = {
            check(!seen(v.serialize), "port enable alias cycle")
            if (aliases(v.serialize) && drivers.contains(v.serialize)) unalias(drivers(v.serialize), seen + v.serialize)
            else v match {
              case Mux(cond, UIntLiteral(a, _), UIntLiteral(b, _), _) if a == 1 && b == 0 => unalias(cond, seen + v.serialize)
              case _ => v
            }
          }
          check(unalias(cond).serialize == unalias(enable).serialize ||
            (constantOne(cond) && constantOne(enable)), "memory validif/enable mismatch")
          clockRoot(value, enable, seen + e.serialize)
        case _ if aliases(e.serialize) && drivers.contains(e.serialize) => clockRoot(drivers(e.serialize), enable, seen + e.serialize)
        case _ => throw new IllegalArgumentException("Native synchronous memory readout: ungated original root clock required")
      }
    }
    val memories = all.collect { case m: DefMemory => m }
    check(memories.nonEmpty, "missing synchronous memory")
    memories.foreach { mem =>
      check(mem.readLatency == 1 && mem.writeLatency == 1 && mem.readUnderWrite == ReadUnderWrite.New &&
        mem.readers.size == 1 && mem.writers.size == 1 && mem.readwriters.isEmpty,
        "only one-reader one-writer write-first synchronous RAM supported: " + mem.name)
      val reader = mem.readers.head
      check(constantOne(drivers(s"${mem.name}.$reader.en")), "reader enable must be always one: " + mem.name)
      (mem.readers ++ mem.writers).foreach { p =>
        check(clockRoot(drivers(s"${mem.name}.$p.clk"), drivers(s"${mem.name}.$p.en")) == clock,
          "memory clock differs from original root")
      }
    }
    val bankStatements = statements(banked.body)
    val bankDrivers = bankStatements.collect { case c: Connect => c.loc.serialize -> c.expr }.toMap
    val bankMemories = bankStatements.collect { case m: DefMemory => m.name -> m }.toMap
    val names = (banked.ports.map(_.name) ++ bankStatements.collect { case d: IsDeclaration => d.name }).toSet
    val holdNames = memories.map(mem => mem.name -> ("CheckpointNativeHold_" + mem.name)).toMap
    val forwardHits = memories.map(mem => mem.name -> ("CheckpointNativeForwardHit_" + mem.name)).toMap
    val forwardData = memories.map(mem => mem.name -> ("CheckpointNativeForwardData_" + mem.name)).toMap
    check(!(holdNames.values ++ forwardHits.values ++ forwardData.values).exists(names) &&
      !names("CheckpointNativeBorrowed"), "shadow/forwarding identifier collision")
    // Native RAM is a data-registered read-first template. Recover the original
    // address-register/write-first result with hit AND full-width data sampled
    // at that same RAM edge. A combinational bypass would follow later inputs.
    def functionalData(mem: DefMemory): String =
      s"mux(${forwardHits(mem.name)}, ${forwardData(mem.name)}, ${mem.name}.${mem.readers.head}.data)"
    val removed = mutable.Set("ckpt_pending", "ckpt_saved_data", "ckpt_saved_error", "ckpt_req_ready", "ckpt_resp_valid")
    memories.foreach { mem =>
      Seq("clk", "en", "addr").foreach(f => removed += s"${mem.name}.ckpt_read.$f")
      Seq("clk", "en", "addr").foreach(f => removed += s"${mem.name}.${mem.readers.head}.$f")
      check(bankMemories(mem.name).readers == (mem.readers :+ "ckpt_read"), "unexpected banked memory ports")
    }
    def select(name: String): Expression =
      snippet(s"node v = mux(neq(ckpt_native_phase, UInt<2>(0)), ckpt_native_$name, ckpt_$name)").head.asInstanceOf[DefNode].value
    val selectors = Map("ckpt_bank" -> select("bank"), "ckpt_slot" -> select("slot"),
      "ckpt_address" -> select("address"))
    val dataRefs = memories.map(mem => s"${mem.name}.${mem.readers.head}.data" -> mem).toMap
    def expr(e: Expression): Expression = {
      if (selectors.contains(e.serialize)) selectors(e.serialize)
      else if (dataRefs.contains(e.serialize)) {
        val mem = dataRefs(e.serialize)
        snippet(s"node v = mux(CheckpointNativeBorrowed, ${holdNames(mem.name)}, ${functionalData(mem)})").head.asInstanceOf[DefNode].value
      } else memories.find(mem => e.serialize == s"${mem.name}.ckpt_read.data") match {
        // Checkpoint data must use the borrowed raw RAM result, never the
        // public output shadow. Do not recursively rewrite this replacement.
        case Some(mem) => firrtl.passes.MemPortUtils.memPortField(bankMemories(mem.name), mem.readers.head, "data")
        case None => e.mapExpr(expr)
      }
    }
    def rewrite(s: Statement): Statement = s match {
      case mem: DefMemory if bankMemories.contains(mem.name) => mem.copy(readLatency = 1, readUnderWrite = ReadUnderWrite.Old,
        readers = mem.readers.filterNot(_ == "ckpt_read"))
      case c: Connect if removed(c.loc.serialize) => EmptyStmt
      case c: Connect => c.copy(expr = expr(c.expr))
      case n: DefNode => n.copy(value = expr(n.value))
      case x => x.mapStmt(rewrite)
    }
    val controls = """reg ckpt_native_phase : UInt<2>, ckpt_clock
reg ckpt_native_bank : UInt<8>, ckpt_clock
reg ckpt_native_slot : UInt<16>, ckpt_clock
reg ckpt_native_address : UInt<32>, ckpt_clock
reg ckpt_native_active : UInt<1>, ckpt_clock
reg ckpt_native_request : UInt<1>, ckpt_clock
node ckpt_native_completion = and(ckpt_native_request, not(ckpt_reset))
node ckpt_native_busy = neq(ckpt_native_phase, UInt<2>(0))
node ckpt_native_borrow = and(and(ckpt_freeze, not(ckpt_reset)), or(ckpt_accept, eq(ckpt_native_phase, UInt<2>(1))))
ckpt_native_phase <= mux(ckpt_reset, UInt<2>(2), mux(ckpt_accept, UInt<2>(1), mux(eq(ckpt_native_phase, UInt<2>(1)), UInt<2>(2), UInt<2>(0))))
ckpt_native_bank <= mux(ckpt_accept, ckpt_bank, ckpt_native_bank)
ckpt_native_slot <= mux(ckpt_accept, ckpt_slot, ckpt_native_slot)
ckpt_native_address <= mux(ckpt_accept, ckpt_address, ckpt_native_address)
ckpt_native_active <= mux(ckpt_reset, UInt<1>(0), mux(ckpt_accept, and(ckpt_freeze, ckpt_match), mux(eq(ckpt_native_phase, UInt<2>(2)), UInt<1>(0), ckpt_native_active)))
ckpt_pending <= mux(ckpt_reset, UInt<1>(0), mux(eq(ckpt_native_phase, UInt<2>(2)), ckpt_native_completion, mux(ckpt_resp_ready, UInt<1>(0), ckpt_pending)))
ckpt_saved_data <= mux(eq(ckpt_native_phase, UInt<2>(1)), mux(and(ckpt_native_active, ckpt_freeze), ckpt_data, UInt<64>(0)), mux(and(eq(ckpt_native_phase, UInt<2>(2)), not(ckpt_freeze)), UInt<64>(0), ckpt_saved_data))
ckpt_saved_error <= mux(eq(ckpt_native_phase, UInt<2>(1)), not(and(ckpt_native_active, ckpt_freeze)), mux(and(eq(ckpt_native_phase, UInt<2>(2)), not(ckpt_freeze)), UInt<1>(1), ckpt_saved_error))
ckpt_req_ready <= and(and(not(ckpt_pending), not(ckpt_native_busy)), not(ckpt_reset))
ckpt_resp_valid <= and(and(ckpt_pending, not(ckpt_reset)), not(ckpt_native_busy))
ckpt_native_request <= mux(ckpt_reset, UInt<1>(0), mux(ckpt_accept, UInt<1>(1), mux(eq(ckpt_native_phase, UInt<2>(2)), UInt<1>(0), ckpt_native_request)))"""
    val memoryLogic = memories.flatMap { mem =>
      val r = mem.readers.head
      val saved = s"CheckpointPipeline_${mem.name}_${r}_addr"
      check(names(saved), "missing original address owner")
      val cpAddress = expr(bankDrivers(s"${mem.name}.ckpt_read.addr")).serialize
      val functionalAddress = expr(drivers(s"${mem.name}.$r.addr")).serialize
      val nativeAddress = s"mux(ckpt_native_borrow, $cpAddress, mux(ckpt_freeze, $saved, $functionalAddress))"
      val writer = mem.writers.head
      def writeField(field: String): String = expr(bankDrivers(s"${mem.name}.$writer.$field")).serialize
      Seq(s"reg ${holdNames(mem.name)} : ${mem.dataType.serialize}, $clock",
        s"reg ${forwardHits(mem.name)} : UInt<1>, $clock",
        s"reg ${forwardData(mem.name)} : ${mem.dataType.serialize}, $clock",
        s"${holdNames(mem.name)} <= mux(CheckpointNativeBorrowed, ${holdNames(mem.name)}, ${functionalData(mem)})",
        s"${forwardHits(mem.name)} <= and(and(${writeField("en")}, ${writeField("mask")}), eq($nativeAddress, ${writeField("addr")}))",
        s"${forwardData(mem.name)} <= ${writeField("data")}",
        s"${mem.name}.$r.clk <= $clock", s"${mem.name}.$r.en <= UInt<1>(1)",
        s"${mem.name}.$r.addr <= $nativeAddress")
    }
    // Declarations precede the rewritten combinational network; connection
    // order is irrelevant in LowForm but FIRRTL identifiers must be in scope.
    val extra = snippet(controls + s"\nreg CheckpointNativeBorrowed : UInt<1>, $clock\nCheckpointNativeBorrowed <= ckpt_native_borrow\n" + memoryLogic.mkString("\n"))
    val declarations = extra.collect { case d: DefRegister => d }
    val rest = extra.filterNot(_.isInstanceOf[DefRegister])
    val transformed = banked.copy(body = Block(declarations ++ Seq(rewrite(banked.body)) ++ rest))
    val resolved = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(
      banks.state.circuit.copy(modules = Seq(transformed)).serialize), firrtl.ChirrtlForm, banks.state.annotations)).copy(form = LowForm)
    val doc = parse(banks.binding).asInstanceOf[JObject]
    val binding = SemanticStateInventory.canonical(JObject(doc.obj.filterNot(_._1 == "instrumented_sha256") ++
      List("instrumented_sha256" -> JString(SemanticStateInventory.sha256(resolved.circuit.serialize)),
        "synchronous_memory_readout" -> policy(memories.map(_.name)))))
    val result = banks.copy(state = resolved, binding = binding)
    GeneratedStateBankAccess.validateAnnotationBinding(result)
    result
  }
}
