// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, Parser}
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Observe every state word introduced by the native synchronous reader,
  * including the emitter-owned Old RAM data register. The original StateIDs
  * and bank selectors remain independent. This adds no sampling register:
  * the caller still needs an atomic snapshot and a complete restore owner.
  */
object NativeSynchronousStateObservation {
  val port = "ckpt_native_state"
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "Native synchronous state observation: " + why)
  private def statements(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }

  private[checkpoint] def generate(state: CircuitState, originalBinding: String): (CircuitState, JObject) = {
    check(state.form == LowForm && state.circuit.modules.size == 1, "one resolved local module required")
    val root = state.circuit.modules.head.asInstanceOf[Module]
    val all = statements(root.body)
    check(!(root.ports.map(_.name) ++ all.collect { case d: IsDeclaration => d.name }).contains(port),
      "observation port identifier collision")
    val original = (parse(originalBinding) \ "state").asInstanceOf[JArray].arr
    val banked = original.map(r => (r \ "storage_hierarchy").asInstanceOf[JString].s).toSet
    check(banked.size == original.size, "duplicate original storage mapping")
    val inventory = (parse(SimulatorStateInventory.discover(state)) \ "state").asInstanceOf[JArray].arr
    check(banked.subsetOf(inventory.map(r => (r \ "hierarchy").asInstanceOf[JString].s).toSet),
      "original storage disappeared from native implementation")
    val registers = all.collect { case r: DefRegister => r.name -> r }.toMap
    val implicitReaders = all.collect { case m: DefMemory => m }.flatMap { m =>
      check(m.readLatency == 1 && m.readUnderWrite == ReadUnderWrite.Old && m.readers.size == 1 &&
        m.readwriters.isEmpty, "unsupported implicit RAM pipeline")
      m.readers.map { r =>
        val ref = firrtl.passes.MemPortUtils.memPortField(m, r, "data")
        firrtl.passes.LowerTypes.loweredName(ref) -> ref.serialize
      }
    }.toMap
    val derived = inventory.filterNot(r => banked((r \ "hierarchy").asInstanceOf[JString].s))
      .sortBy(r => (r \ "hierarchy").asInstanceOf[JString].s)
    check(derived.nonEmpty, "missing derived state")
    var offset = BigInt(0)
    val rows = derived.map { r =>
      val path = (r \ "hierarchy").asInstanceOf[JString].s
      val name = path.stripPrefix(root.name + ".")
      val width = (r \ "width").asInstanceOf[JInt].num
      check((r \ "kind") == JString("register") && (r \ "depth") == JInt(1),
        "unsupported derived state shape " + path)
      val expression = if ((r \ "adapter") == JString("firrtl-emitter-memory-pipeline-v1")) {
        implicitReaders.getOrElse(name, throw new IllegalArgumentException("unmapped native RAM data register " + path))
      } else {
        check(registers.contains(name) && (name.startsWith("ckpt_") || name.startsWith("CheckpointNative")),
          "unknown derived register " + path)
        name
      }
      val row = JObject(List("instrumented_state_id" -> (r \ "state_id"), "hierarchy" -> JString(path),
        "expression" -> JString(expression), "width" -> JInt(width), "offset" -> JInt(offset),
        "signed" -> (r \ "signed"),
        "clock_expression" -> (r \ "clock_expression"), "adapter" -> (r \ "adapter")))
      offset += width
      row
    }
    check(offset > 0 && offset.isValidInt, "derived state vector exceeds FIRRTL width")
    val expression = rows.reverse.map { r =>
      val ref = (r \ "expression").asInstanceOf[JString].s
      if ((r \ "signed") == JBool(true)) s"asUInt($ref)" else ref
    }
      .reduceLeft((a, b) => s"cat($a, $b)")
    val connection = Parser.parse(s"circuit Observe :\n  module Observe :\n    $port <= $expression\n")
      .modules.head.asInstanceOf[Module].body
    val observed = state.copy(circuit = state.circuit.copy(modules = Seq(root.copy(
      ports = root.ports :+ Port(NoInfo, port, Output, UIntType(IntWidth(offset))),
      body = Block(Seq(root.body, connection))))))
    val working = firrtl.passes.ToWorkingIR.run(observed.circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val resolved = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(resolved); firrtl.passes.CheckFlows.run(resolved)
    val schema = JObject(List("observation_version" -> JInt(1),
      "scope" -> JString("derived_native_state_live_vector_only"), "port" -> JString(port),
      "bits" -> JInt(offset), "words" -> JInt((offset + 63) / 64), "bit_order" -> JString("lsb0"),
      "state" -> JArray(rows.toList), "original_banked_entries" -> JInt(original.size),
      "includes_implicit_ram_data_pipeline" -> JBool(true),
      "sampling_contract" -> JString("sample_entire_live_vector_at_one_common_raw_clock_cut"),
      "stable_while_functional_freeze" -> JBool(false), "atomic_snapshot_supported" -> JBool(false),
      "complete_capture_owner" -> JBool(false), "metasim_restore_supported" -> JBool(false)))
    (observed.copy(circuit = resolved), schema)
  }
}
