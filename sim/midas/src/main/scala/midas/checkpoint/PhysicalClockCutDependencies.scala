// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, LowForm, WDefInstance, WRef, WSubField}
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Trace the ORIGINAL, unmasked CE cones. Stopping O does not freeze a CE
  * latch, and observing the masked CE would incorrectly prove a constant zero.
  * This audit establishes prerequisites only: external input owners, physical
  * latch sampling/restoration and production capture admission remain separate.
  */
object PhysicalClockCutDependencies {
  import SemanticStateInventory.{canonical, sha256}
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Physical clock cut dependency unsupported: $why")
  private case class Leaf(kind: String, path: String)
  private case class Definition(module: Module, drivers: Map[String, Expression],
    instances: Map[String, String], registers: Set[String], memories: Map[String, DefMemory])

  def discover(input: CircuitState, manifest: String, plan: String): String = {
    if (input.form != LowForm) fail("resolved LowForm required")
    if (plan != PhysicalClockCutPlan.discover(input, manifest)) fail("stale or altered clock plan")
    val state = PhysicalClockCutPlan.explicitConnections(input)
    val live = GeneratedExternalOwners.instances(state, state.circuit.main)
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    val scopes = live.map { case (p, m) => p -> m.name }.toMap
    val definitions = live.map(_._2).distinct.collect { case m: Module =>
      val drivers = mutable.Map.empty[String, Expression]
      val instances = mutable.Map.empty[String, String]
      val registers = mutable.Set.empty[String]
      val memories = mutable.Map.empty[String, DefMemory]
      def driver(name: String, expr: Expression): Unit = {
        if (drivers.contains(name)) fail(s"duplicate driver ${m.name}.$name")
        drivers(name) = expr
      }
      def visit(s: Statement): Unit = {
        s match {
          case c: Connect => driver(c.loc.serialize, c.expr)
          case n: DefNode => driver(n.name, n.value)
          case r: DefRegister =>
            // Complete inventory rejects asynchronous resets. Retain a direct
            // check here: never label such a register stable just from its clock.
            if (r.reset.tpe != UIntType(IntWidth(1))) fail(s"asynchronous reset ${m.name}.${r.name}")
            registers += r.name
          case i: WDefInstance => instances(i.name) = i.module
          case m: DefMemory => memories(m.name) = m
          case _: Conditionally => fail("conditional statement in final LowForm")
          case _ => ()
        }
        s.foreachStmt(visit)
      }
      visit(m.body)
      m.name -> Definition(m, drivers.toMap, instances.toMap, registers.toSet, memories.toMap)
    }.toMap
    // Definition-local summaries retain relative internal state identities.
    // Substitution at each instance preserves distinct repeated/nested owners.
    val memo = mutable.Map.empty[(String, String), Set[Leaf]]
    val active = mutable.Set.empty[(String, String)]
    def expr(module: String, e: Expression): Set[Leaf] = e match {
      case _: UIntLiteral | _: SIntLiteral => Set.empty
      case _: WRef | _: WSubField => ref(module, e.serialize)
      case m: Mux => expr(module, m.cond) ++ expr(module, m.tval) ++ expr(module, m.fval)
      case v: ValidIf => expr(module, v.cond) ++ expr(module, v.value)
      case p: DoPrim => p.args.flatMap(expr(module, _)).toSet
      case _ => fail(s"unresolved expression $module.${e.serialize}")
    }
    def ref(module: String, name: String): Set[Leaf] = memo.getOrElseUpdate((module, name), {
      val key = module -> name
      if (active(key)) fail(s"combinational cycle at $module.$name")
      active += key
      val d = definitions.getOrElse(module, fail(s"opaque data owner $module.$name"))
      val result = if (d.registers(name)) Set(Leaf("state", name))
      else d.module.ports.find(p => p.name == name && p.direction == Input) match {
        case Some(_) => Set(Leaf("input", name))
        case None if d.drivers.contains(name) => expr(module, d.drivers(name))
        case None =>
          val parts = name.split('.').toList
          parts match {
            case instance :: pin :: Nil if d.instances.contains(instance) =>
              val child = d.instances(instance)
              val port = modules(child).ports.find(_.name == pin).getOrElse(fail(s"unknown pin $module.$name"))
              if (port.direction != Output) fail(s"undriven instance input $module.$name")
              modules(child) match {
                case _: ExtModule => fail(s"physical clock used as data $module.$name")
                case _ => ref(child, pin).flatMap {
                  case Leaf("input", p) => ref(module, instance + "." + p)
                  case Leaf(kind, p) => Set(Leaf(kind, instance + "." + p))
                }
              }
            case memory :: port :: field :: Nil if d.memories.contains(memory) =>
              val m = d.memories(memory)
              if (m.readers.contains(port) && field == "data") {
                if (m.readLatency == 0)
                  Set(Leaf("state", memory)) ++ ref(module, s"$memory.$port.addr") ++ ref(module, s"$memory.$port.en")
                else if (m.readLatency == 1) {
                  val saved = if (m.readUnderWrite == ReadUnderWrite.Old) "data" else "addr"
                  val pipeline = firrtl.passes.LowerTypes.loweredName(firrtl.passes.MemPortUtils.memPortField(m, port, saved))
                  Set(Leaf("state", pipeline)) ++ (if (saved == "addr") Set(Leaf("state", memory)) else Set.empty[Leaf])
                } else fail(s"hidden memory pipeline $module.$name")
              } else if (m.readwriters.contains(port) && field == "rdata" && m.readLatency == 1) {
                val pipeline = firrtl.passes.LowerTypes.loweredName(firrtl.passes.MemPortUtils.memPortField(m, port, field))
                Set(Leaf("state", pipeline))
              } else fail(s"unknown memory output $module.$name")
            case _ => fail(s"undriven or invalid data dependency $module.$name")
          }
      }
      active -= key
      result
    })
    def at(path: String, leaf: Leaf): Set[Leaf] = leaf match {
      case Leaf("input", name) if path != state.circuit.main =>
        val parent = path.take(path.lastIndexOf('.'))
        val instance = path.drop(path.lastIndexOf('.') + 1)
        ref(scopes(parent), instance + "." + name).flatMap(at(parent, _))
      case Leaf(kind, name) => Set(Leaf(kind, path + "." + name))
    }
    val inv = (parse(manifest) \ "state").asInstanceOf[JArray].arr.map(r =>
      (r \ "hierarchy").asInstanceOf[JString].s -> r).toMap
    val doc = parse(plan)
    val coverage = (doc \ "state_clock_coverage").asInstanceOf[JArray].arr.map(r =>
      (r \ "hierarchy").asInstanceOf[JString].s -> r).toMap
    val allInputs = mutable.Set.empty[String]
    val rows = (doc \ "physical_gates").asInstanceOf[JArray].arr.map { gate =>
      val path = (gate \ "hierarchy").asInstanceOf[JString].s
      val parent = path.take(path.lastIndexOf('.'))
      val name = path.drop(path.lastIndexOf('.') + 1)
      val leaves = ref(scopes(parent), name + ".CE").flatMap(at(parent, _)).toSeq.sortBy(l => (l.kind, l.path))
      val terminalRows = leaves.map {
        case Leaf("input", p) =>
          val port = modules(state.circuit.main).ports.find(x => state.circuit.main + "." + x.name == p).get
          if (port.tpe == ClockType) fail(s"oscillating root clock used as CE data $p")
          allInputs += p
          JObject(List("kind" -> JString("external_input"), "hierarchy" -> JString(p),
            "type" -> JString(port.tpe.serialize), "freeze_owner" -> JString("unbound")))
        case Leaf("state", p) =>
          val row = inv.getOrElse(p, fail(s"uninventoried CE state $p"))
          if ((row \ "adapter") == JString("metasim-bufgce-v1")) fail(s"physical O used as CE state $p")
          val clocks = coverage.getOrElse(p, fail(s"missing clock coverage for $p")) \ "clock_consumers"
          if (clocks == JArray(Nil)) fail(s"state has no stopped clock consumers $p")
          JObject(List("kind" -> JString("clocked_state"), "hierarchy" -> JString(p),
            "state_id" -> (row \ "state_id"), "adapter" -> (row \ "adapter"), "clock_consumers" -> clocks))
        case l => fail(s"unknown terminal $l")
      }
      JObject(List("hierarchy" -> JString(path), "ce_driver_scope" -> JString(parent),
        "ce_driver_expression" -> (gate \ "ce_driver_expression"), "terminals" -> JArray(terminalRows.toList),
        "external_inputs_require_freeze" -> JBool(leaves.exists(_.kind == "input"))))
    }
    canonical(JObject(List("dependency_version" -> JInt(1), "scope" -> JString("physical_ce_dependency_prerequisites_only"),
      "root_module" -> JString(state.circuit.main), "reference_sha256" -> JString(sha256(input.circuit.serialize)),
      "simulator_manifest_sha256" -> JString(sha256(manifest)), "plan_sha256" -> JString(sha256(plan)),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "asynchronous_resets_supported" -> JBool(false),
      "analysis" -> JString("conservative_combinational_cones_to_complete_clocked_state_or_root_inputs"),
      "physical_gates" -> JArray(rows), "external_inputs_require_freeze" -> JArray(allInputs.toList.sorted.map(JString(_))),
      "remaining_obligations" -> JArray(Seq("bind_external_input_freeze_owners", "physical_latch_bank_and_metasim_restore_mapping",
        "production_annotation_and_xdc_translation", "functional_memory_timing_bridge_host_and_pre_fame_ownership",
        "connected_ungated_mmio_dma_transport").map(JString(_)).toList))))
  }
}
