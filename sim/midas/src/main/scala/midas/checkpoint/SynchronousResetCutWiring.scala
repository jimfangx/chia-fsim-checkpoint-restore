// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, LowForm, WDefInstance, WRef, WSubField}
import firrtl.ir._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import midas.stage.GoldenGateOutputFileAnnotation
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._

case class SynchronousResetCutWiringAnnotation(resetPort: String) extends NoTargetAnnotation with Unserializable with HasShellOptions {
  val options = Seq(new ShellOption[String](
    longOption = "simulator-synchronous-reset-cut",
    toAnnotationSeq = p => Seq(SynchronousResetCutWiringAnnotation(p)),
    helpText = "Route an explicit synchronous root reset in a separate common-clock-cut sibling; no image admission.",
  ))
}

/** Bind every combinational use of a synchronous root reset to the held reset
  * output of an independently owned collector. Includes reset-as-data (which
  * LowForm uses for synchronous resets), memory controls and native CEs. Never
  * infer synchrony from the name "reset". Register outputs terminate the cone;
  * combinational child outputs and asynchronous memory reads do not.
  *
  * This sibling is not a production image: its new input must be connected to
  * CheckpointResetOwnedClockCut.functional_reset, with separate initialization,
  * writer ownership and an ungated collector. Those owners are not supplied here.
  */
object SynchronousResetCutWiring {
  import SemanticStateInventory.{canonical, sha256}
  val heldReset = "checkpoint_reset_functional"
  case class Result(state: CircuitState, audit: String, binding: String)
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException("Synchronous reset cut wiring unsupported: " + why)
  private def statements(s: Statement): Seq[Statement] = {
    val out = mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }

  def validateRequest(as: firrtl.AnnotationSeq): Unit = {
    val requests = as.collect { case a: SynchronousResetCutWiringAnnotation => a }
    if (requests.isEmpty) return
    if (requests.size != 1 || !requests.head.resetPort.matches("[A-Za-z_][A-Za-z0-9_$]*") ||
        as.count(_ == PhysicalClockCutPlanAnnotation) != 1 ||
        as.count(_ == PhysicalClockCutWiringProbeAnnotation) != 1 ||
        as.count(_.isInstanceOf[SimulatorCheckpointInventoryAnnotation]) != 1 ||
        as.contains(SimulatorBankProbeAnnotation) || as.contains(NativeFunctionalBankProbeAnnotation) ||
        as.contains(SemanticFAMEStateMapAnnotation))
      fail("one explicit reset, clock plan, wiring probe and root inventory required; capture probes cannot be combined")
  }

  def emit(input: CircuitState, manifest: String, plan: String): Seq[GoldenGateOutputFileAnnotation] = {
    validateRequest(input.annotations)
    val requests = input.annotations.collect { case a: SynchronousResetCutWiringAnnotation => a }
    if (requests.isEmpty) return Nil
    if (input.annotations.exists {
      case a: GoldenGateOutputFileAnnotation => a.fileSuffix.startsWith(".synchronous-reset-cut")
      case _ => false
    }) fail("existing reset-cut output")
    val result = generate(input, manifest, plan, requests.head.resetPort)
    val e = new SemanticCleanEmitter
    val emitted = new Compiler(Seq(firrtl.options.Dependency.fromTransform(e)), Forms.LowForm, Set(e)).execute(
      result.state.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(e.getClass)))).getEmittedCircuit.value
    val files = Seq(".synchronous-reset-cut.fir" -> result.state.circuit.serialize,
      ".synchronous-reset-cut.sv" -> emitted, ".synchronous-reset-cut-audit.json" -> result.audit,
      ".synchronous-reset-cut-binding.json" -> result.binding)
    files.map { case (name, body) => GoldenGateOutputFileAnnotation(body, name) } :+
      GoldenGateOutputFileAnnotation(canonical(JObject(List("scope" -> JString("reset_owned_clock_cut_sibling_only"),
        "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
        "inputs" -> JObject(files.map { case (name, body) => name -> JString(sha256(body)) }.toList)))),
        ".synchronous-reset-cut-emission.json")
  }

  def discover(input: CircuitState, manifest: String, plan: String, resetPort: String): String = {
    if (input.form != LowForm || plan != PhysicalClockCutPlan.discover(input, manifest))
      fail("resolved original LowForm and exact clock plan required")
    val state = PhysicalClockCutPlan.explicitConnections(input)
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    val root = modules(state.circuit.main).asInstanceOf[Module]
    if (!root.ports.exists(p => p.name == resetPort && p.direction == Input && p.tpe == UIntType(IntWidth(1))))
      fail("reset source must be an explicit scalar UInt<1> root input")
    val live = GeneratedExternalOwners.instances(state, root.name)
    val scopes = live.map { case (path, m) => path -> m.name }.toMap
    case class Definition(module: Module, all: Seq[Statement], drivers: Map[String, Expression],
      registers: Set[String], memories: Map[String, DefMemory], instances: Map[String, String], invalid: Set[String])
    val definitions = live.map(_._2).distinctBy(_.name).collect { case m: Module =>
      val all = statements(m.body)
      if (all.exists(_.isInstanceOf[Conditionally])) fail("conditional statement in final LowForm")
      val assignments = all.collect { case c: Connect => c.loc.serialize -> c.expr } ++
        all.collect { case n: DefNode => n.name -> n.value }
      if (assignments.map(_._1).distinct.size != assignments.size) fail("duplicate driver in " + m.name)
      m.name -> Definition(m, all, assignments.toMap, all.collect { case r: DefRegister => r.name }.toSet,
        all.collect { case r: DefMemory => r.name -> r }.toMap,
        all.collect { case i: WDefInstance => i.name -> i.module }.toMap,
        all.collect { case i: IsInvalid => i.expr.serialize }.toSet)
    }.toMap
    // Symbolic, definition-local input dependencies avoid artificial cycles on
    // valid ready/valid feedback through a child's independent registered output.
    val memo = mutable.Map.empty[(String, String), Set[String]]
    val active = mutable.Set.empty[(String, String)]
    def expression(module: String, e: Expression): Set[String] = e match {
      case _: UIntLiteral | _: SIntLiteral => Set.empty
      case _: WRef | _: WSubField => reference(module, e.serialize)
      case p: DoPrim => p.args.flatMap(expression(module, _)).toSet
      case x: Mux => Seq(x.cond, x.tval, x.fval).flatMap(expression(module, _)).toSet
      case x: ValidIf => Seq(x.cond, x.value).flatMap(expression(module, _)).toSet
      case _ => fail("unresolved reset expression " + module + ": " + e.serialize)
    }
    def reference(module: String, name: String): Set[String] = memo.getOrElseUpdate(module -> name, {
      val key = module -> name
      if (active(key)) fail("combinational cycle at " + module + "." + name)
      active += key
      // Opaque outputs terminate here only because every opaque INPUT is
      // independently audited below. Native clock latch semantics are owned
      // by the physical cut, never inferred as a combinational reset path.
      if (!definitions.contains(module)) {
        if (!modules(module).ports.exists(p => p.name == name && p.direction == Output))
          fail("invalid opaque output " + module + "." + name)
        active -= key
        Set.empty[String]
      } else {
      val d = definitions(module)
      val result = if (d.registers(name)) Set.empty[String]
      else if (d.module.ports.exists(p => p.name == name && p.direction == Input)) Set(name)
      // An explicit FIRRTL invalid has no reset-expression dependency. This
      // does not choose its value, invent an owner, or discharge state coverage.
      else if (d.invalid(name)) Set.empty[String]
      else d.drivers.get(name).map(expression(module, _)).getOrElse {
        name.split('.').toList match {
          case i :: pin :: Nil if d.instances.contains(i) =>
            if (!modules(d.instances(i)).ports.exists(p => p.name == pin && p.direction == Output))
              fail("undriven instance input " + module + "." + name)
            reference(d.instances(i), pin).flatMap(p => reference(module, i + "." + p))
          case ram :: port :: field :: Nil if d.memories.contains(ram) =>
            val mem = d.memories(ram)
            if (mem.readers.contains(port) && field == "data") {
              if (mem.readLatency == 0)
                Seq(s"$ram.$port.addr", s"$ram.$port.en").flatMap(reference(module, _)).toSet
              else Set.empty[String]
            } else if (mem.readwriters.contains(port) && field == "rdata" && mem.readLatency > 0) Set.empty[String]
            else fail("undriven memory reference " + module + "." + name)
          case _ => fail("undriven reset dependency " + module + "." + name)
        }
      }
      active -= key
      result
      }
    })
    val affectedMemo = mutable.Map.empty[(String, String), Boolean]
    def affected(path: String, input: String): Boolean = affectedMemo.getOrElseUpdate(path -> input, {
      if (path == root.name) input == resetPort
      else {
        val parent = path.take(path.lastIndexOf('.'))
        val instance = path.drop(path.lastIndexOf('.') + 1)
        reference(scopes(parent), instance + "." + input).exists(affected(parent, _))
      }
    })
    val rows = mutable.ArrayBuffer.empty[JValue]
    live.foreach { case (path, module) =>
      def touches(e: Expression): Boolean = expression(module.name, e).exists(affected(path, _))
      def sink(kind: String, name: String, e: Expression): Unit = if (touches(e)) {
        if (e.tpe == ClockType || e.tpe == AsyncResetType || e.tpe == ResetType)
          fail("non-synchronous reset sink " + path + "." + name)
        rows += JObject(List("kind" -> JString(kind), "sink" -> JString(path + "." + name),
          "expression" -> JString(e.serialize)))
      }
      module match {
        case m: ExtModule =>
          val pins = m.ports.filter(_.direction == Input).filter(p => affected(path, p.name)).map(_.name).toSet
          if (pins.nonEmpty) {
            if (m.defname != "BUFGCE" || pins != Set("CE")) fail("opaque or clock reset sink " + path)
            PhysicalClockGateContract.validate(input, m)
            rows += JObject(List("kind" -> JString("native_ce"), "sink" -> JString(path + ".CE")))
          }
        case m: Module =>
          val d = definitions(m.name)
          d.all.collect { case r: DefRegister => r }.foreach { r =>
            if (touches(r.clock)) fail("reset controls register clock " + path + "." + r.name)
            if (touches(r.reset) && r.reset.tpe != UIntType(IntWidth(1))) fail("asynchronous reset " + path + "." + r.name)
            sink("synchronous_reset", r.name + ".reset", r.reset)
            sink("reset_value", r.name + ".init", r.init)
            d.drivers.get(r.name).foreach(sink("register_next", r.name, _))
          }
          d.memories.keys.toSeq.sorted.foreach { mem =>
            d.drivers.toSeq.filter(_._1.startsWith(mem + ".")).sortBy(_._1).foreach { case (pin, e) =>
              sink("memory_port", pin, e)
            }
          }
          d.all.collect { case p: Print => p }.foreach { p =>
            if (touches(p.clk)) fail("reset controls print clock " + path)
            (Seq(p.en) ++ p.args).zipWithIndex.foreach { case (e, i) => sink("print", p.name + "." + i, e) }
          }
          d.all.collect { case p: Stop => p }.foreach { p =>
            if (touches(p.clk)) fail("reset controls stop clock " + path)
            sink("stop", p.name, p.en)
          }
          d.all.collect { case p: Verification => p }.foreach { p =>
            if (touches(p.clk)) fail("reset controls verification clock " + path)
            sink("verification_enable", p.name, p.en); sink("verification_predicate", p.name, p.pred)
          }
          if (path == root.name) m.ports.filter(_.direction == Output).foreach { p =>
            if (reference(m.name, p.name).exists(affected(path, _)))
              rows += JObject(List("kind" -> JString("external_output"), "sink" -> JString(path + "." + p.name)))
          }
        case _ => fail("unknown module " + path)
      }
    }
    if (rows.isEmpty) fail("reset source has no live consumers")
    canonical(JObject(List("reset_cut_version" -> JInt(1), "scope" -> JString("complete_combinational_reset_fanout_only"),
      "reference_sha256" -> JString(sha256(input.circuit.serialize)), "simulator_manifest_sha256" -> JString(sha256(manifest)),
      "plan_sha256" -> JString(sha256(plan)), "root_module" -> JString(root.name), "reset_source" -> JString(root.name + "." + resetPort),
      "source_writer" -> JString("external_unbound"), "source_synchrony" -> JString("external_sampling_contract_required"),
      "sequential_consumers_synchronous" -> JBool(true), "production_freeze_owners_bound" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "sinks" -> JArray(rows.sortBy(canonical).toList))))
  }

  def generate(input: CircuitState, manifest: String, plan: String, resetPort: String): Result = {
    val audit = discover(input, manifest, plan, resetPort)
    val wired = PhysicalClockCutWiring.generate(input, manifest, plan)
    val root = wired.state.circuit.modules.collectFirst { case m: Module if m.name == input.circuit.main => m }.get
    if ((root.ports.map(_.name) ++ statements(root.body).collect { case d: IsDeclaration => d.name }).contains(heldReset))
      fail("held-reset identifier collision")
    var replacements = 0
    def expression(e: Expression): Expression = e match {
      case r: WRef if r.name == resetPort => replacements += 1; r.copy(name = heldReset)
      case other => other.mapExpr(expression)
    }
    def rewrite(s: Statement): Statement = s.mapStmt(rewrite).mapExpr(expression)
    val finalRoot = root.copy(ports = root.ports :+ Port(NoInfo, heldReset, Input, UIntType(IntWidth(1))),
      body = rewrite(root.body))
    if (replacements == 0) fail("no root reset references rewritten")
    val circuit = wired.state.circuit.copy(modules = wired.state.circuit.modules.map {
      case m if m.name == root.name => finalRoot
      case m => m
    })
    val working = firrtl.passes.ToWorkingIR.run(circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    val state = wired.state.copy(circuit = flowed)
    val binding = canonical(JObject(List("reset_wiring_version" -> JInt(1), "scope" -> JString("reset_owned_clock_cut_sibling_only"),
      "reference_sha256" -> JString(sha256(input.circuit.serialize)), "reset_audit_sha256" -> JString(sha256(audit)),
      "clock_wiring_sha256" -> JString(sha256(wired.binding)), "wired_reference_sha256" -> JString(sha256(state.circuit.serialize)),
      "raw_reset_port" -> JString(resetPort), "held_reset_port" -> JString(heldReset), "replaced_root_references" -> JInt(replacements),
      "collector_contract" -> JString("separate_initialization_ungated_clock_and_ResetOwnedClockCut_functional_reset"),
      "production_freeze_owners_bound" -> JBool(false), "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false))))
    Result(state, audit, binding)
  }
}
