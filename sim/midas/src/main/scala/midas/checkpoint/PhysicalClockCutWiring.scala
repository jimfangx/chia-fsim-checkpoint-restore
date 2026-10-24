// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, LowForm, Parser, WDefInstance, WRef, WSubField}
import firrtl.ir._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import midas.stage.GoldenGateOutputFileAnnotation

case object PhysicalClockCutWiringProbeAnnotation extends NoTargetAnnotation with Unserializable with HasShellOptions {
  val options = Seq(new ShellOption[Unit](
    longOption = "simulator-physical-clock-wiring-probe",
    toAnnotationSeq = (_: Unit) => Seq(PhysicalClockCutWiringProbeAnnotation),
    helpText = "Emit separate common-source clock mask wiring; requires a clock plan, never admits a capture image.",
  ))
}

/** Separate generated netlist for integrating the common-low-phase controller.
  * Existing native gates keep their original locations and unmasked CE logic.
  * Their I pins get the raw source through dedicated hierarchy ports, while
  * ordinary root-clock consumers get one new root BUFGCE. Thus the root mask
  * does not cascade into native I pins or stop transport's future raw clock.
  *
  * The caller MUST register mask changes on the raw source's falling edge.
  * This generator supplies neither that controller nor state capture admission.
  * It emits a separate probe, never replaces Golden Gate's deployable netlist;
  * production annotations/XDC, external writers and CE restoration are pending.
  */
object PhysicalClockCutWiring {
  import SemanticStateInventory.{canonical, sha256}
  val prefix = "checkpoint_physical_"
  val mask = prefix + "mask"
  val raw = prefix + "source_clock"
  val ce = prefix + "logical_ce"
  val rootGate = prefix + "root_gate"
  case class Result(state: CircuitState, binding: String)
  private def fail(why: String): Nothing = throw new IllegalArgumentException(s"Physical clock cut wiring unsupported: $why")
  private def all(s: Statement): Seq[Statement] = {
    val out = mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  private def snippet(text: String): Seq[Statement] =
    Parser.parse("circuit Scratch :\n  module Scratch :\n" + text.linesIterator.map("    " + _).mkString("\n") + "\n")
      .modules.head.asInstanceOf[Module].body match { case Block(xs) => xs; case x => Seq(x) }

  def validateRequest(as: firrtl.AnnotationSeq): Unit = {
    val n = as.count(_ == PhysicalClockCutWiringProbeAnnotation)
    if (n == 0) return
    if (n != 1 || as.count(_ == PhysicalClockCutPlanAnnotation) != 1)
      fail("exactly one clock plan and wiring probe request required")
  }

  def generate(input: CircuitState, manifest: String, plan: String): Result = {
    if (input.form != LowForm) fail("resolved LowForm required")
    if (plan != PhysicalClockCutPlan.discover(input, manifest)) fail("stale or altered clock plan")
    val doc = parse(plan)
    val rootClock = (doc \ "common_source_clock").asInstanceOf[JString].s
    val state = PhysicalClockCutPlan.explicitConnections(input)
    val live = GeneratedExternalOwners.instances(state, state.circuit.main)
    val reachable = live.map(_._2.name).toSet
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    val root = modules(state.circuit.main).asInstanceOf[Module]
    if (!root.ports.exists(p => p.name == rootClock && p.direction == Input && p.tpe == ClockType))
      fail("common source must be a scalar root Clock input")
    val primitive = live.collectFirst { case (_, m: ExtModule) if m.defname == "BUFGCE" => m.name }.get
    val paths = mutable.Map.empty[String, Seq[String]]
    def gates(name: String, seen: Set[String] = Set.empty): Seq[String] = paths.getOrElseUpdate(name, {
      if (seen(name)) fail("recursive hierarchy")
      modules(name) match {
        case m: ExtModule if m.defname == "BUFGCE" => Seq("")
        case m: Module => all(m.body).collect { case i: WDefInstance =>
          gates(i.module, seen + name).map(p => if (p.isEmpty) i.name else i.name + "." + p)
        }.flatten.sorted
        case _ => fail("opaque owner")
      }
    })
    val topPaths = gates(root.name)
    if (topPaths.map(root.name + "." + _) != (doc \ "physical_gates").asInstanceOf[JArray].arr.map(
        r => (r \ "hierarchy").asInstanceOf[JString].s)) fail("gate order does not match plan")
    def port(name: String, tpe: Type, direction: Direction): Port = Port(NoInfo, name, direction, tpe)
    val rewritten = state.circuit.modules.map {
      case m: Module if reachable(m.name) =>
        val declarations = m.ports.map(_.name) ++ all(m.body).collect { case d: IsDeclaration => d.name }
        if (declarations.exists(_.startsWith(prefix))) fail(s"reserved identifier collision in ${m.name}")
        val isRoot = m.name == root.name
        val localPaths = gates(m.name)
        val instances = all(m.body).collect { case i: WDefInstance => i.name -> i }.toMap
        val natives = instances.collect { case (name, i) if modules(i.module).isInstanceOf[ExtModule] => name }.toSet
        val needsPorts = isRoot || localPaths.nonEmpty
        val source = if (isRoot) rootClock else raw
        def expression(e: Expression): Expression = e match {
          case WRef(`rootClock`, _, _, _) if isRoot => WSubField(WRef(rootGate), "O", ClockType)
          case x => x.mapExpr(expression)
        }
        def statement(s: Statement): Statement = s match {
          case c: Connect if natives.exists(n => c.loc.serialize == n + ".I") =>
            c.copy(expr = WRef(source, ClockType))
          case c: Connect if natives.exists(n => c.loc.serialize == n + ".CE") =>
            // Save the original logical enable before masking it. A parent
            // observes this complete vector, never the stopped physical O.
            val n = c.loc.serialize.stripSuffix(".CE")
            val wire = prefix + "ce_" + localPaths.indexOf(n)
            Block(Seq(DefWire(NoInfo, wire, UIntType(IntWidth(1))),
              Connect(NoInfo, WRef(wire), expression(c.expr))) ++ snippet(s"${c.loc.serialize} <= and($wire, not($mask))"))
          case x => x.mapStmt(statement).mapExpr(expression)
        }
        val before = if (isRoot) snippet(s"inst $rootGate of $primitive\n$rootGate.I <= $rootClock\n$rootGate.CE <= not($mask)") else Nil
        val added = mutable.ArrayBuffer.empty[Statement]
        instances.toSeq.sortBy(_._1).foreach { case (name, i) => modules(i.module) match {
          case _: Module if gates(i.module).nonEmpty =>
            added ++= snippet(s"$name.$raw <= $source\n$name.$mask <= $mask")
          case _ => ()
        }}
        if (localPaths.nonEmpty) {
          val bits = localPaths.map { p =>
            val head = p.takeWhile(_ != '.')
            if (!p.contains('.')) prefix + "ce_" + localPaths.indexOf(p)
            else {
              val child = instances(head)
              val bit = gates(child.module).indexOf(p.drop(head.length + 1))
              s"bits($head.$ce, $bit, $bit)"
            }
          }
          val vector = bits.reverse.reduceLeft((a, b) => s"cat($a, $b)")
          added ++= snippet(s"$ce <= $vector")
        }
        m.copy(ports = m.ports ++ (if (!needsPorts) Nil else
          (if (isRoot) Nil else Seq(port(raw, ClockType, Input))) ++
          Seq(port(mask, UIntType(IntWidth(1)), Input), port(ce, UIntType(IntWidth(localPaths.size)), Output))),
          body = Block(before ++ Seq(statement(m.body)) ++ added))
      case m => m
    }
    // Resolve fresh port/instance types and flows after connector expansion.
    // Probe annotations are deliberately detached: no unverified production
    // XDC or target rename is silently applied to the generated sibling RTL.
    val circuit = state.circuit.copy(modules = rewritten)
    // Final emitter IR contains internal primops such as addw/subw. They are
    // valid emitter operations but NOT legal textual FIRRTL parser syntax.
    // Resolve the added nets directly, without serializing/reparsing or
    // running arithmetic/memory lowering over the original simulator again.
    val working = firrtl.passes.ToWorkingIR.run(circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed)
    firrtl.passes.CheckFlows.run(flowed)
    val resolved = CircuitState(flowed, LowForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))
    val binding = canonical(JObject(List("wiring_version" -> JInt(1),
      "scope" -> JString("physical_clock_wiring_probe_only"),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "ce_dependency_closure_proven" -> JBool(false), "production_annotations_translated" -> JBool(false),
      "reference_sha256" -> JString(sha256(input.circuit.serialize)),
      "simulator_manifest_sha256" -> JString(sha256(manifest)), "plan_sha256" -> JString(sha256(plan)),
      "wired_reference_sha256" -> JString(sha256(resolved.circuit.serialize)),
      "root_module" -> JString(root.name), "common_source_clock" -> JString(rootClock),
      "mask_port" -> JString(mask), "logical_ce_port" -> JString(ce),
      "mask_contract" -> JString("externally_registered_common_source_falling_edge"),
      "root_gate" -> JString(root.name + "." + rootGate),
      "logical_ce_bit_order" -> JArray(topPaths.map(p => JString(root.name + "." + p)).toList),
      "remaining_obligations" -> JArray(Seq("connected_atomic_cut_controller", "external_ce_reset_and_memory_writer_freeze",
        "physical_latch_bank_and_metasim_restore_mapping", "production_annotation_and_xdc_translation",
        "functional_memory_timing_bridge_host_and_pre_fame_ownership", "connected_ungated_mmio_dma_transport")
        .map(JString(_)).toList))))
    Result(resolved, binding)
  }

  def emit(state: CircuitState, manifest: String, plan: String): Seq[GoldenGateOutputFileAnnotation] = {
    validateRequest(state.annotations)
    if (!state.annotations.contains(PhysicalClockCutWiringProbeAnnotation)) return Nil
    if (state.annotations.exists { case a: GoldenGateOutputFileAnnotation =>
      a.fileSuffix.startsWith(".physical-clock-wiring"); case _ => false }) fail("existing wiring output")
    val result = generate(state, manifest, plan)
    val dependencies = PhysicalClockCutDependencies.discover(state, manifest, plan)
    val emitter = new SemanticCleanEmitter
    val emitted = new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(result.state.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(emitter.getClass))))
    val verilog = emitted.getEmittedCircuit.value
    Seq(GoldenGateOutputFileAnnotation(result.state.circuit.serialize, ".physical-clock-wiring.fir"),
      GoldenGateOutputFileAnnotation(verilog, ".physical-clock-wiring.sv"),
      GoldenGateOutputFileAnnotation(result.binding, ".physical-clock-wiring.json"),
      GoldenGateOutputFileAnnotation(dependencies, ".physical-clock-dependencies.json"),
      GoldenGateOutputFileAnnotation(canonical(JObject(List("wiring_binding_sha256" -> JString(sha256(result.binding)),
        "wired_verilog_sha256" -> JString(sha256(verilog))))), ".physical-clock-wiring-emission.json")) ++
      PhysicalClockStateBank.emit(state, manifest, plan, dependencies)
  }
}
