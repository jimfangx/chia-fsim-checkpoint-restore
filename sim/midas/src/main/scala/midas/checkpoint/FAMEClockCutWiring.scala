// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, LowForm, WDefInstance, WDefInstanceConnector, WRef, WSubField}
import firrtl.annotations.{ModuleTarget, NoTargetAnnotation}
import firrtl.ir._
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._

case class FAMEClockCutWiringAnnotation(primaryDomain: String, owners: Int)
    extends NoTargetAnnotation with Unserializable with HasShellOptions {
  val options = Seq(new ShellOption[String](
    longOption = "simulator-f2-fame-clock-cut",
    toAnnotationSeq = value => {
      val parts = value.split(":", -1)
      require(parts.length == 2 && parts(1).matches("[1-9][0-9]*"),
        "FAME cut requires PRIMARY_DOMAIN:EXTERNAL_OWNER_COUNT")
      Seq(FAMEClockCutWiringAnnotation(parts(0), parts(1).toInt))
    },
    helpText = "Bind one observed FAME execution to a separate F2 collector; requires an explicit primary domain and external owners, no image admission.",
  ))
}

/** Bind the local FAME witness through actual compiler-resolved roles, never
  * a guessed hierarchy or a bridge's ahead-of-target token counter. This is
  * one checked owner in a separate generated collector sibling. It cannot
  * acknowledge satellite, FASED, channel, memory, shell or host owners.
  *
  * The label is the selected domain's APPLIED-edge count. Completed FAME steps
  * and consumed tokens are different quantities. Every domain must satisfy
  * consumed = applied + pending, without 64-bit wrap, and consumed <= steps.
  * Reset and sticky overflow invalidate the witness. The predicate is only
  * sampled by the existing physical collector after its common low-phase stop.
  * No new storage, target edge, or physical-latch restore contract is added.
  */
object FAMEClockCutWiring {
  import SemanticStateInventory.{canonical, sha256}
  val prefix = "checkpoint_fame_cut_"
  val valid = prefix + "consistent"
  val cycle = prefix + "applied_cycle"
  case class Result(state: CircuitState, binding: String, collector: F2ClockResetCutWiring.Result)
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException("FAME clock cut wiring unsupported: " + why)
  private def all(s: Statement): Seq[Statement] = {
    val out = mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  /** VerilogPrep's connectors already have local pin wires. Reopening them
    * and running the emitter again would create those names a second time,
    * potentially shorting the unmasked and masked CE drivers. Move only those
    * existing DefWires to fresh names, with real annotation renames, before
    * expanding the connectors. Ports, registers and their StateIDs stay fixed.
    */
  private def reopen(input: CircuitState): CircuitState = {
    val renames = firrtl.RenameMap()
    val modules = input.circuit.modules.map {
      case m: Module =>
        val statements = all(m.body)
        val wires = statements.collect { case w: DefWire => w.name }.toSet
        val connected = statements.collect { case i: WDefInstanceConnector =>
          i.portCons.collect { case (_, r: WRef) if wires(r.name) => r.name }
        }.flatten.distinct.sorted
        val namespace = firrtl.Namespace(m)
        val names = connected.map(n => n -> namespace.newName("checkpoint_reopened_" + n)).toMap
        names.foreach { case (before, after) =>
          val target = ModuleTarget(input.circuit.main, m.name)
          renames.record(target.ref(before), target.ref(after))
        }
        def expr(e: Expression): Expression = e match {
          case r: WRef if names.contains(r.name) => r.copy(name = names(r.name))
          case x => x.mapExpr(expr)
        }
        def stmt(s: Statement): Statement = s match {
          case w: DefWire if names.contains(w.name) => w.copy(name = names(w.name))
          case x => x.mapStmt(stmt).mapExpr(expr)
        }
        m.copy(body = stmt(m.body))
      case m => m
    }
    PhysicalClockCutPlan.explicitConnections(input.copy(circuit = input.circuit.copy(modules = modules),
      annotations = input.annotations.flatMap(_.update(renames))))
  }
  def validateRequest(as: firrtl.AnnotationSeq): Unit = {
    val requests = as.collect { case a: FAMEClockCutWiringAnnotation => a }
    if (requests.isEmpty) return
    if (requests.size != 1 || !requests.head.primaryDomain.matches("[A-Za-z_][A-Za-z0-9_$]*") ||
        requests.head.owners < 1 || requests.head.owners > 65535 ||
        as.count(_.isInstanceOf[SynchronousResetCutWiringAnnotation]) != 1 ||
        as.exists(_.isInstanceOf[F2ClockResetCutWiringAnnotation]))
      fail("one explicit primary domain, 1..65535 external owners, reset-cut request and no generic-cycle collector required")
    SynchronousResetCutWiring.validateRequest(as)
  }

  def generate(input: CircuitState, manifest: String, plan: String, resetPort: String,
      primaryDomain: String, owners: Int): Result = {
    if (input.form != LowForm || owners < 1 || owners > 65535)
      fail("resolved LowForm and 1..65535 external owners required")
    if (manifest != SimulatorStateInventory.discover(input) || plan != PhysicalClockCutPlan.discover(input, manifest))
      fail("stale complete inventory or physical clock plan")
    FAMEClockProgressBindings.resolve(input)
    val models = input.annotations.collect { case a: FAMEClockProgressModelAnnotation => a }
    if (models.size != 1 || !models.head.domains.contains(primaryDomain))
      fail("exactly one observed model and its explicit primary domain required")
    val model = models.head
    val state = reopen(input)
    FAMEClockProgressBindings.validatePreserved(input, state)
    val rows = FAMEClockProgressBindings.resolve(state)
    def row(domain: String, role: String): JValue = rows.find(r =>
      (r \ "model_identity") == JString(model.model) && (r \ "domain_identity") == JString(domain) &&
        (r \ "role") == JString(role)).getOrElse(fail("missing resolved role"))
    def path(r: JValue): Seq[String] = (r \ "rtl_path").asInstanceOf[JString].s.split("\\.").toSeq
    val modelPath = path(row("", "completed_steps")).dropRight(1)
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    var enclosing = state.circuit.main
    val chain = mutable.ArrayBuffer(enclosing)
    modelPath.drop(1).foreach { instance =>
      val parent = modules(enclosing).asInstanceOf[Module]
      enclosing = all(parent.body).collectFirst { case i: WDefInstance if i.name == instance => i.module }
        .getOrElse(fail("resolved model path disappeared"))
      chain += enclosing
    }
    val bit = UIntType(IntWidth(1)); val word = UIntType(IntWidth(64)); val wide = UIntType(IntWidth(65))
    def ref(domain: String, role: String): Expression = {
      val r = row(domain, role); val p = path(r)
      if (p.dropRight(1) != modelPath || (r \ "storage") != JBool(role != "host_reset"))
        fail("local role storage/path mismatch")
      WRef(p.last, if (Set("consumed", "applied", "completed_steps")(role)) word else bit)
    }
    def op(p: PrimOp, xs: Expression*): Expression = DoPrim(p, xs, Nil, bit)
    def and(a: Expression, b: Expression): Expression = op(firrtl.PrimOps.And, a, b)
    val predicates = model.domains.map { domain =>
      val applied = ref(domain, "applied"); val consumed = ref(domain, "consumed")
      // Keep the carry bit in the equality. max + pending must not alias zero.
      val sum = DoPrim(firrtl.PrimOps.Add, Seq(applied, ref(domain, "pending_flag")), Nil, wide)
      and(op(firrtl.PrimOps.Eq, consumed, sum),
        op(firrtl.PrimOps.Leq, consumed, ref("", "completed_steps")))
    }
    val consistent = predicates.foldLeft(and(op(firrtl.PrimOps.Not, ref("", "overflow")),
      op(firrtl.PrimOps.Not, ref("", "host_reset"))))(and)
    val addedPorts = Seq(Port(NoInfo, valid, Output, bit), Port(NoInfo, cycle, Output, word))
    val rewritten = state.circuit.modules.map {
      case m: Module if chain.contains(m.name) =>
        if ((m.ports.map(_.name) ++ all(m.body).collect { case d: IsDeclaration => d.name }).exists(_.startsWith(prefix)))
          fail("reserved witness identifier collision")
        val index = chain.indexOf(m.name)
        val connections = if (index == chain.size - 1) Seq(
          Connect(NoInfo, WRef(valid, bit), consistent), Connect(NoInfo, WRef(cycle, word), ref(primaryDomain, "applied")))
        else addedPorts.map(p => Connect(NoInfo, WRef(p), WSubField(WRef(modelPath(index + 1)), p.name, p.tpe)))
        m.copy(ports = m.ports ++ addedPorts, body = Block(Seq(m.body) ++ connections))
      case m => m
    }
    val witness = state.copy(circuit = state.circuit.copy(modules = rewritten))
    // Reconstruct the inventory/topology for the added combinational outputs.
    // Every original storage declaration and StateID remains unchanged.
    val inv = SimulatorStateInventory.discover(witness)
    val clockPlan = PhysicalClockCutPlan.discover(witness, inv)
    val collector = F2ClockResetCutWiring.generate(witness, inv, clockPlan, resetPort, cycle, owners + 1)
    val root = collector.state.circuit.modules.collectFirst { case m: Module if m.name == state.circuit.main => m }.get
    val ownerPort = F2ClockResetCutWiring.prefix + "owner_present"
    def expr(e: Expression): Expression = e match {
      case r: WRef if r.name == ownerPort =>
        DoPrim(firrtl.PrimOps.Cat, Seq(WRef(valid, bit), r.copy(tpe = UIntType(IntWidth(owners)))), Nil,
          UIntType(IntWidth(owners + 1)))
      case x => x.mapExpr(expr)
    }
    def statement(s: Statement): Statement = s.mapStmt(statement).mapExpr(expr)
    val connected = root.copy(ports = root.ports.map {
      case p if p.name == ownerPort => p.copy(tpe = UIntType(IntWidth(owners)))
      case p => p
    }, body = statement(root.body))
    val circuit = collector.state.circuit.copy(modules = collector.state.circuit.modules.map {
      case m if m.name == root.name => connected
      case m => m
    })
    val working = firrtl.passes.ToWorkingIR.run(circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    val result = collector.state.copy(circuit = flowed)
    val binding = canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString("local_fame_witness_connected_f2_collector_sibling_only"),
      "reference_sha256" -> JString(sha256(input.circuit.serialize)),
      "reopened_reference_sha256" -> JString(sha256(state.circuit.serialize)),
      "manifest_sha256" -> JString(sha256(manifest)), "plan_sha256" -> JString(sha256(plan)),
      "wired_reference_sha256" -> JString(sha256(result.circuit.serialize)),
      "collector_binding_sha256" -> JString(sha256(collector.binding)),
      "model_identity" -> JString(model.model), "primary_domain" -> JString(primaryDomain),
      "label_role" -> JString("applied"), "label_path" -> (row(primaryDomain, "applied") \ "rtl_path"),
      "fame_owner_bit" -> JInt(owners), "external_owners" -> JInt(owners),
      "predicate" -> JString("not_reset_and_not_overflow_and_all_domains_consumed_eq_65bit_applied_plus_pending_and_consumed_le_completed_steps"),
      "signals" -> JArray(rows.toList), "new_storage_bits" -> JInt(0),
      "global_target_cut_proven" -> JBool(false), "production_freeze_owners_bound" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false))))
    Result(result, binding, collector)
  }

  def validate(input: CircuitState, manifest: String, plan: String, resetPort: String,
      primaryDomain: String, owners: Int, result: Result): Unit = {
    val expected = generate(input, manifest, plan, resetPort, primaryDomain, owners)
    if (result.binding != expected.binding || result.state.circuit.serialize != expected.state.circuit.serialize ||
        result.state.form != expected.state.form || result.state.annotations.toSeq != expected.state.annotations.toSeq ||
        result.collector.binding != expected.collector.binding ||
        result.collector.state.circuit.serialize != expected.collector.state.circuit.serialize ||
        result.collector.reset.binding != expected.collector.reset.binding)
      fail("witness/collector differs from independent regeneration")
  }

  def emit(input: CircuitState, manifest: String, plan: String): Seq[GoldenGateOutputFileAnnotation] = {
    validateRequest(input.annotations)
    val requests = input.annotations.collect { case a: FAMEClockCutWiringAnnotation => a }
    if (requests.isEmpty) return Nil
    if (input.annotations.exists { case a: GoldenGateOutputFileAnnotation =>
      a.fileSuffix.startsWith(".f2-fame-clock-cut"); case _ => false }) fail("existing FAME cut output")
    val request = requests.head
    val reset = input.annotations.collect { case a: SynchronousResetCutWiringAnnotation => a }.head
    val result = generate(input, manifest, plan, reset.resetPort, request.primaryDomain, request.owners)
    val emitter = new SemanticCleanEmitter
    val rtl = new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(result.state.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(emitter.getClass)))).getEmittedCircuit.value
    val files = Seq(".f2-fame-clock-cut.fir" -> result.state.circuit.serialize,
      ".f2-fame-clock-cut.sv" -> rtl, ".f2-fame-clock-cut-binding.json" -> result.binding,
      ".f2-fame-clock-cut-collector.json" -> result.collector.binding)
    files.map { case (suffix, body) => GoldenGateOutputFileAnnotation(body, suffix) } :+
      GoldenGateOutputFileAnnotation(canonical(JObject(List("complete_fpga_capture" -> JBool(false),
        "deployable_capture_image" -> JBool(false), "inputs" -> JObject(files.map { case (n, body) =>
          n -> JString(sha256(body)) }.toList)))), ".f2-fame-clock-cut-emission.json")
  }
}
