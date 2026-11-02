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
import org.json4s.native.JsonMethods.parse

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
  /** Combinational local witness only. Existing storage, native gates and their
    * owners remain in place; no collector, mask, reset or transport is added.
    */
  case class Witness(state: CircuitState, binding: String)
  private def sameState(a: CircuitState, b: CircuitState): Boolean =
    a.circuit.serialize == b.circuit.serialize && a.form == b.form &&
      a.annotations.toSeq == b.annotations.toSeq &&
      a.renames.map(_.serialize.linesIterator.toSeq.sorted) == b.renames.map(_.serialize.linesIterator.toSeq.sorted)
  case class Native(banks: GeneratedStateBankAccess.Result, dependencies: String,
      manifest: String, plan: String, witness: CircuitState, staging: F2NativeCollectorSnapshot.Result)
  case class Result(state: CircuitState, binding: String, collector: F2ClockResetCutWiring.Result,
      native: Option[Native] = None)
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
      primaryDomain: String, owners: Int): Result =
    generateImpl(input, manifest, plan, resetPort, primaryDomain, owners, nativeBanks = false)

  /** Own native observer/pending readout with the same applied-edge witness.
    * Stage the running collector and idle response endpoint BEFORE opening any
    * functional bank. The separate raw clock keeps transport alive while the
    * common cut holds original storage. Physical CE dependency checks remain
    * mandatory; this does not own native latch restoration or production state.
    */
  def generateNative(input: CircuitState, manifest: String, plan: String, resetPort: String,
      primaryDomain: String, owners: Int): Result =
    generateImpl(input, manifest, plan, resetPort, primaryDomain, owners, nativeBanks = true)

  /** Reuse the checked applied-edge label in an existing coordinator without
    * instantiating a second clock-cut controller. The consumer must separately
    * prove its common cut and all other owners before accepting this witness.
    */
  def generateWitness(input: CircuitState, manifest: String, plan: String,
      primaryDomain: String): Witness = {
    if (input.form != LowForm) fail("resolved LowForm required")
    if (manifest != SimulatorStateInventory.discover(input) || plan != PhysicalClockCutPlan.discover(input, manifest))
      fail("stale complete inventory or physical clock plan")
    generateWitness(input, primaryDomain)
  }

  /** Resolve only the concrete FAME roles. An enclosing transport may contain
    * separately owned opaque storage; it cannot be admitted by this witness.
    * The production consumer must independently validate that storage and its
    * exact transport ABI, resources, owner bindings and incomplete-image guard.
    */
  def generateWitness(input: CircuitState, primaryDomain: String): Witness = {
    if (input.form != LowForm) fail("resolved LowForm required")
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
    val binding = canonical(JObject(List(
      "witness_version" -> JInt(1), "scope" -> JString("local_fame_combinational_witness_only"),
      "reference_sha256" -> JString(sha256(input.circuit.serialize)),
      "reopened_reference_sha256" -> JString(sha256(state.circuit.serialize)),
      "wired_reference_sha256" -> JString(sha256(witness.circuit.serialize)),
      "model_identity" -> JString(model.model), "primary_domain" -> JString(primaryDomain),
      "label_role" -> JString("applied"), "label_path" -> (row(primaryDomain, "applied") \ "rtl_path"),
      "valid_port" -> JString(valid), "cycle_port" -> JString(cycle),
      "predicate" -> JString("not_reset_and_not_overflow_and_all_domains_consumed_eq_65bit_applied_plus_pending_and_consumed_le_completed_steps"),
      "signals" -> JArray(rows.toList), "new_storage_bits" -> JInt(0),
      "global_target_cut_proven" -> JBool(false), "production_freeze_owners_bound" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "metasim_restore_supported" -> JBool(false))))
    Witness(witness, binding)
  }

  def validateWitness(input: CircuitState, primaryDomain: String, result: Witness): Unit = {
    val expected = generateWitness(input, primaryDomain)
    if (expected.binding != result.binding || !sameState(expected.state, result.state))
      fail("witness differs from independent regeneration")
  }

  def validateWitness(input: CircuitState, manifest: String, plan: String,
      primaryDomain: String, result: Witness): Unit = {
    val expected = generateWitness(input, manifest, plan, primaryDomain)
    if (expected.binding != result.binding || !sameState(expected.state, result.state))
      fail("witness differs from independent regeneration")
  }

  private def generateImpl(input: CircuitState, manifest: String, plan: String, resetPort: String,
      primaryDomain: String, owners: Int, nativeBanks: Boolean): Result = {
    if (input.form != LowForm || owners < 1 || owners > 65535)
      fail("resolved LowForm and 1..65535 external owners required")
    val local = generateWitness(input, manifest, plan, primaryDomain)
    val witness = local.state
    val witnessBinding = parse(local.binding)
    val rows = (witnessBinding \ "signals").asInstanceOf[JArray].arr
    val bit = UIntType(IntWidth(1))
    // Reconstruct the inventory/topology for the added combinational outputs.
    // Every original storage declaration and StateID remains unchanged.
    val inv = SimulatorStateInventory.discover(witness)
    val clockPlan = PhysicalClockCutPlan.discover(witness, inv)
    val native = if (!nativeBanks) None else {
      val dependencies = PhysicalClockCutDependencies.discover(witness, inv, clockPlan)
      val banks = GeneratedStateBankAccess.generatePhysicalCompanion(witness, inv, clockPlan, dependencies)
      val staging = F2NativeCollectorSnapshot.generate(witness, inv, clockPlan, resetPort, cycle,
        owners + 1, dependencies, banks)
      Some(Native(banks, dependencies, inv, clockPlan, witness, staging))
    }
    val collector = native.map(_.staging.collector).getOrElse(
      F2ClockResetCutWiring.generate(witness, inv, clockPlan, resetPort, cycle, owners + 1))
    val composed = native.map(_.staging.state).getOrElse(collector.state)
    val root = composed.circuit.modules.collectFirst { case m: Module if m.name == witness.circuit.main => m }.get
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
    val circuit = composed.circuit.copy(modules = composed.circuit.modules.map {
      case m if m.name == root.name => connected
      case m => m
    })
    val working = firrtl.passes.ToWorkingIR.run(circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    val result = composed.copy(circuit = flowed)
    val boundRows = native.map { n =>
      val inventory = (parse(n.banks.binding) \ "state").asInstanceOf[JArray].arr
      rows.map { row =>
        if ((row \ "storage") != JBool(true)) row
        else {
          val entries = inventory.filter(e => (e \ "hierarchy") == (row \ "rtl_path"))
          if (entries.size != 1) fail("observer/pending role missing or duplicated in native banks")
          val entry = entries.head
          JObject(row.asInstanceOf[JObject].obj ++ List(
            "generated_state_id" -> (entry \ "state_id"), "bank" -> (entry \ "bank"),
            "slot" -> (entry \ "slot"), "address" -> JInt(0),
            "width" -> (entry \ "width"), "depth" -> (entry \ "depth"),
            "storage_hierarchy" -> (entry \ "storage_hierarchy")))
        }
      }
    }.getOrElse(rows)
    val binding = canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString(if (native.nonEmpty)
        "local_fame_witness_native_banks_staged_f2_collector_sibling_only"
        else "local_fame_witness_connected_f2_collector_sibling_only"),
      "reference_sha256" -> JString(sha256(input.circuit.serialize)),
      "reopened_reference_sha256" -> (witnessBinding \ "reopened_reference_sha256"),
      "manifest_sha256" -> JString(sha256(manifest)), "plan_sha256" -> JString(sha256(plan)),
      "wired_reference_sha256" -> JString(sha256(result.circuit.serialize)),
      "collector_binding_sha256" -> JString(sha256(collector.binding)),
      "model_identity" -> (witnessBinding \ "model_identity"), "primary_domain" -> JString(primaryDomain),
      "label_role" -> JString("applied"), "label_path" -> (witnessBinding \ "label_path"),
      "fame_owner_bit" -> JInt(owners), "external_owners" -> JInt(owners),
      "predicate" -> JString("not_reset_and_not_overflow_and_all_domains_consumed_eq_65bit_applied_plus_pending_and_consumed_le_completed_steps"),
      "signals" -> JArray(boundRows.toList), "new_storage_bits" -> JInt(0),
      "global_target_cut_proven" -> JBool(false), "production_freeze_owners_bound" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false)) ++ native.toList.flatMap { n => List(
        "bank_binding_sha256" -> JString(sha256(n.banks.binding)),
        "dependencies_sha256" -> JString(sha256(n.dependencies)),
        "witness_manifest_sha256" -> JString(sha256(n.manifest)),
        "witness_plan_sha256" -> JString(sha256(n.plan)),
        "staging_binding_sha256" -> JString(sha256(n.staging.binding)),
        "functional_entries" -> JInt(n.banks.entries.size),
        "native_latch_restore_supported" -> JBool(false), "metasim_restore_supported" -> JBool(false)) }))
    Result(result, binding, collector, native)
  }

  def validate(input: CircuitState, manifest: String, plan: String, resetPort: String,
      primaryDomain: String, owners: Int, result: Result): Unit = {
    validateResult(generate(input, manifest, plan, resetPort, primaryDomain, owners), result)
  }

  def validateNative(input: CircuitState, manifest: String, plan: String, resetPort: String,
      primaryDomain: String, owners: Int, result: Result): Unit = {
    validateResult(generateNative(input, manifest, plan, resetPort, primaryDomain, owners), result)
  }

  private def validateResult(expected: Result, result: Result): Unit = {
    // RenameMap is mutable compiler metadata with reference equality. Compare
    // its actual mappings, just as we compare emitted IR and live annotations;
    // independently regenerated native bank results are distinct objects.
    def sameCollector(a: F2ClockResetCutWiring.Result, b: F2ClockResetCutWiring.Result): Boolean =
      a.binding == b.binding && sameState(a.state, b.state) &&
        a.reset.binding == b.reset.binding && a.reset.audit == b.reset.audit && sameState(a.reset.state, b.reset.state)
    val sameNative = (result.native, expected.native) match {
      case (None, None) => true
      case (Some(a), Some(b)) =>
        GeneratedStateBankAccess.validateAnnotationBinding(a.banks)
        a.dependencies == b.dependencies && a.manifest == b.manifest && a.plan == b.plan &&
          sameState(a.witness, b.witness) && a.banks.binding == b.banks.binding &&
          a.banks.entries == b.banks.entries && sameState(a.banks.state, b.banks.state) &&
          a.staging.binding == b.staging.binding && sameState(a.staging.state, b.staging.state) &&
          sameCollector(a.staging.collector, b.staging.collector)
      case _ => false
    }
    if (!sameNative || result.binding != expected.binding || !sameState(result.state, expected.state) ||
        !sameCollector(result.collector, expected.collector))
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
