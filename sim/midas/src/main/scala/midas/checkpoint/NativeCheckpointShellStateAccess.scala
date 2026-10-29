// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, LowForm, WDefInstance, WRef, WSubField}
import firrtl.annotations.{IsModule, InstanceTarget, JsonProtocol, ModuleTarget}
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Bank only the local storage of one existing, unique concrete execution.
  * Immediate children remain active, retain their definitions and connections,
  * and acquire no checkpoint ports. This is a local readout experiment, not an
  * all-owner freeze, a transport owner, a restore implementation or an image.
  *
  * The temporary reference replaces child pins with boundary ports. Its banks
  * are grafted back into the original definition; the boundary ports never
  * appear in the resulting interface. Original annotation contracts are
  * retained exactly and checked against the complete original/resulting hierarchy,
  * rather than flattened into this temporary reference. In particular memory
  * initialization and physical constraints retain their actual owners.
  */
object NativeCheckpointShellStateAccess {
  import SemanticStateInventory.{canonical, sha256}
  val prefix = "checkpoint_shell_"
  case class Result(host: CircuitState, target: IsModule, sidebandPrefix: String,
      reference: CircuitState, manifest: String, banks: GeneratedStateBankAccess.Result,
      state: CircuitState, entries: Seq[StateAccessEntry], mapping: String, binding: String)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "Native checkpoint shell state: " + why)
  private def statements(s: Statement): Seq[Statement] = {
    val out = mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  private def json(as: firrtl.AnnotationSeq): String = JsonProtocol.serialize(as)
  // Translation is an owner validator here, never an annotation producer.
  // Distinct validation identities keep two executions of a shared definition
  // separate, including overlap checks on nonlocal memory initialization.
  private def validationClones(live: Seq[(String, DefModule)]): Seq[GeneratedStateBankAnnotations.Clone] =
    live.collect { case (path, m: Module) => path -> m }.zipWithIndex.map { case ((path, m), index) =>
      GeneratedStateBankAnnotations.Clone(m.name, path, s"ShellAnnotationValidation_$index", m)
    }
  private def resolve(c: Circuit): Circuit = {
    val working = firrtl.passes.ToWorkingIR.run(c)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    flowed
  }
  private def ground(t: Type): Boolean = t match {
    case UIntType(IntWidth(w)) if w > 0 => true
    case SIntType(IntWidth(w)) if w > 0 => true
    case ClockType | ResetType | AsyncResetType => true
    case _ => false
  }

  def generate(host: CircuitState, target: IsModule, sidebandPrefix: String = prefix): Result = {
    check(host.form == LowForm, "resolved LowForm required")
    check(sidebandPrefix.matches("checkpoint_shell_[A-Za-z0-9_]*"), "invalid sideband namespace")
    check(target.circuit == host.circuit.main, "foreign selected circuit")
    val (context, owners) = host.annotations.partition(SimulatorBankProbeContext.accepts)
    SimulatorBankProbeContext.snapshot(context, host.circuit.serialize, host.circuit.serialize)
    val storage = host.copy(annotations = owners)
    GeneratedStateBankAnnotations.preflight(storage)
    val modules = host.circuit.modules.map(m => m.name -> m).toMap
    check(modules.size == host.circuit.modules.size, "duplicate module definitions")
    val live = GeneratedExternalOwners.instances(storage, host.circuit.main)
    // Validate all supported target contracts, including declarations not in
    // the shell. preflight alone deliberately does not validate every target.
    val (_, sourceValidation) = GeneratedStateBankAnnotations.translate(storage, validationClones(live))

    val selectedPath = target match {
      case t: ModuleTarget =>
        val paths = live.collect { case (p, m: Module) if m.name == t.module => p }
        check(paths.size == 1, "module selection requires exactly one concrete execution")
        paths.head
      case t: InstanceTarget =>
        check(t.module == host.circuit.main && t.asPath.nonEmpty,
          "instance target must be rooted at the enclosing circuit")
        var path = host.circuit.main
        t.asPath.foreach { case (i, m) =>
          path += "." + i.value
          check(live.exists { case (p, d) => p == path && d.name == m.value }, "stale instance selection " + path)
        }
        path
      case _ => throw new IllegalArgumentException("Native checkpoint shell state: unsupported selection")
    }
    val selected = live.find(_._1 == selectedPath).map(_._2) match {
      case Some(m: Module) => m
      case _ => throw new IllegalArgumentException("Native checkpoint shell state: concrete selected module required")
    }
    val shared = live.count(_._2.name == selected.name) > 1
    val emittedName = if (shared) sidebandPrefix + "selected_" + selected.name else selected.name
    check(!shared || !modules.contains(emittedName), "specialized definition collision " + emittedName)
    val hops = selectedPath.split("\\.").toSeq
    val ancestorPaths = hops.indices.dropRight(1).map(i => hops.take(i + 1).mkString("."))
    val ancestors = ancestorPaths.map { path =>
      val m = live.find(_._1 == path).get._2.asInstanceOf[Module]
      check(live.count(_._2.name == m.name) == 1, "shared ancestor requires specialization")
      m.name -> hops(path.split("\\.").length)
    }.toMap
    (ancestors.keys.toSeq :+ selected.name).foreach { name =>
      val m = modules(name).asInstanceOf[Module]
      check(!(m.ports.map(_.name) ++ statements(m.body).collect { case d: IsDeclaration => d.name })
        .exists(_.startsWith(sidebandPrefix)), "reserved sideband identifier in " + name)
    }
    val children = statements(selected.body).collect { case i: WDefInstance => i }
    check(children.map(_.name).distinct.size == children.size, "duplicate immediate child")
    val childNames = children.map(_.name).toSet
    val boundary = children.flatMap { child =>
      modules.getOrElse(child.module, throw new IllegalArgumentException("missing child definition")).ports.map { p =>
        check(ground(p.tpe), "child boundary requires resolved ground ports: " + child.name + "." + p.name)
        val name = sidebandPrefix + "boundary_" + child.name + "_" + p.name
        ((child.name, p.name), Port(p.info, name, if (p.direction == Input) Output else Input, p.tpe))
      }
    }
    check(boundary.map(_._2.name).distinct.size == boundary.size, "ambiguous flattened child boundary")
    val pins = boundary.toMap
    def extractExpr(e: Expression): Expression = e match {
      case p: WSubField => p.expr match {
        case r: WRef if childNames(r.name) =>
          val port = pins.getOrElse((r.name, p.name), throw new IllegalArgumentException("unknown child pin"))
          WRef(port.name, port.tpe)
        case _ => e.mapExpr(extractExpr)
      }
      case r: WRef if childNames(r.name) =>
        throw new IllegalArgumentException("Native checkpoint shell state: aggregate child reference unsupported")
      case _ => e.mapExpr(extractExpr)
    }
    def extract(s: Statement): Statement = s match {
      case _: WDefInstance => EmptyStmt
      case _ => s.mapStmt(extract).mapExpr(extractExpr)
    }
    val local = selected.copy(ports = selected.ports ++ boundary.map(_._2), body = extract(selected.body))
    // Synthetic boundary clocks cannot create a functional-clock authority.
    // Reject their use by any sequential consumer, following local aliases.
    val all = statements(local.body)
    val drivers = all.collect {
      case n: DefNode => n.name -> n.value
      case c: Connect => c.loc.serialize -> c.expr
    }.groupBy(_._1)
    val boundaryClocks = boundary.map(_._2).filter(p => p.direction == Input && p.tpe == ClockType).map(_.name).toSet
    def touchesBoundary(e: Expression, seen: Set[String] = Set.empty): Boolean = {
      val name = e.serialize
      check(!seen(name), "clock alias cycle")
      if (boundaryClocks(name)) true
      else {
        var found = false
        // A validif's enable is data; following its next-state cone would
        // confuse an ordinary feedback register with a clock-alias cycle.
        e match {
          case ValidIf(_, value, ClockType) => found = touchesBoundary(value, seen + name)
          // Follow casts through UInt<1> as well: asClock(asUInt(child.O))
          // must not turn a child-produced clock into a new authority.
          case _ => e.foreachExpr(x => if (touchesBoundary(x, seen + name)) found = true)
        }
        if (found) true else drivers.getOrElse(name, Nil).exists { case (_, v) => touchesBoundary(v, seen + name) }
      }
    }
    all.foreach {
      case r: DefRegister => check(!touchesBoundary(r.clock), "child-produced local register clock unsupported")
      case p: Print => check(!touchesBoundary(p.clk), "child-produced effect clock unsupported")
      case p: Stop => check(!touchesBoundary(p.clk), "child-produced effect clock unsupported")
      case p: Verification => check(!touchesBoundary(p.clk), "child-produced effect clock unsupported")
      case m: DefMemory => (m.readers ++ m.writers ++ m.readwriters).foreach { p =>
        drivers.getOrElse(m.name + "." + p + ".clk", Nil).foreach { case (_, e) =>
          check(!touchesBoundary(e), "child-produced local memory clock unsupported")
        }
      }
      case _ => ()
    }
    val policies = owners.filter {
      case firrtl.annotations.MemorySynthInit | firrtl.annotations.MemoryNoSynthInit |
           firrtl.transforms.NoCircuitDedupAnnotation | firrtl.passes.memlib.InferReadWriteAnnotation |
           firrtl.passes.memlib.DefaultReadFirstAnnotation | firrtl.passes.memlib.DefaultWriteFirstAnnotation |
           firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation => true
      case _ => false
    }
    val reference = CircuitState(resolve(host.circuit.copy(main = selected.name, modules = Seq(local))), LowForm, policies)
    val manifest = SimulatorStateInventory.discover(reference)
    val ordinaryBanks = GeneratedStateBankAccess.generate(reference, manifest, separateAccessClock = true)
    val banks = if (NativeSynchronousMemoryReadout.needed(reference))
      NativeSynchronousMemoryReadout.generate(reference, ordinaryBanks) else ordinaryBanks
    GeneratedStateBankAccess.validateAnnotationBinding(banks)
    check(banks.state.circuit.modules.size == 1, "local extraction unexpectedly generated hierarchy")
    val generated = banks.state.circuit.modules.head.asInstanceOf[Module]
    val originalNames = local.ports.map(_.name).toSet
    val sidebands = generated.ports.filterNot(p => originalNames(p.name)).map(p => p.copy(name = sidebandPrefix + p.name))
    check(sidebands.nonEmpty, "missing local access ports")
    val reversePins = boundary.map { case ((instance, pin), p) => p.name -> (instance, pin, p.tpe) }.toMap
    def graftExpr(e: Expression): Expression = e match {
      case r: WRef if reversePins.contains(r.name) =>
        val (instance, pin, tpe) = reversePins(r.name)
        WSubField(WRef(instance), pin, tpe)
      case r: WRef if r.name.startsWith("ckpt_") => r.copy(name = sidebandPrefix + r.name)
      case _ => e.mapExpr(graftExpr)
    }
    def graft(s: Statement): Statement = {
      val rewritten = s.mapStmt(graft).mapExpr(graftExpr)
      rewritten match {
        case r: DefRegister if r.name.startsWith("ckpt_") => r.copy(name = sidebandPrefix + r.name)
        case n: DefNode if n.name.startsWith("ckpt_") => n.copy(name = sidebandPrefix + n.name)
        case w: DefWire if w.name.startsWith("ckpt_") => w.copy(name = sidebandPrefix + w.name)
        case _ => rewritten
      }
    }
    val bankedLocal = selected.copy(name = emittedName, ports = selected.ports ++ sidebands,
      body = Block(children ++ Seq(graft(generated.body))))
    val rewritten = host.circuit.modules.map {
      case m if m.name == selected.name && !shared => bankedLocal
      case m: Module if ancestors.contains(m.name) =>
        val child = ancestors(m.name)
        val connections = sidebands.map { p =>
          val pin = WSubField(WRef(child), p.name, p.tpe)
          val exposed = WRef(p.name, p.tpe)
          if (p.direction == Input) Connect(NoInfo, pin, exposed) else Connect(NoInfo, exposed, pin)
        }
        def specialize(s: Statement): Statement = s match {
          case i: WDefInstance if shared && i.name == child && i.module == selected.name => i.copy(module = emittedName)
          case x => x.mapStmt(specialize)
        }
        m.copy(ports = m.ports ++ sidebands, body = Block(Seq(specialize(m.body)) ++ connections))
      case m => m
    }
    val bare = CircuitState(resolve(host.circuit.copy(modules = rewritten ++ (if (shared) Seq(bankedLocal) else Nil))), LowForm)
    val after = GeneratedExternalOwners.instances(bare, host.circuit.main)
    check(live.map { case (p, m) => p -> (if (shared && p == selectedPath) emittedName else m.name) }.toSet ==
      after.map { case (p, m) => p -> m.name }.toSet,
      "original concrete execution set changed")
    val finalModules = bare.circuit.modules.map(m => m.name -> m).toMap
    host.circuit.modules.filterNot(m => m.name == selected.name || ancestors.contains(m.name)).foreach { m =>
      check(finalModules(m.name) == m, "changed child or unrelated original definition " + m.name)
    }
    // Unique definitions retain the exact original vector. A shared selected
    // execution needs concrete-path translation so correlated nonlocal owners
    // follow that copy and never broaden onto its unselected siblings.
    val clones = live.collect { case (path, m: Module) =>
      GeneratedStateBankAnnotations.Clone(m.name, path, after.find(_._1 == path).get._2.name, m)
    }
    val translated: firrtl.AnnotationSeq = if (shared)
      GeneratedStateBankAnnotations.translate(storage, clones, preserveConcreteStorage = true)._1 ++ context
      else host.annotations
    val annotations = translated.filterNot(_.isInstanceOf[firrtl.transforms.CombinationalPath])
    val state = bare.copy(annotations = annotations)
    val (_, finalValidation) = GeneratedStateBankAnnotations.translate(state.copy(
      annotations = annotations.filterNot(SimulatorBankProbeContext.accepts)), validationClones(after))
    val snapshot = SimulatorBankProbeContext.snapshot(context, host.circuit.serialize, state.circuit.serialize)
    def actualPath(path: String): String = {
      check(path.startsWith(selected.name + "."), "unrooted bank storage mapping")
      selectedPath + path.substring(selected.name.length)
    }
    val rows = (parse(manifest) \ "state").asInstanceOf[JArray].arr
    val bankRows = (parse(banks.binding) \ "state").asInstanceOf[JArray].arr
    val mappings = banks.entries.map { e =>
      val row = rows.find(r => (r \ "state_id") == JString(e.stateID)).get
      val JString(hierarchy) = row \ "hierarchy"
      val JString(kind) = row \ "kind"
      val actual = actualPath(hierarchy)
      val id = SimulatorStateInventory.stateID(actual, kind)
      val storageRow = bankRows.find(r => (r \ "state_id") == JString(e.stateID)).get
      val JString(storageHierarchy) = storageRow \ "storage_hierarchy"
      val mapping = JObject(List("state_id" -> JString(id), "local_state_id" -> JString(e.stateID),
        "hierarchy" -> JString(actual), "storage_hierarchy" -> JString(actualPath(storageHierarchy)),
        "kind" -> JString(kind), "width" -> (row \ "width"), "depth" -> (row \ "depth"),
        "bank" -> JInt(e.bank), "slot" -> JInt(e.slot), "transfer_width" -> JInt(e.width),
        "transfer_depth" -> JInt(e.depth)))
      (e.copy(stateID = id), mapping)
    }
    val mapping = canonical(JObject(List("mapping_version" -> JInt(1),
      "selected_execution" -> JString(selectedPath), "state" -> JArray(mappings.map(_._2).toList))))
    val binding = canonical(JObject(List("shell_access_version" -> JInt(1),
      "scope" -> JString(if (shared) "existing_concrete_execution_local_state_readout_only" else
        "existing_unique_execution_local_state_readout_only"),
      "target" -> JString(target.serialize), "selected_execution" -> JString(selectedPath),
      "original_definition" -> JString(selected.name), "emitted_definition" -> JString(emittedName),
      "sideband_prefix" -> JString(sidebandPrefix), "host_sha256" -> JString(sha256(host.circuit.serialize)),
      "host_annotations_sha256" -> JString(sha256(json(host.annotations))),
      "reference_sha256" -> JString(sha256(reference.circuit.serialize)),
      "manifest_sha256" -> JString(sha256(manifest)), "banks_binding_sha256" -> JString(sha256(banks.binding)),
      "instrumented_sha256" -> JString(sha256(state.circuit.serialize)),
      "instrumented_annotations_sha256" -> JString(sha256(json(state.annotations))),
      "mapping_sha256" -> JString(sha256(mapping)),
      "annotation_policy" -> JObject(List("policy_version" -> JInt(1),
        "preservation" -> JString(if (shared) "concrete_execution_translation" else
          "exact_source_annotation_vector_except_invalidated_combinational_path_caches"),
        "translation_results_applied" -> JBool(shared),
        "invalidated_path_caches" -> JArray(host.annotations.collect {
          case a: firrtl.transforms.CombinationalPath => JString(a.serialize)
        }.toList))),
      "annotation_validation" -> JObject(List("validation_only" -> JBool(true),
        "source_contracts" -> sourceValidation, "grafted_contracts" -> finalValidation)),
      "context" -> parse(snapshot), "local_entries" -> JInt(mappings.size),
      "child_executions_frozen" -> JBool(false), "transport_state_supported" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    Result(host, target, sidebandPrefix, reference, manifest, banks, state, mappings.map(_._1), mapping, binding)
  }

  /** Reconstruct all equations, annotation contracts and descriptor identities
    * from the retained original. A caller cannot excuse alterations by hashing
    * a modified result or replacing a sidecar digest.
    */
  def validate(result: Result): Unit = {
    val expected = generate(result.host, result.target, result.sidebandPrefix)
    def same(a: CircuitState, b: CircuitState): Boolean = a.form == b.form && a.renames == b.renames &&
      a.circuit == b.circuit && a.annotations.toVector == b.annotations.toVector
    check(same(result.state, expected.state) && same(result.reference, expected.reference) &&
      same(result.banks.state, expected.banks.state) && result.banks.entries == expected.banks.entries &&
      result.banks.binding == expected.banks.binding && result.manifest == expected.manifest &&
      result.entries == expected.entries && result.mapping == expected.mapping && result.binding == expected.binding,
      "changed source extraction, local banks, graft, annotations or identity")
  }
}
