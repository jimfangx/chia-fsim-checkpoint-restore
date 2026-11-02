// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, WDefInstance}
import firrtl.annotations._
import firrtl.ir._
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Immutable construction-site model/domain identities, NOT live hierarchy
  * names. Signal annotations alone follow native compiler renames. Keeping the
  * expected role set separately detects deletion, replication and aliasing.
  */
case class FAMEClockProgressModelAnnotation(model: String, domains: Seq[String]) extends NoTargetAnnotation
case class FAMEClockProgressSignalAnnotation(model: String, domain: String, role: String,
    target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget): FAMEClockProgressSignalAnnotation = copy(target = n)
}

/** Read-only role resolution. This does not install a cut controller, own a
  * physical CE latch, or turn any counter into global target-cycle authority.
  */
object FAMEClockProgressBindings {
  import SemanticStateInventory.{canonical, sha256}
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException("FAME clock progress binding unsupported: " + why)
  private val common = Seq("host_clock", "host_reset", "finishing", "completed_steps", "overflow")
  private val perClock = Seq("consume", "launch", "pending", "consumed", "applied", "pending_flag", "gate_ce", "gate_output")
  private val registers = Set("completed_steps", "overflow", "consumed", "applied", "pending_flag")
  private def models(state: CircuitState) = state.annotations.collect { case a: FAMEClockProgressModelAnnotation => a }
  private def signals(state: CircuitState) = state.annotations.collect { case a: FAMEClockProgressSignalAnnotation => a }
  private def key(a: FAMEClockProgressSignalAnnotation) = (a.model, a.domain, a.role)

  /** InlineInstances does not rename a module-local instance.field target
    * when the enclosing module is removed. Anchor every execution at the real
    * circuit root, and express gate pins as nonlocal leaf-port targets before
    * any native inlining or platform gate specialization occurs.
    */
  def rootAtConstruction(state: CircuitState): CircuitState = {
    val ss = signals(state)
    if (ss.isEmpty) return state
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    val paths = mutable.Map.empty[String, Vector[Seq[(TargetToken.Instance, TargetToken.OfModule)]]]
    def instances(s: Statement): Seq[WDefInstance] = {
      val out = mutable.ArrayBuffer.empty[WDefInstance]
      def visit(x: Statement): Unit = { x match { case i: WDefInstance => out += i; case _ => () }; x.foreachStmt(visit) }
      visit(s); out.toSeq
    }
    def walk(name: String, path: Seq[(TargetToken.Instance, TargetToken.OfModule)], seen: Set[String]): Unit = {
      if (seen(name)) fail("recursive construction hierarchy")
      paths(name) = paths.getOrElse(name, Vector.empty) :+ path
      modules.get(name).foreach {
        case m: Module => instances(m.body).foreach(i =>
          walk(i.module, path :+ (TargetToken.Instance(i.name) -> TargetToken.OfModule(i.module)), seen + name))
        case _ => ()
      }
    }
    walk(state.circuit.main, Nil, Set.empty)
    state.copy(annotations = state.annotations.map {
      case a: FAMEClockProgressSignalAnnotation =>
        val t = a.target; val parents = paths.getOrElse(t.module, Vector.empty)
        if (t.circuit != state.circuit.main || t.path.nonEmpty || parents.size != 1)
          fail("construction role requires one concrete model execution")
        val rooted = t.copy(module = state.circuit.main, path = parents.head)
        val target = t.component match {
          case Nil => rooted
          case Seq(TargetToken.Field(pin)) if Set("gate_ce", "gate_output")(a.role) =>
            val module = modules(t.module).asInstanceOf[Module]
            val gate = instances(module.body).find(_.name == t.ref).getOrElse(fail("missing construction gate"))
            rooted.copy(path = rooted.path :+ (TargetToken.Instance(gate.name) -> TargetToken.OfModule(gate.module)),
              ref = pin, component = Nil)
          case _ => fail("invalid construction role target")
        }
        a.copy(target = target)
      case a => a
    })
  }

  def validatePreserved(before: CircuitState, after: CircuitState): Unit = {
    if (models(before).sortBy(_.model) != models(after).sortBy(_.model) ||
        signals(before).map(key).sorted != signals(after).map(key).sorted)
      fail("construction-site identities lost or duplicated by compiler")
  }

  /** Validate against the actual IR, including all hierarchy hops and the
    * original gate's I/CE/O connectivity. No name-prefix role inference.
    */
  def resolve(input: CircuitState): Seq[JValue] = resolveImpl(input, banked = false)

  /** A bank companion holds each observer and original pending register using
    * its local ckpt_freeze port. Check that exact generated wrapper before
    * checking the original reset owner inside it. This grants no physical cut
    * authority and is deliberately separate from the production role checker.
    */
  private[checkpoint] def resolveBanked(input: CircuitState): Seq[JValue] =
    resolveImpl(input, banked = true)

  /** Source-bound diagnostic projection. The original role contract remains strict;
    * only the exact generated bank and physical mask equations may differ. */
  private[checkpoint] def resolveFrozen(source: CircuitState, input: CircuitState): Seq[JValue] =
    resolveImpl(input, banked = false, source = Some(source))

  private def resolveImpl(input: CircuitState, banked: Boolean,
      source: Option[CircuitState] = None): Seq[JValue] = {
    val sourceRows = source.map { original =>
      val rows = resolve(original)
      validatePreserved(original, input)
      rows
    }
    if (models(input).isEmpty && signals(input).isEmpty) return Nil
    val state = PhysicalClockCutPlan.explicitConnections(input)
    val ms = models(state); val ss = signals(state)
    if (ms.isEmpty && ss.isEmpty) return Nil
    if (ms.isEmpty || ms.map(_.model).distinct.size != ms.size)
      fail("missing or duplicate model identities")
    val expected = ms.flatMap { m =>
      if (m.model.isEmpty || m.domains.isEmpty || m.domains.distinct.size != m.domains.size ||
          m.domains.exists(!_.matches("[A-Za-z_][A-Za-z0-9_$]*"))) fail("invalid model/domain identity")
      common.map(r => (m.model, "", r)) ++ m.domains.flatMap(d => perClock.map(r => (m.model, d, r)))
    }
    if (ss.map(key).sorted != expected.sorted) fail("missing, duplicated or unknown signal role")
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    val declarations = modules.values.collect { case m: Module =>
      val out = mutable.ArrayBuffer.empty[Statement]
      def visit(s: Statement): Unit = { out += s; s.foreachStmt(visit) }
      visit(m.body); m.name -> out.toSeq
    }.toMap
    val paths = mutable.Map.empty[String, Vector[Seq[String]]]
    def walk(name: String, path: Seq[String], seen: Set[String]): Unit = {
      if (seen(name) || !modules.contains(name)) fail("recursive or missing hierarchy")
      paths(name) = paths.getOrElse(name, Vector.empty) :+ path
      declarations.getOrElse(name, Nil).collect { case i: WDefInstance => i }.foreach { i =>
        walk(i.module, path :+ i.name, seen + name)
      }
    }
    walk(state.circuit.main, Seq(state.circuit.main), Set.empty)
    case class Location(module: Module, path: Seq[String], target: ReferenceTarget,
        declaration: Option[Statement], tpe: Type) {
      def absolute: String = (path :+ target.ref).mkString(".") + target.component.map {
        case TargetToken.Field(f) => "." + f
        case _ => fail("indexed role target")
      }.mkString
    }
    val locations = ss.map { a =>
      // Analyze a nonlocal gate leaf port at its parent instance, preserving
      // the original compiler-renamed target in the emitted role map.
      val original = a.target
      val t = if (Set("gate_ce", "gate_output")(a.role) && original.path.nonEmpty && original.component.isEmpty) {
        val (instance, _) = original.path.last
        original.copy(path = original.path.dropRight(1), ref = instance.value, component = Seq(TargetToken.Field(original.ref)))
      } else original
      val parents = paths.getOrElse(t.module, Vector.empty)
      if (t.circuit != state.circuit.main || parents.size != 1)
        fail("foreign, unreachable or replicated role target: " + t.serialize + " executions=" + parents.size)
      var owner = t.module; var path = parents.head
      t.path.foreach { case (i, m) =>
        if (!declarations.getOrElse(owner, Nil).exists {
          case d: WDefInstance => d.name == i.value && d.module == m.value
          case _ => false
        }) fail("role instance path mismatch")
        path :+= i.value; owner = m.value
      }
      val module = modules(owner) match { case m: Module => m; case _ => fail("opaque role owner") }
      if (paths.getOrElse(module.name, Vector.empty).size != 1) fail("replicated model role execution")
      val declaration = declarations(owner).find {
        case d: IsDeclaration => d.name == t.ref
        case _ => false
      }
      val tpe = t.component match {
        case Nil => declaration match {
          case Some(d: DefRegister) => d.tpe
          case Some(d: DefWire) => d.tpe
          case Some(d: DefNode) => d.value.tpe
          case _ => module.ports.find(_.name == t.ref).map(_.tpe).getOrElse(fail("missing signal declaration"))
        }
        case Seq(TargetToken.Field(pin)) if a.role == "gate_ce" || a.role == "gate_output" =>
          val instance = declaration.collect { case i: WDefInstance => i }.getOrElse(fail("missing gate instance"))
          if (original.path.nonEmpty && original.component.isEmpty && original.path.last._2.value != instance.module)
            fail("gate leaf module path mismatch")
          val gate = modules(instance.module) match {
            case e: ExtModule if e.defname == "BUFGCE" => PhysicalClockGateContract.validate(state, e); e
            case _ => fail("role requires the original native BUFGCE")
          }
          if (pin != (if (a.role == "gate_ce") "CE" else "O")) fail("wrong native gate pin role")
          gate.ports.find(_.name == pin).map(_.tpe).getOrElse(fail("missing native gate pin"))
        case _ => fail("reshaped signal role target")
      }
      val width = if (Set("completed_steps", "consumed", "applied")(a.role)) 64 else 1
      val expectedType = if (Set("host_clock", "gate_output")(a.role)) ClockType else UIntType(IntWidth(width))
      if (tpe != expectedType || (registers(a.role) && !declaration.exists(_.isInstanceOf[DefRegister])))
        fail("signal role kind/width mismatch: " + a.role)
      a -> Location(module, path, t, declaration, tpe)
    }.toMap
    if (locations.values.map(_.absolute).toSeq.distinct.size != ss.size) fail("aliased signal roles")

    if (source.isEmpty) ms.foreach { model =>
      val owners = ss.filter(_.model == model.model)
      val ownerLocations = owners.map(a => locations(a).path)
      if (ownerLocations.distinct.size != 1) fail("model roles split across executions")
      val module = locations(owners.head).module
      val stmts = declarations(module.name)
      // Resolve actual aliases after LowerTypes, CSE and VerilogRename. Reject
      // multiple writers rather than picking the last connection in a map.
      val drivers = stmts.collect { case c: Connect => c.loc.serialize -> c.expr }
      val finishingName = owners.find(a => a.domain.isEmpty && a.role == "finishing").get.target.ref
      def normalize(e: Expression, seen: Set[String] = Set.empty): String = {
        val name = e.serialize
        // The exact preserved completion wire is a role boundary. Expanding
        // its entire CPU/channel dependency cone would duplicate functional
        // logic without strengthening the gate-to-completion association.
        if (name == finishingName) return name
        val node = stmts.collectFirst { case n: DefNode if n.name == name => n.value }
        val isWire = stmts.exists { case w: DefWire => w.name == name; case _ => false }
        val candidates = if (isWire) drivers.filter(_._1 == name).map(_._2) else node.toSeq
        if (isWire && candidates.size != 1) fail("missing or multiple observation writers")
        if (candidates.nonEmpty) {
          if (seen(name)) fail("cyclic observation alias")
          normalize(candidates.head, seen + name)
        } else e match {
          case d: DoPrim => d.op.serialize + "(" + d.args.map(x => normalize(x, seen)).mkString(",") +
            ";" + d.consts.mkString(",") + ")"
          case m: Mux => "mux(" + Seq(m.cond, m.tval, m.fval).map(x => normalize(x, seen)).mkString(",") + ")"
          case v: ValidIf => "validif(" + Seq(v.cond, v.value).map(x => normalize(x, seen)).mkString(",") + ")"
          case _ => name
        }
      }
      def annotation(domain: String, role: String) = owners.find(a => a.domain == domain && a.role == role).get
      def expression(a: FAMEClockProgressSignalAnnotation): Expression = firrtl.WRef(a.target.ref, locations(a).tpe)
      val host = annotation("", "host_clock"); val reset = annotation("", "host_reset")
      owners.filter(a => registers(a.role)).foreach { a =>
        val reg = locations(a).declaration.get.asInstanceOf[DefRegister]
        // RemoveReset moves a synchronous reset into the register's data mux
        // before Verilog emission. Check that exact ownership rather than
        // accepting any reset-free register of the right width.
        val driven = drivers.filter(_._1 == reg.name).map(_._2)
        val next = if (!banked) driven else driven match {
          case Seq(m: Mux) if module.ports.exists(p => p.name == "ckpt_freeze" &&
              p.direction == Input && p.tpe == UIntType(IntWidth(1))) &&
              m.cond.serialize == "ckpt_freeze" && m.tval.serialize == reg.name =>
            // Banking preserves the original register reset expression even
            // when RemoveReset has already lowered it to the literal zero.
            // FIRRTL may retain that exact inert reset mux in LowForm.
            val functional = m.fval match {
              case inner: Mux if inner.cond == UIntLiteral(0, IntWidth(1)) &&
                  inner.tval.serialize == reg.name => inner.fval
              case other => other
            }
            Seq(functional)
          case _ => fail("missing exact bank freeze/hold wrapper: " + reg.name)
        }
        val resetMux = next match {
          case Seq(m: Mux) => normalize(m.cond) == normalize(expression(reset)) && (m.tval match {
            case UIntLiteral(value, _) => value == 0
            case _ => false
          })
          case _ => false
        }
        val originalReset = normalize(reg.reset) == normalize(expression(reset)) && (reg.init match {
          case UIntLiteral(value, _) => value == 0
          case _ => false
        })
        val loweredReset = (reg.reset match { case UIntLiteral(value, _) => value == 0; case _ => false }) &&
          reg.init.serialize == reg.name && resetMux
        if (normalize(reg.clock) != normalize(expression(host)) || !(if (banked) loweredReset else originalReset || loweredReset))
          fail("observer/pending register clock or reset owner mismatch: " + reg.name +
            " clock=" + reg.clock.serialize + " reset=" + reg.reset.serialize + " init=" + reg.init.serialize +
            " next=" + next.map(_.serialize).mkString(","))
      }
      model.domains.foreach { domain =>
        val ce = annotation(domain, "gate_ce"); val output = annotation(domain, "gate_output")
        val gateName = locations(ce).target.ref
        if (gateName != locations(output).target.ref) fail("native CE/output refer to different gates")
        def pin(name: String): Expression = {
          val values = drivers.filter(_._1 == gateName + "." + name).map(_._2)
          if (values.size != 1) fail("missing or multiple native gate pin writers")
          values.head
        }
        if (normalize(pin("CE")) != normalize(expression(annotation(domain, "launch"))) ||
            normalize(pin("I")) != normalize(expression(host)) ||
            normalize(expression(annotation(domain, "pending"))) != normalize(expression(annotation(domain, "pending_flag"))))
          fail("native gate or pending role connectivity mismatch")
      }
    }
    val rows = ss.sortBy(key).map { a =>
      val location = locations(a)
      JObject(List("model_identity" -> JString(a.model), "domain_identity" -> JString(a.domain),
        "role" -> JString(a.role), "target" -> JString(a.target.serialize), "rtl_path" -> JString(location.absolute),
        "type" -> JString(location.tpe.serialize), "storage" -> JBool(registers(a.role))))
    }
    source.foreach(original => FAMEClockProgressProjection.validate(original, input, sourceRows.get, rows))
    rows
  }

  /** Bind native-bank roles to this companion emitter's actual prepared IR.
    * StateIDs and selectors come from the independently validated input bank,
    * while RTL paths follow the emitter's real renames. No inventory of added
    * transport registers is confused with the original functional inventory.
    */
  private[checkpoint] def emitNativeBanked(banks: GeneratedStateBankAccess.Result,
      actual: CircuitState, rtl: String): Seq[GoldenGateOutputFileAnnotation] = {
    GeneratedStateBankAccess.validateAnnotationBinding(banks)
    validatePreserved(banks.state, actual)
    val before = resolveBanked(banks.state)
    val after = resolveBanked(actual)
    if (before.isEmpty) return Nil
    val binding = parse(banks.binding)
    if ((binding \ "scope") != JString("native_functional_readout_companion_only"))
      fail("native companion bank binding required")
    val inventory = (binding \ "state").asInstanceOf[JArray].arr
    val byPath = inventory.groupBy(r => (r \ "storage_hierarchy").values.toString)
    def identity(r: JValue) = Seq("model_identity", "domain_identity", "role").map(k => r \ k)
    val originals = before.map(r => identity(r) -> r).toMap
    val bound = after.map { row =>
      val original = originals(identity(row))
      if ((row \ "storage") != JBool(true)) row
      else {
        val matches = byPath.getOrElse((original \ "rtl_path").values.toString, Nil)
        if (matches.size != 1) fail("bank role missing or duplicated in functional inventory")
        val entry = matches.head
        val width = if (Set("completed_steps", "consumed", "applied")((row \ "role").values.toString)) 64 else 1
        if ((entry \ "depth") != JInt(1) || (entry \ "width") != JInt(width))
          fail("bank role inventory storage shape mismatch")
        JObject(row.asInstanceOf[JObject].obj ++ List(
          "generated_state_id" -> (entry \ "state_id"), "bank" -> (entry \ "bank"),
          "slot" -> (entry \ "slot"), "input_storage_hierarchy" -> (entry \ "storage_hierarchy")))
      }
    }
    Seq(GoldenGateOutputFileAnnotation(canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString("native-banked-fame-clock-progress-role-map-only"),
      "global_target_cut_proven" -> JBool(false), "capture_supported" -> JBool(false),
      "deployable_capture_image" -> JBool(false),
      "bank_binding_sha256" -> JString(sha256(banks.binding)),
      "bank_input_reference_sha256" -> (binding \ "reference_sha256"),
      "bank_instrumented_sha256" -> JString(sha256(banks.state.circuit.serialize)),
      "emitted_verilog_sha256" -> JString(sha256(rtl)),
      "emission_reference_sha256" -> JString(sha256(actual.circuit.serialize)),
      "signals" -> JArray(bound.toList)))), ".native-functional-fame-clock-progress.json"))
  }

  def emit(state: CircuitState, rtl: String, manifest: String): Seq[GoldenGateOutputFileAnnotation] = {
    val rows = resolve(state)
    if (rows.isEmpty) return Nil
    val inventory = (parse(manifest) \ "state").asInstanceOf[JArray].arr
    val storage = inventory.map(e => (e \ "hierarchy").values.toString -> e).toMap
    val bound = rows.map { row =>
      if ((row \ "storage") != JBool(true)) row
      else {
        val entry = storage.getOrElse((row \ "rtl_path").values.toString, fail("role missing from complete inventory"))
        if ((entry \ "kind") != JString("register") || (entry \ "depth") != JInt(1))
          fail("role inventory storage kind mismatch")
        JObject(row.asInstanceOf[JObject].obj :+ ("generated_state_id" -> (entry \ "state_id")))
      }
    }
    Seq(GoldenGateOutputFileAnnotation(canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString("fame-clock-progress-role-map-only"),
      "global_target_cut_proven" -> JBool(false), "capture_supported" -> JBool(false),
      "emitted_verilog_sha256" -> JString(sha256(rtl)),
      "emission_reference_sha256" -> JString(sha256(state.circuit.serialize)),
      "generated_manifest_sha256" -> JString(sha256(manifest)), "signals" -> JArray(bound.toList)))),
      ".fame-clock-progress.json"))
  }
}
