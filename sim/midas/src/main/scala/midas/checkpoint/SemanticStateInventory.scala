// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import scala.collection.mutable

import firrtl.{CircuitState, WDefInstance, CDefMemory, CDefMPort}
import firrtl.ir._
import firrtl.annotations.{NoTargetAnnotation, ReferenceTarget, InstanceTarget, SingleTargetAnnotation}
import firrtl.transforms.{DontTouchAnnotation, BlackBoxInlineAnno}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.{compact, parse, render}
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler

/** Explicit instance selection excludes harness/bridge state without name heuristics. */
case class SemanticCheckpointRootAnnotation(path: String) extends NoTargetAnnotation with Unserializable
object SemanticCheckpointRootAnnotation extends HasShellOptions {
  val options = Seq(new ShellOption[String](
    longOption = "semantic-checkpoint-root",
    toAnnotationSeq = (path: String) => Seq(SemanticCheckpointRootAnnotation(path)),
    helpText = "Inventory this pre-FAME instance (circuit top followed by dot-separated instance names).",
    helpValueName = Some("<Top.dut>"),
  ))
}

/** Select a target whose environment bridges must first become boundary ports.
  * This remains a pre-FAME inventory; bridge implementation and token state are
  * outside the selected hierarchy. It is explicitly separate from the earlier
  * resolved-target inventory, whose reference and StateIDs remain unchanged.
  */
case class SemanticBridgeCheckpointRootAnnotation(path: String) extends NoTargetAnnotation with Unserializable
object SemanticBridgeCheckpointRootAnnotation extends HasShellOptions {
  val options = Seq(new ShellOption[String](
    longOption = "semantic-bridge-checkpoint-root",
    toAnnotationSeq = (path: String) => Seq(SemanticBridgeCheckpointRootAnnotation(path)),
    helpText = "Inventory this pre-FAME instance after extracting environment bridges to boundary ports.",
    helpValueName = Some("<Top.dut>"),
  ))
}

/** Discovery only: event phases and a coherent multi-clock cut must be bound
  * by a separate capture backend before this inventory can authorize restore.
  */
case object SemanticMultiClockInventoryAnnotation extends NoTargetAnnotation with Unserializable with HasShellOptions {
  val options = Seq(new ShellOption[Unit](
    longOption = "semantic-multiclock-inventory",
    toAnnotationSeq = (_: Unit) => Seq(SemanticMultiClockInventoryAnnotation),
    helpText = "Emit explicit pre-FAME clock ownership; does not enable multi-clock capture or restore.",
  ))
}

/** Compiler-maintained clean-RTL location, separate from original StateID. */
case class SemanticCleanStorageAnnotation(stateID: String, target: ReferenceTarget, suffix: String = "",
                                          instanceKeys: Seq[String] = Nil)
    extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget): SemanticCleanStorageAnnotation = copy(target = n)
}

case class SemanticCleanInstanceAnnotation(key: String, target: InstanceTarget)
    extends SingleTargetAnnotation[InstanceTarget] {
  def duplicate(n: InstanceTarget): SemanticCleanInstanceAnnotation = copy(target = n)
}

/** FIRRTL's emitter runs VerilogPrep/VerilogRename internally but returns the
  * input CircuitState. Observe that exact internal state, never reconstruct a
  * keyword rename from the emitted spelling or from the earlier result state.
  */
class SemanticCleanEmitter extends firrtl.SystemVerilogEmitter {
  var preparedState: Option[CircuitState] = None
  override protected def runTransforms(state: CircuitState): CircuitState = {
    val prepared = super.runTransforms(state)
    preparedState = Some(prepared)
    prepared
  }
}

/** Run at the resolved bridge-extraction boundary, before target instrumentation
  * and every FAME transform. No circuit changes or mapping guesses are made.
  */
class BridgeSemanticStateInventory extends firrtl.Transform {
  def inputForm = firrtl.LowForm
  def outputForm = firrtl.LowForm
  def execute(state: CircuitState): CircuitState = {
    val roots = state.annotations.collect { case SemanticBridgeCheckpointRootAnnotation(path) => path }
    if (roots.isEmpty) state
    else {
      require(roots.size == 1, "Semantic checkpoint unsupported: exactly one bridge-extracted root is required")
      require(!state.annotations.exists {
        case _: SemanticCheckpointRootAnnotation | _: SemanticBankedAccessAnnotation => true
        case _ => false
      }, "Semantic checkpoint unsupported: bridge-extracted inventory cannot mix root phases or banked bindings")
      val selected = state.copy(form = firrtl.LowForm, annotations =
        state.annotations.filterNot(_.isInstanceOf[SemanticBridgeCheckpointRootAnnotation]) :+
          SemanticCheckpointRootAnnotation(roots.head))
      val inventoried = SemanticStateInventory(selected, allowOutputClockAliases = true)
      // The selection is consumed. Do not run this inventory again at a later
      // compiler boundary, or leave a synthetic early-root annotation behind.
      inventoried.copy(annotations = inventoried.annotations.filterNot {
        case _: SemanticCheckpointRootAnnotation | SemanticMultiClockInventoryAnnotation => true
        case _ => false
      })
    }
  }
}

/** Read-only discovery at the resolved LowForm boundary, before MidasTransforms.
  * No ports or instrumentation are added. Unsupported state aborts compilation.
  * IDs describe the lowered target hierarchy, never the transformed simulator's names.
  */
object SemanticStateInventory {
  private def fail(message: String): Nothing =
    throw new IllegalArgumentException(s"Semantic checkpoint unsupported: $message")

  def sha256(text: String): String =
    MessageDigest.getInstance("SHA-256").digest(text.getBytes(UTF_8)).map(b => f"${b & 0xff}%02x").mkString

  // Matches the checkpoint codec's canonical ASCII JSON for these ASCII-only fields.
  def canonical(value: JValue): String = {
    def sorted(v: JValue): JValue = v match {
      case JObject(fields) => JObject(fields.sortBy(_._1).map { case (k, x) => k -> sorted(x) })
      case JArray(xs) => JArray(xs.map(sorted))
      case other => other
    }
    compact(render(sorted(value)))
  }

  def stateID(hierarchy: String, kind: String): String = sha256(canonical(JObject(List(
    "hierarchy" -> JString(hierarchy), "kind" -> JString(kind), "namespace" -> JString("pre-fame-state-v1"),
  ))))

  private def statements(body: Statement): Seq[Statement] = {
    val result = mutable.ArrayBuffer.empty[Statement]
    def visit(s: Statement): Unit = { result += s; s.foreachStmt(visit) }
    visit(body)
    result.toSeq
  }

  def discover(state: CircuitState, root: String, allowOutputClockAliases: Boolean = false,
               allowMultipleClocks: Boolean = false): String = {
    if (state.form != firrtl.LowForm) fail("discovery requires resolved LowForm")
    val circuit = state.circuit
    val modules = circuit.modules.map(m => m.name -> m).toMap
    val path = root.split("\\.", -1).toSeq
    def identifier(s: String): Boolean = s.matches("[A-Za-z_][A-Za-z0-9_$]*")
    if (path.isEmpty || path.head != circuit.main || !path.forall(identifier)) fail(s"invalid DUT root $root")
    def concrete(name: String): Module = modules.get(name) match {
      case Some(m: Module) => m
      case Some(_: ExtModule) => fail(s"opaque external module $name")
      case _ => fail(s"missing module $name")
    }
    var selected = concrete(circuit.main)
    path.tail.foreach { name =>
      val instances = statements(selected.body).collect { case i: WDefInstance if i.name == name => i }
      if (instances.size != 1) fail(s"DUT root $root does not identify a unique instance at $name")
      selected = concrete(instances.head.module)
    }

    val entries = mutable.ArrayBuffer.empty[JObject]
    val externalModels = mutable.ArrayBuffer.empty[JObject]
    val domains = mutable.Set.empty[String]
    val forwardedDomains = mutable.Set.empty[String]
    val rootClockAliases = mutable.Map.empty[String, String]
    def shape(t: Type, at: String): (BigInt, Boolean) = t match {
      case UIntType(IntWidth(w)) if w > 0 => (w, false)
      case SIntType(IntWidth(w)) if w > 0 => (w, true)
      case _ => fail(s"unresolved, aggregate, or non-bit state type ${t.serialize} at $at")
    }
    // Resolve forwarded clocks in their defining instance, not in the consumer's
    // module. An output may depend on a sibling's forwarded clock; resolving all
    // child inputs eagerly would then recurse even for an acyclic net. Carry a
    // hierarchy-qualified dependency stack and resolve only the required input.
    // Never follow a register's next-state driver as a combinational alias.
    class ClockScope(val m: Module, val hierarchy: String,
                     incoming: (String, Set[String]) => String, ancestors: Set[String]) {
      if (ancestors(m.name)) fail(s"recursive hierarchy at $hierarchy")
      val body = statements(m.body)
      val drivers = mutable.Map.empty[String, Expression]
      val combinationalAliases = body.collect {
        case DefNode(_, name, _) => name
        case DefWire(_, name, _) => name
      }.toSet
      body.foreach {
        case Connect(_, loc, expr) =>
          if (drivers.contains(loc.serialize)) fail(s"multiple drivers at $hierarchy.${loc.serialize}")
          drivers(loc.serialize) = expr
        case DefNode(_, name, expr) => drivers(name) = expr
        case _ =>
      }
      // Clock IO cells may carry an unchanged clock as a one-bit UInt between
      // asUInt/asClock casts. Track provenance through exact bit identities,
      // never through data expressions, logic gates or sequential storage.
      private def clockCarrier(t: Type): Boolean = t == ClockType || t == UIntType(IntWidth(1))
      private val inputs = m.ports.filter(p => clockCarrier(p.tpe) && p.direction == Input).map(_.name).toSet
      private val outputs = m.ports.filter(p => clockCarrier(p.tpe) && p.direction == Output).map(p => p.name -> p.tpe).toMap
      private val instances = body.collect { case i: WDefInstance => i.name -> i }.toMap
      private val children = mutable.Map.empty[String, ClockScope]
      def child(i: WDefInstance): ClockScope = children.getOrElseUpdate(i.name,
        new ClockScope(concrete(i.module), s"$hierarchy.${i.name}", (port, seen) => {
          val source = drivers.getOrElse(s"${i.name}.$port", fail(s"missing child clock at $hierarchy.${i.name}.$port"))
          clock(source, seen)
        }, ancestors + m.name))
      def clock(e: Expression, seen: Set[String] = Set.empty): String = {
        val name = e.serialize
        // A parent instance pin and the child's port have the same dotted
        // path but are separate dependency steps, not a cycle by themselves.
        val key = s"$hierarchy::$name"
        if (seen(key)) fail(s"clock alias cycle at $hierarchy.$name")
        val next = seen + key
        if (!clockCarrier(e.tpe)) fail(s"non-clock alias at $hierarchy.$name")
        if (inputs(name)) incoming(name, next)
        else e match {
          case DoPrim(firrtl.PrimOps.AsClock, Seq(value), Nil, ClockType)
              if allowOutputClockAliases && value.tpe == UIntType(IntWidth(1)) => clock(value, next)
          case DoPrim(firrtl.PrimOps.AsUInt, Seq(value), Nil, UIntType(IntWidth(width)))
              if allowOutputClockAliases && width == 1 && value.tpe == ClockType => clock(value, next)
          case DoPrim(firrtl.PrimOps.Bits, Seq(value), Seq(zeroHigh, zeroLow), UIntType(IntWidth(width)))
              if allowOutputClockAliases && width == 1 && zeroHigh == 0 && zeroLow == 0 && value.tpe == UIntType(IntWidth(1)) =>
            clock(value, next)
          case _ => name.split("\\.", -1).toSeq match {
            case Seq(instance, port) if instances.contains(instance) =>
              if (!allowOutputClockAliases) fail(s"output clock at $hierarchy.$name")
              val scope = child(instances(instance))
              val pin = scope.m.ports.find(p => p.name == port && clockCarrier(p.tpe))
                .getOrElse(fail(s"non-clock child pin at $hierarchy.$name"))
              val tpe = pin.tpe
              if (tpe != e.tpe) fail(s"reshaped child clock at $hierarchy.$name")
              if (pin.direction == Input) {
                // Rocket's serial domain fans out a child's connected clock
                // input. This is the parent net, not an output dependency.
                val source = drivers.getOrElse(name, fail(s"undriven child clock pin at $hierarchy.$name"))
                if (source.tpe != tpe) fail(s"reshaped child clock input at $hierarchy.$name")
                clock(source, next)
              } else scope.clock(Reference(port, tpe), next)
            case _ => drivers.get(name) match {
              case Some(alias) if alias.tpe == e.tpe && (combinationalAliases(name) || outputs.contains(name)) =>
                clock(alias, next)
              case _ => fail(s"derived, gated, or undriven clock at $hierarchy.$name")
            }
          }
        }
      }
    }
    val rootDomains = selected.ports.filter(p => p.tpe == ClockType && p.direction == Input).map { p =>
      p.name -> s"$root.${p.name}"
    }.toMap
    def walk(scope: ClockScope): Unit = {
      val m = scope.m
      val hierarchy = scope.hierarchy
      val body = scope.body
      val drivers = scope.drivers
      val combinationalAliases = scope.combinationalAliases
      def clock(e: Expression): String = scope.clock(e)
      // Extracted bridges forward their clock to the environment. Accept only
      // a resolved alias of a selected incoming clock, never a generated clock.
      // Distinct state clock domains still reject at the inventory boundary.
      // Ordinary inventories retain their original rejection of output clocks.
      m.ports.filter(p => p.tpe == ClockType && p.direction == Output).foreach { p =>
        if (!allowOutputClockAliases) fail(s"output clock at $hierarchy.${p.name}")
        val source = drivers.getOrElse(p.name, fail(s"undriven output clock at $hierarchy.${p.name}"))
        val domain = clock(source)
        if (!rootDomains.values.toSet(domain)) fail(s"non-target output clock at $hierarchy.${p.name}")
        forwardedDomains += domain
        if (hierarchy == root) rootClockAliases(p.name) = domain.stripPrefix(root + ".")
      }
      def enableIdentity(e: Expression, seen: Set[String] = Set.empty): String = {
        val name = e.serialize
        if (seen(name)) fail(s"enable alias cycle at $hierarchy.$name")
        e match {
          case Mux(cond, UIntLiteral(one, IntWidth(w1)), UIntLiteral(zero, IntWidth(w0)), _)
              if one == 1 && zero == 0 && w1 == 1 && w0 == 1 => enableIdentity(cond, seen + name)
          case _ => drivers.get(name) match {
            case Some(alias) if combinationalAliases(name) && e.tpe == UIntType(IntWidth(1)) =>
              enableIdentity(alias, seen + name)
            case _ => name
          }
        }
      }
      def memoryClock(e: Expression, enable: Expression, seen: Set[String] = Set.empty): String = {
        val name = e.serialize
        if (seen(name)) fail(s"memory clock alias cycle at $hierarchy.$name")
        e match {
          // Conditional Chisel writes use validif(enable, clock), with enable
          // often lowered through nodes and mux(enable, 1, 0). Only that exact
          // Boolean identity permits stripping the clock validity guard.
          case ValidIf(cond, value, ClockType) if enableIdentity(cond) == enableIdentity(enable) => clock(value)
          case _ => drivers.get(name) match {
            case Some(alias) if alias.tpe == ClockType && combinationalAliases(name) =>
              memoryClock(alias, enable, seen + name)
            case _ => clock(e)
          }
        }
      }
      def add(name: String, kind: String, t: Type, depth: BigInt, domain: String, reset: String): Unit = {
        if (!identifier(name)) fail(s"non-ASCII/nonstandard state name at $hierarchy.$name")
        val (width, signed) = shape(t, s"$hierarchy.$name")
        if (depth <= 0 || depth > Int.MaxValue || width > Int.MaxValue) fail(s"oversized/empty state at $hierarchy.$name")
        val h = s"$hierarchy.$name"
        if (domain != "initial") domains += domain
        entries += JObject(List(
          "state_id" -> JString(stateID(h, kind)), "hierarchy" -> JString(h), "kind" -> JString(kind),
          "width" -> JInt(width), "depth" -> JInt(depth), "signed" -> JBool(signed),
          "clock_domain" -> JString(domain), "reset" -> JString(reset),
          "bit_order" -> JString("lsb0"), "address_order" -> JString("ascending_zero_based"),
        ))
      }
      body.foreach {
        case r: DefRegister =>
          val reset = r.reset.tpe match {
            case UIntType(IntWidth(w)) if w == 1 => r.reset match {
              case UIntLiteral(v, _) if v == 0 => "none"
              case _ => "synchronous"
            }
            case _ => fail(s"asynchronous/unresolved reset at $hierarchy.${r.name}")
          }
          add(r.name, "register", r.tpe, 1, clock(r.clock), reset)
        case mem: DefMemory =>
          // The FIRRTL passthrough emitter stores an address for new/undefined
          // readers, data for old readers, and rdata for old/undefined readwriters.
          // These are semantic sequential owners in addition to the logical RAM.
          // Keep the exact port field type/name; never approximate it as RAM data.
          if (mem.readLatency < 0 || mem.readLatency > 1 || mem.writeLatency != 1 ||
              (mem.readwriters.nonEmpty &&
                (mem.readLatency != 1 || mem.readUnderWrite == ReadUnderWrite.New)))
            fail(s"memory latency/readwrite pipeline at $hierarchy.${mem.name} (read=${mem.readLatency}, write=${mem.writeLatency})")
          val ports = mem.readers ++ mem.writers ++ mem.readwriters
          if (ports.isEmpty) fail(s"portless memory at $hierarchy.${mem.name}")
          val portClocks = ports.map { port =>
            drivers.get(s"${mem.name}.$port.clk") match {
              case Some(e) => memoryClock(e, drivers.getOrElse(s"${mem.name}.$port.en",
                fail(s"missing memory enable at $hierarchy.${mem.name}.$port")))
              case None => fail(s"missing memory clock at $hierarchy.${mem.name}.$port")
            }
          }.distinct
          if (portClocks.size != 1) fail(s"multiple memory clocks at $hierarchy.${mem.name}")
          add(mem.name, "memory", mem.dataType, mem.depth, portClocks.head, "none")
          def pipeline(port: String, field: String): Unit = {
            val ref = firrtl.passes.MemPortUtils.memPortField(mem, port, field)
            add(firrtl.passes.LowerTypes.loweredName(ref), "register", ref.tpe, 1, portClocks.head, "none")
          }
          if (mem.readLatency == 1) {
            mem.readers.foreach(r => pipeline(r, if (mem.readUnderWrite == ReadUnderWrite.Old) "data" else "addr"))
            mem.readwriters.foreach(rw => pipeline(rw, "rdata"))
          }
        case i: WDefInstance =>
          modules.get(i.module) match {
            case Some(m: ExtModule) =>
              // Runtime initialization is state even though it has no clock.
              // Accept only the exact non-SYNTHESIS model and specialization;
              // DEFAULT is never substituted for the captured stored value.
              val sources = state.annotations.collect {
                case a: BlackBoxInlineAnno if a.target.name == m.name &&
                    a.target.circuit.name == circuit.main => a.text
              }
              val width = m.params.collect { case IntParam("WIDTH", w) => w }
              val default = m.params.collect { case IntParam("DEFAULT", v) => v }
              val format = m.params.collect { case StringParam("FORMAT", s) => s.string }
              if (m.defname != "plusarg_reader" || sources.size != 1 ||
                  sha256(sources.head) != SimulatorStateInventory.plusargModelSHA256 ||
                  m.params.size != 3 || width.size != 1 || default.size != 1 || format.size != 1 ||
                  width.head <= 0 || width.head > Int.MaxValue || default.head < 0 ||
                  default.head.bitLength > width.head || !format.head.matches("[A-Za-z_][A-Za-z0-9_]*=%[dbh]") ||
                  m.ports.map(p => (p.name, p.direction, p.tpe)) !=
                    Seq(("out", Output, UIntType(IntWidth(width.head)))))
                fail(s"opaque external module ${m.name}; no exact semantic plusarg state adapter")
              val instancePath = s"$hierarchy.${i.name}"
              // add() takes a local identifier; the external scope has no
              // FIRRTL body and cannot participate in clock alias resolution.
              val h = s"$instancePath.myplus"
              entries += JObject(List(
                "state_id" -> JString(stateID(h, "register")), "hierarchy" -> JString(h),
                "kind" -> JString("register"), "width" -> JInt(width.head), "depth" -> JInt(1),
                "signed" -> JBool(false), "clock_domain" -> JString("initial"), "reset" -> JString("none"),
                "bit_order" -> JString("lsb0"), "address_order" -> JString("ascending_zero_based"),
              ))
              externalModels += JObject(List(
                "hierarchy" -> JString(instancePath), "module" -> JString(m.name),
                "adapter" -> JString("semantic-plusarg-v1"), "defname" -> JString(m.defname),
                "source_sha256" -> JString(sha256(sources.head)), "width" -> JInt(width.head),
                "default" -> JInt(default.head), "format" -> JString(format.head),
              ))
            case _ =>
              val child = concrete(i.module)
              if (!allowOutputClockAliases && child.ports.exists(p => p.tpe == ClockType && p.direction == Output))
                fail(s"output clock at $hierarchy.${i.name}")
              val childScope = scope.child(i)
              child.ports.filter(p => p.tpe == ClockType && p.direction == Input).foreach { p =>
                childScope.clock(Reference(p.name, ClockType))
              }
              walk(childScope)
          }
        case _: Conditionally | _: CDefMemory | _: CDefMPort => fail(s"non-lowered state at $hierarchy")
        case _ =>
      }
    }
    if (!allowOutputClockAliases && selected.ports.exists(p => p.tpe == ClockType && p.direction == Output))
      fail(s"output clock at $root")
    walk(new ClockScope(selected, root, (port, _) => rootDomains.getOrElse(port,
      fail(s"clock provenance reaches data input at $root.$port")), Set.empty))
    if (entries.isEmpty) fail(s"no state under $root")
    if (domains.isEmpty || (!allowMultipleClocks && domains.size != 1))
      fail(s"multiple target clock domains: ${domains.toSeq.sorted.mkString(", ")}")
    if (!allowMultipleClocks && forwardedDomains.exists(_ != domains.head))
      fail("forwarded output clock outside the state clock domain")
    val sorted = entries.sortBy(e => (e \ "state_id").values.toString).toList
    if (sorted.map(e => (e \ "state_id").values).distinct.size != sorted.size) fail("duplicate StateID")
    if (sorted.map(e => (e \ "hierarchy").values).distinct.size != sorted.size) fail("duplicate semantic state path")
    canonical(JObject(List(
      "manifest_version" -> JInt(if (allowMultipleClocks) 3 else if (externalModels.isEmpty) 1 else 2),
      "manifest_type" -> JString("rtlckpt"),
      "identity_scheme" -> JString("pre-fame-state-v1"), "circuit" -> JString(circuit.main),
      "dut_root" -> JString(root),
      "reference_sha256" -> JString(sha256(circuit.serialize)), "state" -> JArray(sorted),
    ) ++ (if (!allowMultipleClocks) List("clock_domain" -> JString(domains.head)) else List(
      "clock_domains" -> JArray(domains.toSeq.sorted.map(JString(_)).toList),
      "clock_inputs" -> JArray(rootDomains.values.toSeq.sorted.map(JString(_)).toList),
      "clock_aliases" -> JArray(rootClockAliases.toSeq.sortBy(_._1).map { case (name, source) =>
        JObject(List("name" -> JString(name), "source" -> JString(source), "direction" -> JString("output")))
      }.toList),
    )) ++ (if (externalModels.isEmpty && !allowMultipleClocks) Nil else List(
      "external_models" -> JArray(externalModels.sortBy(e => (e \ "hierarchy").values.toString).toList),
    ))))
  }

  def apply(state: CircuitState, allowOutputClockAliases: Boolean = false): CircuitState = {
    val roots = state.annotations.collect { case SemanticCheckpointRootAnnotation(path) => path }
    val bridgeRoots = state.annotations.collect { case SemanticBridgeCheckpointRootAnnotation(path) => path }
    val access = state.annotations.collect { case a: SemanticBankedAccessAnnotation => a }
    val multiclock = state.annotations.count(_ == SemanticMultiClockInventoryAnnotation)
    if (multiclock > 1 || (multiclock != 0 && (roots.isEmpty && bridgeRoots.isEmpty)))
      fail("multi-clock inventory requires exactly one explicit root and one opt-in")
    if (multiclock != 0 && access.nonEmpty) fail("multi-clock banked cut protocol is not implemented")
    if (bridgeRoots.nonEmpty) {
      if (bridgeRoots.size != 1 || roots.nonEmpty || access.nonEmpty)
        fail("bridge-extracted inventory cannot mix root phases, duplicate roots, or banked bindings")
    }
    if (roots.isEmpty) {
      if (access.nonEmpty) fail("banked access requires an explicit semantic inventory root")
      state
    }
    else {
      if (roots.size != 1) fail("exactly one explicit DUT root is required")
      val manifest = discover(state, roots.head, allowOutputClockAliases, multiclock != 0)
      val accessFiles = if (access.isEmpty) Seq.empty else {
        if (access.size != 1 || access.head.root != roots.head) fail("banked binding root/count mismatch")
        val binding = access.head.entries
        val entries = (parse(manifest) \ "state").asInstanceOf[JArray].arr
        val expected = entries.map(e => ((e \ "state_id").values.toString,
          (e \ "width").asInstanceOf[JInt].num.toInt, (e \ "depth").asInstanceOf[JInt].num.toInt))
        if (binding.map(e => (e.stateID, e.width, e.depth)) != expected)
          fail("banked binding omits, adds, reorders, or reshapes semantic state")
        if (binding.map(e => (e.bank, e.slot)).distinct.size != binding.size)
          fail("duplicate banked client slot")
        val body = canonical(JObject(List(
          "access_version" -> JInt(1), "protocol" -> JString("banked64-v1"),
          "dut_root" -> JString(roots.head), "state_manifest_sha256" -> JString(sha256(manifest)),
          "state" -> JArray(binding.map(e => JObject(List(
            "state_id" -> JString(e.stateID), "bank" -> JInt(e.bank), "slot" -> JInt(e.slot),
            "width" -> JInt(e.width), "depth" -> JInt(e.depth),
          ))).toList),
        )))
        Seq(GoldenGateOutputFileAnnotation(body, ".semantic-access.json"))
      }
      // Emit a DUT-only ordinary RTL reference from this exact pre-FAME IR.
      // No bridges, host clocks, or simulator state participate in its build.
      val modules = state.circuit.modules.map(m => m.name -> m).toMap
      def instances(s: Statement): Seq[WDefInstance] = {
        val found = mutable.ArrayBuffer.empty[WDefInstance]
        def visit(x: Statement): Unit = {
          x match { case i: WDefInstance => found += i; case _ => }
          x.foreachStmt(visit)
        }
        visit(s)
        found.toSeq
      }
      var selected = modules(state.circuit.main).asInstanceOf[Module]
      roots.head.split("\\.").tail.foreach { name =>
        selected = modules(instances(selected.body).find(_.name == name).get.module).asInstanceOf[Module]
      }
      val reachable = mutable.Set.empty[String]
      def include(name: String): Unit = if (reachable.add(name)) {
        modules(name) match {
          case m: Module => instances(m.body).foreach(i => include(i.module))
          case _: ExtModule => // Validated exact owner in discover().
        }
      }
      include(selected.name)
      val cleanCircuit = state.circuit.copy(main = selected.name, modules = state.circuit.modules.filter(m => reachable(m.name)))
      val preserveStorage = if (multiclock == 0) Seq.empty else cleanCircuit.modules.flatMap {
        case m: Module =>
          statements(m.body).collect {
            case r: DefRegister => DontTouchAnnotation(ReferenceTarget(cleanCircuit.main, m.name, Nil, r.name, Nil))
            case m0: DefMemory => DontTouchAnnotation(ReferenceTarget(cleanCircuit.main, m.name, Nil, m0.name, Nil))
          }
        case _ => Seq.empty
      }
      val storageLocations = if (multiclock == 0) Seq.empty else {
        (parse(manifest) \ "state").asInstanceOf[JArray].arr.map { entry =>
          val relative = (entry \ "hierarchy").values.toString.stripPrefix(roots.head + ".").split("\\.").toSeq
          var scope = selected
          val instanceKeys = mutable.ArrayBuffer.empty[String]
          // InstanceTarget and ModuleTarget share IsModule, but keep the path
          // explicit so each repeated module instance remains independently owned.
          val path = relative.dropRight(1).map { name =>
            val instance = instances(scope.body).find(_.name == name).getOrElse(
              fail(s"missing clean semantic instance $name"))
            instanceKeys += s"${scope.name}::$name"
            val pair = (firrtl.annotations.TargetToken.Instance(name), firrtl.annotations.TargetToken.OfModule(instance.module))
            modules(instance.module) match { case m: Module => scope = m; case _: ExtModule => }
            pair
          }
          val pipeline = statements(scope.body).collect { case mem: DefMemory => mem }.flatMap { mem =>
            val fields = mem.readers.map(p => (p, if (mem.readUnderWrite == ReadUnderWrite.Old) "data" else "addr")) ++
              mem.readwriters.map(p => (p, "rdata"))
            if (mem.readLatency == 1) fields.map { case (port, field) =>
              (firrtl.passes.LowerTypes.loweredName(firrtl.passes.MemPortUtils.memPortField(mem, port, field)),
                mem.name, s"_${port}_${field}")
            } else Seq.empty
          }.filter(_._1 == relative.last)
          if (pipeline.size > 1) fail("ambiguous clean memory pipeline owner")
          val (reference, suffix) = pipeline.headOption.map(x => (x._2, x._3)).getOrElse((relative.last, ""))
          SemanticCleanStorageAnnotation((entry \ "state_id").values.toString,
            ReferenceTarget(cleanCircuit.main, selected.name, path, reference, Nil), suffix, instanceKeys.toSeq)
        }
      }
      // Local instance targets preserve keyword renames in each path segment.
      // FIRRTL's nonlocal ReferenceTarget update leaves some such segments in
      // their original spelling, even though it updates the terminal reference.
      val requiredInstanceKeys = storageLocations.flatMap(_.instanceKeys).toSet
      val instanceLocations = if (multiclock == 0) Seq.empty else cleanCircuit.modules.flatMap {
        case m: Module => instances(m.body).filter(i => requiredInstanceKeys(s"${m.name}::${i.name}")).map { i =>
          SemanticCleanInstanceAnnotation(s"${m.name}::${i.name}",
            InstanceTarget(cleanCircuit.main, m.name, Nil, i.name, i.module))
        }
        case _ => Seq.empty
      }
      val emitter = if (multiclock == 0) new firrtl.SystemVerilogEmitter else new SemanticCleanEmitter
      val cleanResult = new Compiler(Seq(Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter)).execute(
        CircuitState(cleanCircuit, firrtl.LowForm, Seq(
          // Preserve the same implicit memory owners as the inventoried IR and
          // Golden Gate's emission path; lowering again creates different state.
          firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation,
          firrtl.EmitCircuitAnnotation(emitter.getClass),
        ) ++ preserveStorage ++ storageLocations ++ instanceLocations)
      )
      val prepared = emitter match {
        case e: SemanticCleanEmitter => e.preparedState.getOrElse(fail("clean emitter did not expose its actual storage layout"))
        case _ => cleanResult
      }
      val cleanRTL = cleanResult.getEmittedCircuit.value
      val externalSources = state.annotations.collect {
        case a: BlackBoxInlineAnno if reachable(a.target.name) &&
            modules(a.target.name).isInstanceOf[ExtModule] && a.target.circuit.name == state.circuit.main => a.text
      }.distinct
      val clean = cleanRTL + externalSources.map("\n" + _).mkString
      val cleanAccessFiles = if (multiclock == 0) Seq.empty else {
        val locations = prepared.annotations.collect { case a: SemanticCleanStorageAnnotation => a }.sortBy(_.stateID)
        val renamedInstances = prepared.annotations.collect { case a: SemanticCleanInstanceAnnotation => a }
        if (renamedInstances.map(_.key).sorted != instanceLocations.map(_.key).sorted)
          fail("lost or duplicated clean instance location")
        val instancePaths = renamedInstances.map(a => a.key -> a.target.instance).toMap
        val originalIDs = (parse(manifest) \ "state").asInstanceOf[JArray].arr.map(e => (e \ "state_id").values.toString)
        if (locations.map(_.stateID) != originalIDs || locations.exists(a => a.target.component.nonEmpty))
          fail("lost, duplicated, or reshaped clean RTL storage annotation")
        val bindings = locations.map { a =>
          val path = (a.instanceKeys.map(key => instancePaths.getOrElse(key,
            fail("missing clean instance path segment"))) :+ (a.target.ref + a.suffix)).mkString(".")
          JObject(List("state_id" -> JString(a.stateID), "rtl_path" -> JString(path)))
        }.toList
        if (bindings.map(e => (e \ "rtl_path").values).distinct.size != bindings.size)
          fail("aliased clean RTL storage locations")
        Seq(GoldenGateOutputFileAnnotation(canonical(JObject(List(
          "access_version" -> JInt(1), "scope" -> JString("clean-rtl-inventory-only"),
          "state_manifest_sha256" -> JString(sha256(manifest)), "clean_rtl_sha256" -> JString(sha256(clean)),
          "dut_module" -> JString(prepared.circuit.main), "state" -> JArray(bindings),
        ))), ".semantic-clean-access.json"))
      }
      val outputClocks = selected.ports.filter(p => p.tpe == ClockType && p.direction == Output)
      val clockFields = if (multiclock != 0) List(
        "clock_inputs" -> (parse(manifest) \ "clock_inputs"),
        "clock_aliases" -> (parse(manifest) \ "clock_aliases"),
      ) else if (outputClocks.isEmpty) List.empty else List(
        "clock_aliases" -> JArray(outputClocks.map(p => JObject(List(
          "name" -> JString(p.name), "source" -> JString("clock"), "direction" -> JString("output"),
        ))).toList),
      )
      val boundary = canonical(JObject(List(
        "boundary_version" -> JInt(if (multiclock != 0) 3 else if (outputClocks.isEmpty) 1 else 2),
        "dut_module" -> JString(if (multiclock == 0) selected.name else prepared.circuit.main), "dut_root" -> JString(roots.head),
        "sample_phase" -> JString(if (multiclock != 0) "before_clock_event" else "before_rising_edge"),
        "ports" -> JArray(selected.ports.filterNot(p => outputClocks.contains(p) ||
          (multiclock != 0 && p.tpe == ClockType)).map { port =>
          val width = port.tpe match {
            case ClockType if port.name == "clock" && port.direction == Input => BigInt(1)
            case UIntType(IntWidth(w)) => w
            case SIntType(IntWidth(w)) => w
            case _ => fail(s"unsupported boundary type at ${port.name}")
          }
          JObject(List("name" -> JString(port.name), "width" -> JInt(width),
            "direction" -> JString(if (port.direction == Input) "input" else "output")))
        }.toList),
      ) ++ (if (multiclock != 0) Nil else List("clock" -> JString("clock"))) ++ clockFields))
      // The live observer must read the authoritative values, including AXI
      // response status/user fields which later optimization can otherwise
      // erase at this boundary. Keep only data inputs on the opt-in extracted
      // bridge path; ordinary inventories and target logic are unchanged.
      val boundaryInputs = if (!allowOutputClockAliases) Seq.empty else
        selected.ports.filter(p => p.direction == Input && p.tpe != ClockType).map { p =>
          DontTouchAnnotation(ReferenceTarget(state.circuit.main, selected.name, Nil, p.name, Nil))
        }
      state.copy(annotations = state.annotations ++ accessFiles ++ cleanAccessFiles ++ boundaryInputs ++ Seq(
        GoldenGateOutputFileAnnotation(manifest, ".semantic-state.json"),
        GoldenGateOutputFileAnnotation(state.circuit.serialize, ".semantic-reference.fir"),
        GoldenGateOutputFileAnnotation(clean, ".semantic-clean.sv"),
        GoldenGateOutputFileAnnotation(boundary, ".semantic-boundary.json"),
      ))
    }
  }
}
