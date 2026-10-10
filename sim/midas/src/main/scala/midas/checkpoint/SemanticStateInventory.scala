// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import scala.collection.mutable

import firrtl.{CircuitState, WDefInstance, CDefMemory, CDefMPort}
import firrtl.ir._
import firrtl.annotations.{NoTargetAnnotation, ReferenceTarget}
import firrtl.transforms.DontTouchAnnotation
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
      inventoried.copy(annotations = inventoried.annotations.filterNot(_.isInstanceOf[SemanticCheckpointRootAnnotation]))
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

  def discover(state: CircuitState, root: String, allowOutputClockAliases: Boolean = false): String = {
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
    val domains = mutable.Set.empty[String]
    def shape(t: Type, at: String): (BigInt, Boolean) = t match {
      case UIntType(IntWidth(w)) if w > 0 => (w, false)
      case SIntType(IntWidth(w)) if w > 0 => (w, true)
      case _ => fail(s"unresolved, aggregate, or non-bit state type ${t.serialize} at $at")
    }
    def walk(m: Module, hierarchy: String, clocks: Map[String, String], ancestors: Set[String]): Unit = {
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
      def clock(e: Expression, seen: Set[String] = Set.empty): String = {
        val name = e.serialize
        if (seen(name)) fail(s"clock alias cycle at $hierarchy.$name")
        clocks.get(name) match {
          case Some(domain) => domain
          case None => drivers.get(name) match {
            case Some(alias) if alias.tpe == ClockType => clock(alias, seen + name)
            case _ => fail(s"derived, gated, or undriven clock at $hierarchy.$name")
          }
        }
      }
      // Extracted bridges forward their clock to the environment. Accept only
      // a resolved alias of the sole incoming clock, never a generated clock.
      // Ordinary inventories retain their original rejection of output clocks.
      m.ports.filter(p => p.tpe == ClockType && p.direction == Output).foreach { p =>
        if (!allowOutputClockAliases) fail(s"output clock at $hierarchy.${p.name}")
        val source = drivers.getOrElse(p.name, fail(s"undriven output clock at $hierarchy.${p.name}"))
        if (clock(source) != s"$root.clock") fail(s"non-target output clock at $hierarchy.${p.name}")
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
            case Some(alias) if alias.tpe == ClockType && !clocks.contains(name) =>
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
        domains += domain
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
          // Synchronous read pipelines are additional state, not just logical RAM words.
          // Until those are inventoried, accepting such a memory would silently lose state.
          if (mem.readLatency != 0 || mem.writeLatency != 1 || mem.readwriters.nonEmpty)
            fail(s"memory latency/readwrite pipeline at $hierarchy.${mem.name} (read=${mem.readLatency}, write=${mem.writeLatency})")
          val ports = mem.readers ++ mem.writers
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
        case i: WDefInstance =>
          val child = concrete(i.module)
          if (!allowOutputClockAliases && child.ports.exists(p => p.tpe == ClockType && p.direction == Output))
            fail(s"output clock at $hierarchy.${i.name}")
          val childClocks = child.ports.filter(p => p.tpe == ClockType && p.direction == Input).map { p =>
            val source = drivers.getOrElse(s"${i.name}.${p.name}", fail(s"missing child clock at $hierarchy.${i.name}.${p.name}"))
            p.name -> clock(source)
          }.toMap
          walk(child, s"$hierarchy.${i.name}", childClocks, ancestors + m.name)
        case _: Conditionally | _: CDefMemory | _: CDefMPort => fail(s"non-lowered state at $hierarchy")
        case _ =>
      }
    }
    if (!allowOutputClockAliases && selected.ports.exists(p => p.tpe == ClockType && p.direction == Output))
      fail(s"output clock at $root")
    val clocks = selected.ports.filter(p => p.tpe == ClockType && p.direction == Input).map { p =>
      p.name -> s"$root.${p.name}"
    }.toMap
    walk(selected, root, clocks, Set.empty)
    if (entries.isEmpty) fail(s"no state under $root")
    if (domains.size != 1) fail(s"multiple target clock domains: ${domains.toSeq.sorted.mkString(", ")}")
    val sorted = entries.sortBy(e => (e \ "state_id").values.toString).toList
    if (sorted.map(e => (e \ "state_id").values).distinct.size != sorted.size) fail("duplicate StateID")
    canonical(JObject(List(
      "manifest_version" -> JInt(1), "manifest_type" -> JString("rtlckpt"),
      "identity_scheme" -> JString("pre-fame-state-v1"), "circuit" -> JString(circuit.main),
      "dut_root" -> JString(root), "clock_domain" -> JString(domains.head),
      "reference_sha256" -> JString(sha256(circuit.serialize)), "state" -> JArray(sorted),
    )))
  }

  def apply(state: CircuitState, allowOutputClockAliases: Boolean = false): CircuitState = {
    val roots = state.annotations.collect { case SemanticCheckpointRootAnnotation(path) => path }
    val bridgeRoots = state.annotations.collect { case SemanticBridgeCheckpointRootAnnotation(path) => path }
    val access = state.annotations.collect { case a: SemanticBankedAccessAnnotation => a }
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
      val manifest = discover(state, roots.head, allowOutputClockAliases)
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
        instances(modules(name).asInstanceOf[Module].body).foreach(i => include(i.module))
      }
      include(selected.name)
      val cleanCircuit = state.circuit.copy(main = selected.name, modules = state.circuit.modules.filter(m => reachable(m.name)))
      val clean = new Compiler(Seq(Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(
        CircuitState(cleanCircuit, firrtl.LowForm, Seq(firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
      ).getEmittedCircuit.value
      val outputClocks = selected.ports.filter(p => p.tpe == ClockType && p.direction == Output)
      val clockFields = if (outputClocks.isEmpty) List.empty else List(
        "clock_aliases" -> JArray(outputClocks.map(p => JObject(List(
          "name" -> JString(p.name), "source" -> JString("clock"), "direction" -> JString("output"),
        ))).toList),
      )
      val boundary = canonical(JObject(List(
        "boundary_version" -> JInt(if (outputClocks.isEmpty) 1 else 2), "dut_module" -> JString(selected.name),
        "dut_root" -> JString(roots.head), "clock" -> JString("clock"),
        "sample_phase" -> JString("before_rising_edge"),
        "ports" -> JArray(selected.ports.filterNot(outputClocks.contains).map { port =>
          val width = port.tpe match {
            case ClockType if port.name == "clock" && port.direction == Input => BigInt(1)
            case UIntType(IntWidth(w)) => w
            case SIntType(IntWidth(w)) => w
            case _ => fail(s"unsupported boundary type at ${port.name}")
          }
          JObject(List("name" -> JString(port.name), "width" -> JInt(width),
            "direction" -> JString(if (port.direction == Input) "input" else "output")))
        }.toList),
      ) ++ clockFields))
      // The live observer must read the authoritative values, including AXI
      // response status/user fields which later optimization can otherwise
      // erase at this boundary. Keep only data inputs on the opt-in extracted
      // bridge path; ordinary inventories and target logic are unchanged.
      val boundaryInputs = if (!allowOutputClockAliases) Seq.empty else
        selected.ports.filter(p => p.direction == Input && p.tpe != ClockType).map { p =>
          DontTouchAnnotation(ReferenceTarget(state.circuit.main, selected.name, Nil, p.name, Nil))
        }
      state.copy(annotations = state.annotations ++ accessFiles ++ boundaryInputs ++ Seq(
        GoldenGateOutputFileAnnotation(manifest, ".semantic-state.json"),
        GoldenGateOutputFileAnnotation(state.circuit.serialize, ".semantic-reference.fir"),
        GoldenGateOutputFileAnnotation(clean, ".semantic-clean.sv"),
        GoldenGateOutputFileAnnotation(boundary, ".semantic-boundary.json"),
      ))
    }
  }
}
