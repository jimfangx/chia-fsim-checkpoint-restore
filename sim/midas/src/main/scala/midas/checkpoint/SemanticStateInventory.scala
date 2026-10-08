// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import scala.collection.mutable

import firrtl.{CircuitState, WDefInstance, CDefMemory, CDefMPort}
import firrtl.ir._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.{compact, render}

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

  def discover(state: CircuitState, root: String): String = {
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
              case Some(e) => clock(e)
              case None => fail(s"missing memory clock at $hierarchy.${mem.name}.$port")
            }
          }.distinct
          if (portClocks.size != 1) fail(s"multiple memory clocks at $hierarchy.${mem.name}")
          add(mem.name, "memory", mem.dataType, mem.depth, portClocks.head, "none")
        case i: WDefInstance =>
          val child = concrete(i.module)
          val childClocks = child.ports.filter(_.tpe == ClockType).map { p =>
            if (p.direction != Input) fail(s"output clock at $hierarchy.${i.name}.${p.name}")
            val source = drivers.getOrElse(s"${i.name}.${p.name}", fail(s"missing child clock at $hierarchy.${i.name}.${p.name}"))
            p.name -> clock(source)
          }.toMap
          walk(child, s"$hierarchy.${i.name}", childClocks, ancestors + m.name)
        case _: Conditionally | _: CDefMemory | _: CDefMPort => fail(s"non-lowered state at $hierarchy")
        case _ =>
      }
    }
    val clocks = selected.ports.filter(_.tpe == ClockType).map { p =>
      if (p.direction != Input) fail(s"output clock at $root.${p.name}")
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

  def apply(state: CircuitState): CircuitState = {
    val roots = state.annotations.collect { case SemanticCheckpointRootAnnotation(path) => path }
    if (roots.isEmpty) state
    else {
      if (roots.size != 1) fail("exactly one explicit DUT root is required")
      val manifest = discover(state, roots.head)
      state.copy(annotations = state.annotations ++ Seq(
        GoldenGateOutputFileAnnotation(manifest, ".semantic-state.json"),
        GoldenGateOutputFileAnnotation(state.circuit.serialize, ".semantic-reference.fir"),
      ))
    }
  }
}
