// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, WDefInstance, WDefInstanceConnector, CDefMemory, CDefMPort}
import firrtl.ir._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import firrtl.transforms.BlackBoxInlineAnno
import midas.passes.DefineAbstractClockGate
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._

case class SimulatorCheckpointInventoryAnnotation(rootModule: String) extends NoTargetAnnotation with Unserializable
object SimulatorCheckpointInventoryAnnotation extends HasShellOptions {
  val options = Seq(new ShellOption[String](
    longOption = "simulator-checkpoint-inventory",
    toAnnotationSeq = root => Seq(SimulatorCheckpointInventoryAnnotation(root)),
    helpText = "Inventory all final generated simulator RTL state (independent of pre-FAME semantic state).",
  ))
}

/** Read-only inventory after the VerilogOptimized passes which the emitter
  * normally runs internally (including memory delay lowering).
  * This covers generated RTL only. Drivers, transport, harness and functional
  * host DRAM require separate owners before a full .fsckpt can be published.
  */
object SimulatorStateInventory {
  // Backend must verify the actual supplied metasim model against this hash.
  val bufgceModelSHA256 = "061be30e475233b0ac347aafb88d470279f16c4ebb1908f399edd4446c811e28"
  private def fail(message: String): Nothing =
    throw new IllegalArgumentException(s"Simulator checkpoint unsupported: $message")

  def stateID(path: String, kind: String): String = SemanticStateInventory.sha256(
    SemanticStateInventory.canonical(JObject(List(
      "hierarchy" -> JString(path), "kind" -> JString(kind),
      "namespace" -> JString("generated-simulator-state-v1"),
    )))
  )

  def discover(state: CircuitState, rootModule: String = ""): String = {
    val circuit = state.circuit
    val modules = circuit.modules.map(m => m.name -> m).toMap
    if (modules.size != circuit.modules.size) fail("duplicate module definitions")
    val entries = mutable.ArrayBuffer.empty[JValue]
    val instances = mutable.ArrayBuffer.empty[JValue]
    def identifier(name: String): Boolean = name.matches("[A-Za-z_][A-Za-z0-9_$]*")
    def shape(t: Type, path: String): (BigInt, Boolean) = t match {
      case UIntType(IntWidth(w)) if w > 0 && w <= Int.MaxValue => (w, false)
      case SIntType(IntWidth(w)) if w > 0 && w <= Int.MaxValue => (w, true)
      case _ => fail(s"unresolved, aggregate or non-bit state at $path: ${t.serialize}")
    }
    def add(path: String, module: String, kind: String, t: Type, depth: BigInt,
            clock: String, reset: String, adapter: String): Unit = {
      if (!path.split("\\.").forall(identifier)) fail(s"unsupported hierarchy $path")
      if (depth <= 0 || depth > Int.MaxValue) fail(s"unsupported depth at $path")
      val (width, signed) = shape(t, path)
      entries += JObject(List(
        "state_id" -> JString(stateID(path, kind)), "hierarchy" -> JString(path),
        "module" -> JString(module), "kind" -> JString(kind),
        "width" -> JInt(width), "depth" -> JInt(depth), "signed" -> JBool(signed),
        "clock_expression" -> JString(clock), "reset_expression" -> JString(reset),
        "adapter" -> JString(adapter), "bit_order" -> JString("lsb0"),
        "address_order" -> JString("ascending_zero_based"),
      ))
    }
    def walk(name: String, path: String, ancestors: Set[String]): Unit = {
      if (!identifier(name) || !path.split("\\.").forall(identifier)) fail(s"unsupported hierarchy $path")
      if (ancestors(name)) fail(s"recursive hierarchy at $path")
      val module = modules.getOrElse(name, fail(s"missing module $name at $path"))
      val sourceHash = module match {
        case m: ExtModule =>
          // An explicit adapter for this exact inline clock-gate implementation.
          // Name/signature alone cannot establish that an opaque module has one bit of state.
          val expected = DefineAbstractClockGate.blackbox
          val sources = state.annotations.collect { case a: BlackBoxInlineAnno if a.target.name == name => a.text }
          if (m.params.nonEmpty ||
              m.ports.map(p => (p.name, p.direction, p.tpe)) != expected.ports.map(p => (p.name, p.direction, p.tpe)))
            fail(s"opaque external module $name at $path; no exact state adapter")
          if (m.defname == expected.defname && sources.size == 1 && sources.head == DefineAbstractClockGate.impl) {
            add(s"$path.enable", name, "register", UIntType(IntWidth(1)), 1, "I", "none", "abstract-clock-gate-v1")
            SemanticStateInventory.sha256(sources.head)
          } else if (m.defname == "BUFGCE" && sources.isEmpty) {
            // The metasim model stores O in a procedural output register.
            // It has no CE latch. This adapter does not describe FPGA primitives.
            add(s"$path.O", name, "register", UIntType(IntWidth(1)), 1,
              "posedge/negedge I", "none", "metasim-bufgce-v1")
            bufgceModelSHA256
          } else fail(s"opaque external module $name at $path; no exact state adapter")
        case m: Module =>
          def visit(s: Statement): Unit = {
            s match {
              case r: DefRegister =>
                r.reset.tpe match {
                  case UIntType(IntWidth(w)) if w == 1 =>
                  case _ => fail(s"asynchronous/unresolved reset at $path.${r.name}")
                }
                add(s"$path.${r.name}", name, "register", r.tpe, 1,
                  r.clock.serialize, r.reset.serialize, "firrtl-register-v1")
              case mem: DefMemory =>
                // Implicit emitter pipelines must have become explicit registers by this boundary.
                if (mem.readLatency != 0 || mem.writeLatency != 1 || mem.readwriters.nonEmpty)
                  fail(s"hidden memory pipeline at $path.${mem.name}")
                add(s"$path.${mem.name}", name, "memory", mem.dataType, mem.depth,
                  "memory-port-clocks", "none", "firrtl-memory-v1")
              case i: WDefInstance => walk(i.module, s"$path.${i.name}", ancestors + name)
              case i: WDefInstanceConnector => walk(i.module, s"$path.${i.name}", ancestors + name)
              case _: CDefMemory | _: CDefMPort | _: Conditionally => fail(s"non-lowered state at $path")
              case _ =>
            }
            s.foreachStmt(visit)
          }
          visit(m.body)
          SemanticStateInventory.sha256(m.serialize)
        case _ => fail(s"unsupported module wrapper at $path")
      }
      instances += JObject(List("hierarchy" -> JString(path), "module" -> JString(name),
        "implementation_sha256" -> JString(sourceHash)))
    }
    val root = if (rootModule.isEmpty) circuit.main else rootModule
    walk(root, root, Set.empty)
    val sorted = entries.sortBy(e => (e \ "state_id").values.toString).toList
    if (sorted.isEmpty || sorted.map(e => (e \ "state_id").values).distinct.size != sorted.size)
      fail("empty or duplicate state inventory")
    SemanticStateInventory.canonical(JObject(List(
      "manifest_version" -> JInt(1), "manifest_type" -> JString("fsckpt-generated"),
      "identity_scheme" -> JString("generated-simulator-state-v1"),
      "circuit" -> JString(circuit.main), "scope" -> JString("generated_rtl_only"),
      "root_module" -> JString(root),
      "external_runtime_required" -> JBool(true),
      "reference_sha256" -> JString(SemanticStateInventory.sha256(circuit.serialize)),
      "instances" -> JArray(instances.sortBy(e => (e \ "hierarchy").values.toString).toList),
      "state" -> JArray(sorted),
    )))
  }

  def apply(state: CircuitState): CircuitState = {
    val requested = state.annotations.collect { case a: SimulatorCheckpointInventoryAnnotation => a.rootModule }
    if (requested.isEmpty) state
    else {
      if (requested.size != 1) fail("exactly one inventory request is required")
      val manifest = discover(state, requested.head)
      state.copy(annotations = state.annotations ++ Seq(
        GoldenGateOutputFileAnnotation(manifest, ".simulator-state.json"),
        GoldenGateOutputFileAnnotation(state.circuit.serialize, ".simulator-reference.fir"),
      ))
    }
  }
}
