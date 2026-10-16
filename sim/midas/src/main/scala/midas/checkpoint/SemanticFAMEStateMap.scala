// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, WDefInstance, WDefInstanceConnector}
import firrtl.ir._
import firrtl.annotations._
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import firrtl.transforms.DontTouchAnnotation
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Explicit diagnostic mapping, independent of either checkpoint restore path.
  * Keeping storage is necessary: a constant-driven original register still has
  * a semantic value before its next edge. Never reconstruct erased state.
  */
case object SemanticFAMEStateMapAnnotation extends NoTargetAnnotation with Unserializable with HasShellOptions {
  val options = Seq(new ShellOption[Unit](
    longOption = "semantic-fame-state-map",
    toAnnotationSeq = (_: Unit) => Seq(SemanticFAMEStateMapAnnotation),
    helpText = "Preserve and map original schema-3 semantic storage through FAME; does not authorize capture.",
  ))
}

case class SemanticFAMEManifestAnnotation(body: String) extends NoTargetAnnotation
case class SemanticFAMEStorageAnnotation(stateID: String, target: ReferenceTarget,
    suffix: String = "", external: Boolean = false, instanceKeys: Seq[String] = Nil)
    extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget): SemanticFAMEStorageAnnotation = copy(target = n)
}

object SemanticFAMEStateMap {
  private def fail(message: String): Nothing =
    throw new IllegalArgumentException(s"Semantic FAME map unsupported: $message")
  private def root(state: CircuitState): String = {
    val roots = state.annotations.collect { case SimulatorCheckpointInventoryAnnotation(root) => root }
    if (roots.size > 1) fail("duplicate generated inventory root")
    roots.headOption.getOrElse(state.circuit.main)
  }
  private def statements(s: Statement): Seq[Statement] = {
    val xs = mutable.ArrayBuffer.empty[Statement]
    def visit(s: Statement): Unit = { xs += s; s.foreachStmt(visit) }
    visit(s); xs.toSeq
  }
  private def instances(m: DefModule): Seq[(String, String)] = m match {
    case m: Module => statements(m.body).collect {
      case i: WDefInstance => (i.name, i.module)
      case i: WDefInstanceConnector => (i.name, i.module)
    }
    case _ => Seq.empty
  }

  /** Reinstall storage preservation after FAME removes its port DontTouches.
    * The tracking annotations follow compiler renames; a target's spelling is
    * never guessed from an original hierarchical name.
    */
  def preserve(state: CircuitState): CircuitState = {
    val owners = state.annotations.collect { case a: SemanticFAMEStorageAnnotation if !a.external => a }
    if (owners.isEmpty) state
    else state.copy(annotations = state.annotations ++ owners.map(a => DontTouchAnnotation(a.target)).distinct)
  }

  /** Resolve nonlocal tracking to concrete storage before emitter renaming.
    * Local instance annotations then carry every emitted path segment, including
    * Verilog keywords. Ambiguous replicated/threaded storage is unsupported.
    */
  def prepareEmission(state: CircuitState): CircuitState = {
    if (!state.annotations.contains(SemanticFAMEStateMapAnnotation)) return state
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    val paths = mutable.Map.empty[String, Vector[Seq[(String, String)]]]
    def walk(module: String, path: Seq[(String, String)], seen: Set[String]): Unit = {
      if (seen(module)) fail("recursive generated hierarchy")
      paths(module) = paths.getOrElse(module, Vector.empty) :+ path
      instances(modules(module)).foreach { case (name, child) => walk(child, path :+ (name -> child), seen + module) }
    }
    val selectedRoot = root(state)
    if (!modules.contains(selectedRoot)) fail("missing generated inventory root")
    walk(state.circuit.main, Nil, Set.empty)
    val rootPaths = paths.getOrElse(selectedRoot, Vector.empty)
    if (rootPaths.size != 1) fail("ambiguous generated inventory root")
    val original = state.annotations.collect { case a: SemanticFAMEStorageAnnotation => a }
    val manifests = state.annotations.collect { case a: SemanticFAMEManifestAnnotation => a.body }
    if (manifests.size != 1) fail("missing/duplicate semantic manifest")
    val ids = (parse(manifests.head) \ "state").asInstanceOf[JArray].arr.map(e => (e \ "state_id").values.toString)
    if (original.map(_.stateID).sorted != ids.sorted) fail("lost or duplicated original StateID")
    val instanceAnnotations = mutable.LinkedHashMap.empty[String, SemanticCleanInstanceAnnotation]
    val locations = original.map { a =>
      val parents = paths.getOrElse(a.target.module, Vector.empty)
      if (a.target.circuit != state.circuit.main || parents.size != 1 || a.target.component.nonEmpty)
        fail(s"ambiguous, missing or reshaped storage for ${a.stateID}: ${a.target.serialize}")
      val tail = a.target.path.map { case (i, m) => (i.value -> m.value) }
      val absolute = parents.head ++ tail
      if (!absolute.startsWith(rootPaths.head)) fail(s"semantic owner outside generated root: ${a.stateID}")
      val full = absolute.drop(rootPaths.head.size)
      var module = selectedRoot
      val keys = full.map { case (name, child) =>
        if (!instances(modules(module)).contains(name -> child)) fail(s"missing instance $module.$name")
        val key = s"semantic-fame::$module::$name"
        instanceAnnotations(key) = SemanticCleanInstanceAnnotation(key,
          InstanceTarget(state.circuit.main, module, Nil, name, child))
        module = child
        key
      }
      a.copy(target = ReferenceTarget(state.circuit.main, module, Nil, a.target.ref, Nil), instanceKeys = keys)
    }
    state.copy(annotations = state.annotations.filterNot(_.isInstanceOf[SemanticFAMEStorageAnnotation]) ++
      locations ++ instanceAnnotations.values)
  }

  /** Use the emitter's actual prepared IR and its complete generated inventory.
    * Validate kind/width/depth for every StateID, not just annotation survival.
    */
  def emit(state: CircuitState, rtl: String): GoldenGateOutputFileAnnotation = {
    val manifests = state.annotations.collect { case a: SemanticFAMEManifestAnnotation => a.body }
    if (manifests.size != 1) fail("missing/duplicate semantic manifest")
    val entries = (parse(manifests.head) \ "state").asInstanceOf[JArray].arr
    val locations = state.annotations.collect { case a: SemanticFAMEStorageAnnotation => a }.sortBy(_.stateID)
    if (locations.map(_.stateID) != entries.map(e => (e \ "state_id").values.toString))
      fail("lost or duplicated emitted StateID")
    val instances = state.annotations.collect { case a: SemanticCleanInstanceAnnotation => a.key -> a.target.instance }.toMap
    val selectedRoot = root(state)
    val generated = parse(SimulatorStateInventory.discover(state, selectedRoot))
    val storage = (generated \ "state").asInstanceOf[JArray].arr.map(e => (e \ "hierarchy").values.toString -> e).toMap
    val bindings = (locations zip entries).map { case (a, e) =>
      val terminal = if (a.external) "myplus" else a.target.ref + a.suffix
      val path = (Seq(selectedRoot) ++ a.instanceKeys.map(k => instances.getOrElse(k,
        fail(s"lost emitted instance $k"))) :+ terminal).mkString(".")
      val actual = storage.getOrElse(path, fail(s"missing emitted storage for ${a.stateID}: $path"))
      if (a.target.component.nonEmpty || Seq("kind", "width", "depth", "signed").exists(k => (e \ k) != (actual \ k)))
        fail(s"reshaped emitted storage for ${a.stateID}: $path")
      JObject(List("state_id" -> JString(a.stateID), "rtl_path" -> JString(path),
        "generated_state_id" -> (actual \ "state_id")))
    }
    if (bindings.map(b => (b \ "rtl_path").values).distinct.size != bindings.size)
      fail("aliased emitted storage")
    GoldenGateOutputFileAnnotation(SemanticStateInventory.canonical(JObject(List(
      "map_version" -> JInt(1), "scope" -> JString("semantic-storage-map-only"),
      "state_manifest_sha256" -> JString(SemanticStateInventory.sha256(manifests.head)),
      "generated_rtl_sha256" -> JString(SemanticStateInventory.sha256(rtl)),
      "generated_manifest_sha256" -> JString(SemanticStateInventory.sha256(SemanticStateInventory.canonical(generated))),
      "capture_supported" -> JBool(false), "state" -> JArray(bindings.toList),
    ))), ".semantic-fame-map.json")
  }
}

class PreserveSemanticFAMEStorage extends firrtl.Transform {
  def inputForm = firrtl.LowForm
  def outputForm = firrtl.LowForm
  def execute(state: CircuitState): CircuitState = SemanticFAMEStateMap.preserve(state)
}
