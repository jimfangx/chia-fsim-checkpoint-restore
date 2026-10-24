// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.CircuitState
import firrtl.annotations.{Annotation, JsonProtocol, MemoryFileInlineAnnotation, MemorySynthInit, MemoryNoSynthInit, ReferenceTarget}
import firrtl.ir._
import firrtl.transforms.DontTouchAnnotation
import org.json4s._

/** Explicit annotation contracts for instance-specialized storage. Never copy
  * an annotation with an old module target or silently drop an unknown owner.
  * A local target applies to every reachable instance of its module; a nonlocal
  * target applies only to the named subtree in each instance of its parent.
  */
private[checkpoint] object GeneratedStateBankAnnotations {
  case class Clone(originalModule: String, originalPath: String, module: String, body: Module)
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Generated checkpoint bank unsupported: $why")
  private def supported(a: Annotation): Boolean = a match {
    case _: DontTouchAnnotation | _: MemoryFileInlineAnnotation | _: firrtl.transforms.CombinationalPath |
         _: SynthesisPlusArgConstantAnnotation => true
    case MemorySynthInit | MemoryNoSynthInit | firrtl.transforms.NoCircuitDedupAnnotation => true
    case _ => false
  }
  def preflight(state: CircuitState): Unit = {
    GeneratedSynthesisConstantAdapter.validate(state, instrumented = true)
    val unsupported = state.annotations.filterNot(supported)
    if (unsupported.nonEmpty) fail("input annotations require explicit clone/rename translation: " +
      unsupported.map(_.getClass.getName).distinct.mkString(", "))
    if (state.annotations.contains(MemorySynthInit) && state.annotations.contains(MemoryNoSynthInit))
      fail("conflicting memory synthesis initialization policies")
    // The resolver may omit unreachable definitions before specialization.
    // A preservation/initialization contract on one must fail here, rather
    // than be deleted by a compiler rename or an invalid-target cleanup pass.
    val reachable = GeneratedExternalOwners.instances(state, state.circuit.main).map(_._2.name).toSet
    state.annotations.foreach {
      case a: DontTouchAnnotation if !reachable(a.target.module) =>
        fail(s"annotation target is unreachable: ${a.target.serialize}")
      case a: MemoryFileInlineAnnotation if !reachable(a.target.module) =>
        fail(s"annotation target is unreachable: ${a.target.serialize}")
      case a: SynthesisPlusArgConstantAnnotation if !reachable(a.target.module) =>
        fail(s"annotation target is unreachable: ${a.target.serialize}")
      case _ => ()
    }
  }
  def annotationJSON(annotations: Seq[Annotation]): String = JsonProtocol.serialize(annotations.filter {
    case _: DontTouchAnnotation | _: MemoryFileInlineAnnotation | _: SynthesisPlusArgConstantAnnotation => true
    case MemorySynthInit | MemoryNoSynthInit | firrtl.transforms.NoCircuitDedupAnnotation => true
    case _ => false
  }.distinct.sortBy(_.serialize))
  def translatedHash(state: CircuitState): String = SemanticStateInventory.sha256(annotationJSON(state.annotations))

  def translate(state: CircuitState, clones: Seq[Clone]): (Seq[Annotation], JValue) = {
    preflight(state)
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    def declarations(m: Module): Seq[IsDeclaration] = {
      val all = mutable.ArrayBuffer.empty[IsDeclaration]
      def visit(s: Statement): Unit = { s match { case d: IsDeclaration => all += d; case _ => () }; s.foreachStmt(visit) }
      visit(m.body); all.toSeq
    }
    def targets(target: ReferenceTarget, memory: Boolean): Seq[ReferenceTarget] = {
      if (target.circuit != state.circuit.main || target.component.nonEmpty)
        fail(s"annotation requires an exact ground declaration target: ${target.serialize}")
      val parents = clones.filter(_.originalModule == target.module)
      if (parents.isEmpty) fail(s"annotation target is unreachable: ${target.serialize}")
      var leaf = target.module
      target.path.foreach { case (instance, child) =>
        val m = modules.get(leaf) match { case Some(m: Module) => m; case _ => fail("opaque annotation path") }
        if (!declarations(m).exists {
          case i: firrtl.WDefInstance => i.name == instance.value && i.module == child.value
          case _ => false
        }) fail(s"annotation instance path mismatch: ${target.serialize}")
        leaf = child.value
      }
      parents.map { parent =>
        val path = (Seq(parent.originalPath) ++ target.path.map(_._1.value)).mkString(".")
        val clone = clones.find(_.originalPath == path).getOrElse(fail(s"missing annotation clone: $path"))
        val declared = declarations(clone.body).find(_.name == target.ref)
        val valid = if (memory) declared.exists(_.isInstanceOf[DefMemory]) else
          declared.exists {
            case _: DefRegister | _: DefMemory | _: DefWire | _: DefNode => true
            case _ => false
          } || clone.body.ports.exists(_.name == target.ref)
        if (!valid) fail(s"annotation declaration/kind mismatch: ${target.serialize}")
        ReferenceTarget(state.circuit.main, clone.module, Nil, target.ref, Nil)
      }
    }
    val initTargets = mutable.Set.empty[ReferenceTarget]
    val mappings = mutable.ArrayBuffer.empty[JValue]
    val requested = state.annotations.flatMap {
      case _: firrtl.transforms.CombinationalPath => Nil // Invalidated path cache, recomputed by Compiler.
      case a @ (MemorySynthInit | MemoryNoSynthInit | firrtl.transforms.NoCircuitDedupAnnotation) => Seq(a)
      case a: DontTouchAnnotation =>
        val copies = targets(a.target, false).map(t => a.copy(target = t))
        mappings += JObject(List("source" -> JString(a.serialize),
          "targets" -> JArray(copies.map(c => JString(c.target.serialize)).toList)))
        copies
      case a: SynthesisPlusArgConstantAnnotation =>
        if (a.target.circuit != state.circuit.main) fail(s"foreign synthesis constant: ${a.target.serialize}")
        val copies = clones.filter(_.originalModule == a.target.module).map(c =>
          a.copy(target = firrtl.annotations.ModuleTarget(state.circuit.main, c.module)))
        if (copies.isEmpty) fail(s"unreachable synthesis constant: ${a.target.serialize}")
        mappings += JObject(List("source" -> JString(a.serialize),
          "targets" -> JArray(copies.map(c => JString(c.target.serialize)).toList)))
        copies
      case a: MemoryFileInlineAnnotation =>
        val copies = targets(a.target, true).map { t =>
          if (!initTargets.add(t)) fail(s"overlapping memory initialization annotations: ${t.serialize}")
          a.copy(target = t)
        }
        mappings += JObject(List("source" -> JString(a.serialize),
          "targets" -> JArray(copies.map(c => JString(c.target.serialize)).toList)))
        copies
      case a => fail(s"annotation has no translation contract: ${a.getClass.getName}")
    }
    // Instance-specialized bank wiring is authoritative. Prevent the resolver
    // from merging even stateless repeated helpers and returning stale clone
    // targets or an unowned DedupedResult side channel.
    val translated = (requested :+ firrtl.transforms.NoCircuitDedupAnnotation).distinct
    val provenance = JObject(List("translation_version" -> JInt(1),
      "source_sha256" -> JString(SemanticStateInventory.sha256(annotationJSON(state.annotations))),
      "translated_sha256" -> JString(SemanticStateInventory.sha256(annotationJSON(translated))),
      "mappings" -> JArray(mappings.toList)))
    (translated, provenance)
  }
}
