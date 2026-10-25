// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, WDefInstance, WRef, WSubField}
import firrtl.annotations.{CircuitName, InstanceTarget, JsonProtocol}
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse
import scala.collection.mutable

/** Replace an existing functional instance with its reconstructed native banks
  * and clock mask. Ancestors and unrelated control/transport execution retain
  * their original clocks and state. There is exactly one functional execution.
  *
  * This deliberately bounded integration API exposes the access sideband at the
  * enclosing root. It neither supplies freeze-owner acknowledgements nor admits
  * a complete simulator image. Host annotations use the explicit bank annotation
  * contracts against the complete old/new hierarchy; unknown owners reject.
  */
object NativeCheckpointInPlace {
  import SemanticStateInventory.{canonical, sha256}
  val prefix = "checkpoint_inplace_"
  case class Result(state: CircuitState, binding: String, identity: NativeCheckpointIdentity.Result,
      annotationProjection: String, generatedAnnotations: firrtl.AnnotationSeq)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "Native in-place checkpoint: " + why)
  private def statements(s: Statement): Seq[Statement] = {
    val out = mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }

  def generate(host: CircuitState, target: InstanceTarget, reference: CircuitState,
      manifest: String, plan: String, dependencies: String): Result = {
    check(host.form == LowForm && reference.form == LowForm, "resolved LowForm required")
    GeneratedStateBankAnnotations.preflight(host)
    check(target.circuit == host.circuit.main && target.module == host.circuit.main,
      "instance must be rooted at the enclosing circuit")
    val modules = host.circuit.modules.map(m => m.name -> m).toMap
    check(modules.size == host.circuit.modules.size, "duplicate host module definition")
    val live = GeneratedExternalOwners.instances(host, host.circuit.main)
    val path = target.asPath.map { case (i, m) => i.value -> m.value }
    check(path.nonEmpty && path.last._2 == reference.circuit.main, "selected root differs from reference")
    var parent = host.circuit.main
    val parents = path.map { case (instance, child) =>
      val definition = modules.get(parent) match {
        case Some(m: Module) => m
        case _ => throw new IllegalArgumentException("Native in-place checkpoint: opaque ancestor")
      }
      check(live.count(_._2.name == parent) == 1, "shared ancestor requires explicit specialization")
      val instances = statements(definition.body).collect { case i: WDefInstance if i.name == instance => i }
      check(instances.size == 1 && instances.head.module == child, "instance path mismatch")
      val value = (definition, instance, child)
      parent = child
      value
    }
    check(live.count(_._2.name == reference.circuit.main) == 1,
      "selected module must have exactly one concrete execution")
    // The supplied source must be the current executing subtree, including
    // every reachable leaf definition. Same interface or a rehashed manifest
    // cannot substitute a different implementation.
    val referenceLive = GeneratedExternalOwners.instances(reference, reference.circuit.main)
    referenceLive.map(_._2).distinct.foreach { m =>
      check(modules.get(m.name).contains(m), "stale or different executing definition " + m.name)
    }
    val banks = GeneratedStateBankAccess.generatePhysicalCompanion(reference, manifest, plan, dependencies)
    val wired = PhysicalClockCutWiring.generateNative(reference, manifest, plan, dependencies, banks)
    val original = modules(reference.circuit.main).asInstanceOf[Module]
    val generatedRoot = wired.state.circuit.modules.find(_.name == original.name).get.asInstanceOf[Module]
    val originalPorts = original.ports.map(_.name).toSet
    val access = generatedRoot.ports.filterNot(p => originalPorts(p.name) || p.name == "ckpt_clock")
    check(access.nonEmpty && generatedRoot.ports.count(_.name == "ckpt_clock") == 1,
      "missing independent access endpoint")

    // Resolve only transparent aliases to a scalar ancestor Clock input.
    // Gates, muxes, constants, data casts and unrelated source clocks reject.
    val JString(commonClock) = parse(plan) \ "common_source_clock"
    var sourcePort = commonClock
    val clockBindings = parents.reverse.map { case (m, instance, _) =>
      val drivers = statements(m.body).collect {
        case c: Connect => c.loc.serialize -> c.expr
        case n: DefNode => n.name -> n.value
      }.groupBy(_._1)
      def resolve(name: String, seen: Set[String]): String = {
        check(!seen(name), "clock alias cycle")
        if (m.ports.exists(p => p.name == name && p.direction == Input && p.tpe == ClockType)) name
        else {
          val ds = drivers.getOrElse(name, Nil)
          check(ds.size == 1, "undriven or multiply driven raw clock " + m.name + "." + name)
          ds.head._2 match {
            case r: WRef if r.tpe == ClockType => resolve(r.name, seen + name)
            case _ => throw new IllegalArgumentException("Native in-place checkpoint: nontransparent raw clock")
          }
        }
      }
      val selectedPin = sourcePort
      sourcePort = resolve(instance + "." + selectedPin, Set.empty)
      (m.name, instance, selectedPin, sourcePort)
    }
    val enclosingClock = sourcePort
    val byParent = parents.map { case (m, i, child) => m.name -> (i, child) }.toMap
    val clocks = clockBindings.map(x => x._1 -> x._4).toMap
    val rewritten = host.circuit.modules.map {
      case m: Module if byParent.contains(m.name) =>
        check(!(m.ports.map(_.name) ++ statements(m.body).collect { case d: IsDeclaration => d.name })
          .exists(_.startsWith(prefix)), "reserved sideband identifier in " + m.name)
        val (instance, child) = byParent(m.name)
        val immediate = child == original.name
        val connections = access.map { p =>
          val childName = if (immediate) p.name else prefix + p.name
          val pin = WSubField(WRef(instance), childName, p.tpe)
          val exposed = WRef(prefix + p.name, p.tpe)
          if (p.direction == Input) Connect(NoInfo, pin, exposed) else Connect(NoInfo, exposed, pin)
        }
        val raw = if (!immediate) Nil else Seq(Connect(NoInfo,
          WSubField(WRef(instance), "ckpt_clock", ClockType), WRef(clocks(m.name), ClockType)))
        m.copy(ports = m.ports ++ access.map(p => p.copy(name = prefix + p.name)),
          body = Block(Seq(m.body) ++ connections ++ raw))
      case m if m.name == original.name => generatedRoot
      case m => m
    }
    val hostNames = host.circuit.modules.map(_.name).toSet
    val added = wired.state.circuit.modules.filterNot(_.name == original.name).filter { m =>
      if (!hostNames(m.name)) true
      else { check(modules(m.name) == m, "generated module collides with enclosing definition " + m.name); false }
    }
    val circuit = host.circuit.copy(modules = rewritten ++ added)
    val working = firrtl.passes.ToWorkingIR.run(circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed)
    firrtl.passes.CheckFlows.run(flowed)
    val renames = firrtl.RenameMap(Map(CircuitName(reference.circuit.main) -> Seq(CircuitName(host.circuit.main))))
    val grafted = CircuitState(flowed, LowForm)
    val after = GeneratedExternalOwners.instances(grafted, host.circuit.main).toMap
    // Follow concrete instance names, not module names: a shared input definition
    // can have specialized banked executions and unchanged sibling executions.
    // Parent-rooted constraints must retain all correlated instance hops.
    val clones = live.collect { case (path, m: Module) =>
      val emitted = after.getOrElse(path,
        throw new IllegalArgumentException("Native in-place checkpoint: lost execution " + path))
      check(emitted.isInstanceOf[Module], "changed concrete owner " + path)
      GeneratedStateBankAnnotations.Clone(m.name, path, emitted.name, m)
    }
    val (projected, projection) = GeneratedStateBankAnnotations.translate(host, clones)
    val generated = wired.state.annotations.flatMap(_.update(renames))
    val result = grafted.copy(annotations = (projected ++ generated).filterNot(
      _.isInstanceOf[firrtl.transforms.CombinationalPath]).distinct)
    // Check the actual new declarations as well as the original targets. The
    // second, identity translation validates every path, pin and memory kind
    // without changing any circuit or annotation. No invalid-target cleanup is
    // allowed to hide a stale target after the graft.
    val finalClones = after.toSeq.collect { case (path, m: Module) =>
      GeneratedStateBankAnnotations.Clone(m.name, path, m.name, m)
    }
    val (validated, _) = GeneratedStateBankAnnotations.translate(result, finalClones)
    check(validated.toSet == result.annotations.filterNot(_.isInstanceOf[firrtl.transforms.CombinationalPath]).toSet,
      "post-graft annotation targets changed during validation")
    def annotationJSON(as: firrtl.AnnotationSeq): String = JsonProtocol.serialize(as.distinct.sortBy(_.serialize))
    val annotationProjection = canonical(JObject(List(
      "projection_version" -> JInt(1),
      "host_annotations_sha256" -> JString(sha256(annotationJSON(host.annotations))),
      "generated_annotations_sha256" -> JString(sha256(annotationJSON(generated))),
      "instrumented_annotations_sha256" -> JString(sha256(annotationJSON(result.annotations))),
      "host_translation" -> projection,
      "invalidated_path_caches" -> JArray(host.annotations.collect {
        case a: firrtl.transforms.CombinationalPath => JString(a.serialize)
      }.toList),
      "invalidated_generated_path_caches" -> JArray(generated.collect {
        case a: firrtl.transforms.CombinationalPath => JString(a.serialize)
      }.toList),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false))))
    val identity = NativeCheckpointIdentity.discover(host, target, reference, manifest, plan, dependencies)
    val binding = canonical(JObject(List(
      "binding_version" -> JInt(3), "scope" -> JString("existing_instance_native_banks_only"),
      "target" -> JString(target.serialize), "host_sha256" -> JString(sha256(host.circuit.serialize)),
      "reference_sha256" -> JString(sha256(reference.circuit.serialize)),
      "bank_binding_sha256" -> JString(sha256(banks.binding)),
      "wiring_binding_sha256" -> JString(sha256(wired.binding)),
      "instrumented_sha256" -> JString(sha256(result.circuit.serialize)),
      "identity_binding_sha256" -> JString(sha256(identity.binding)),
      "annotation_projection_sha256" -> JString(sha256(annotationProjection)),
      "enclosing_raw_clock" -> JString(enclosingClock),
      "selected_raw_clock" -> JString(commonClock),
      "endpoint_raw_clock" -> JString("ckpt_clock"),
      "sideband_ports" -> JArray(access.map(p => JObject(List(
        "selected" -> JString(p.name), "enclosing" -> JString(prefix + p.name),
        "direction" -> JString(if (p.direction == Input) "input" else "output"),
        "type" -> JString(p.tpe.serialize)))).toList),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    Result(result, binding, identity, annotationProjection, generated)
  }
}
