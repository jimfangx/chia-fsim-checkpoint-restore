// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm}
import firrtl.annotations.{InstanceTarget, JsonProtocol, ModuleTarget}
import org.json4s._

/** Projection at the actual post-lowering compiler boundary. Inert stage
  * context remains separate from storage annotations and is retained verbatim.
  * This does not provide any missing freeze or capture owner.
  */
object NativeCheckpointInPlaceContext {
  import SemanticStateInventory.{canonical, sha256}
  case class Result(state: CircuitState, native: NativeCheckpointInPlace.Result,
      context: String, binding: String, reference: CircuitState,
      manifest: String, plan: String, dependencies: String, host: CircuitState)
  private def json(as: firrtl.AnnotationSeq): String = JsonProtocol.serialize(as.distinct.sortBy(_.serialize))
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "Native in-place compiler context: " + why)

  def generate(host: CircuitState, target: InstanceTarget): Result = {
    check(host.form == LowForm, "resolved LowForm required")
    val (context, owners) = host.annotations.partition(SimulatorBankProbeContext.accepts)
    val storage = host.copy(annotations = owners)
    // Validate context before constructing banks; do not let deduplication hide
    // duplicate options, conflicting enums or passive output suffixes.
    SimulatorBankProbeContext.snapshot(context, host.circuit.serialize, host.circuit.serialize)
    GeneratedStateBankAnnotations.preflight(storage)
    check(target.circuit == host.circuit.main && target.module == host.circuit.main && target.asPath.nonEmpty,
      "target must name an existing execution rooted at the enclosing circuit")
    val instances = GeneratedExternalOwners.instances(storage, storage.circuit.main).toMap
    var path = storage.circuit.main
    target.asPath.foreach { case (i, m) =>
      path += "." + i.value
      check(instances.get(path).exists(_.name == m.value), "stale selected instance " + path)
    }
    val root = target.asPath.last._2.value
    val reachable = GeneratedExternalOwners.instances(storage, root).map(_._2.name).toSet
    // Target-bearing host annotations are projected against the COMPLETE host
    // below. Only concrete synthesis-constant contracts are also required by
    // the selected bank generator. Memory initialization, preservation, XDC,
    // target-box and historical dedup targets must not be independently
    // flattened into a local root (or duplicated across unrelated executions).
    val constants = owners.collect { case a: SynthesisPlusArgConstantAnnotation if reachable(a.target.module) =>
      a.copy(target = ModuleTarget(root, a.target.module))
    }
    val reference = CircuitState(storage.circuit.copy(main = root,
      modules = storage.circuit.modules.filter(m => reachable(m.name))), LowForm, constants)
    val manifest = SimulatorStateInventory.discover(reference)
    val plan = PhysicalClockCutPlan.discover(reference, manifest)
    val dependencies = PhysicalClockCutDependencies.discover(reference, manifest, plan)
    val native = NativeCheckpointInPlace.generate(storage, target, reference, manifest, plan, dependencies)
    val projected = native.state.copy(annotations = native.state.annotations ++ context)
    val snapshot = SimulatorBankProbeContext.snapshot(context, host.circuit.serialize, projected.circuit.serialize)
    val binding = canonical(JObject(List(
      "context_binding_version" -> JInt(1), "scope" -> JString("inplace_compiler_projection_only"),
      "target" -> JString(target.serialize),
      "host_sha256" -> JString(sha256(host.circuit.serialize)),
      "instrumented_sha256" -> JString(sha256(projected.circuit.serialize)),
      "host_annotations_sha256" -> JString(sha256(json(host.annotations))),
      "storage_annotations_sha256" -> JString(sha256(json(owners))),
      "context_annotations_sha256" -> JString(sha256(json(context))),
      "instrumented_annotations_sha256" -> JString(sha256(json(projected.annotations))),
      "selected_reference_annotations_sha256" -> JString(sha256(json(reference.annotations))),
      "native_binding_sha256" -> JString(sha256(native.binding)),
      "compiler_context_sha256" -> JString(sha256(snapshot)),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    Result(projected, native, snapshot, binding, reference, manifest, plan, dependencies, host)
  }

  /** Both the caller's retained state and the actual downstream state must
    * preserve context. Rehashing a sidecar cannot excuse a changed option or
    * payload, and the storage translation is independently checked again.
    */
  def validate(result: Result, downstream: CircuitState): Unit = {
    val sourceContext = result.host.annotations.filter(SimulatorBankProbeContext.accepts)
    val expected = SimulatorBankProbeContext.snapshot(sourceContext, result.host.circuit.serialize,
      result.native.state.circuit.serialize)
    check(result.context == expected, "compiler context changed from source")
    val binding = org.json4s.native.JsonMethods.parse(result.binding)
    check((binding \ "context_binding_version") == JInt(1) &&
      (binding \ "scope") == JString("inplace_compiler_projection_only") &&
      (binding \ "target") == (org.json4s.native.JsonMethods.parse(result.native.binding) \ "target"),
      "changed projection contract")
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").foreach(k => check((binding \ k) == JBool(false), "changed capture scope"))
    check((binding \ "host_sha256") == JString(sha256(result.host.circuit.serialize)) &&
      (binding \ "host_annotations_sha256") == JString(sha256(json(result.host.annotations))) &&
      (binding \ "storage_annotations_sha256") == JString(sha256(json(result.host.annotations.filterNot(
        SimulatorBankProbeContext.accepts)))) &&
      (binding \ "context_annotations_sha256") == JString(sha256(json(sourceContext))) &&
      (binding \ "native_binding_sha256") == JString(sha256(result.native.binding)) &&
      (binding \ "selected_reference_annotations_sha256") == JString(sha256(json(result.reference.annotations))),
      "source projection binding changed")
    Seq(result.state, downstream).foreach { state =>
      val (context, owners) = state.annotations.partition(SimulatorBankProbeContext.accepts)
      check((binding \ "instrumented_sha256") == JString(sha256(state.circuit.serialize)),
        "instrumented circuit changed")
      check((binding \ "instrumented_annotations_sha256") == JString(sha256(json(state.annotations))),
        "projected annotations changed")
      val snapshot = SimulatorBankProbeContext.snapshot(context, result.host.circuit.serialize,
        state.circuit.serialize)
      check(snapshot == result.context, "compiler context changed")
      check((binding \ "compiler_context_sha256") == JString(sha256(result.context)), "unbound compiler context")
      check(owners.toVector == result.native.state.annotations.toVector, "storage projection changed")
      GeneratedStateBankAnnotations.preflight(state.copy(annotations = owners))
    }
  }
}
