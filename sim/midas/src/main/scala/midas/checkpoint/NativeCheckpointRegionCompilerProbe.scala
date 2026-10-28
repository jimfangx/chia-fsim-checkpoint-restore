// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardCopyOption, StandardOpenOption}
import firrtl.{AnnotationSeq, CircuitState, LowForm}
import firrtl.annotations.{InstanceTarget, JsonProtocol, ModuleTarget, NoTargetAnnotation}
import firrtl.ir.Module
import firrtl.options.{HasShellOptions, ShellOption, TargetDirAnnotation, Unserializable}
import org.json4s._
import scala.util.control.NonFatal

case class NativeCheckpointRegionCompilerProbeAnnotation(paths: String) extends NoTargetAnnotation with Unserializable
case class NativeCheckpointShellCompilerProbeAnnotation(paths: String) extends NoTargetAnnotation with Unserializable
/** Inert metadata-resolution history, never a storage or freeze-owner contract. */
case class NativeCheckpointRegionCompilerMetadataAnnotation(body: String) extends NoTargetAnnotation
object NativeCheckpointRegionCompilerProbeAnnotation extends HasShellOptions {
  val options = Seq(new ShellOption[String](
    longOption = "simulator-native-region-compiler-probe",
    toAnnotationSeq = value => Seq(NativeCheckpointRegionCompilerProbeAnnotation(value)),
    helpText = "Diagnose existing regional widget hookup at the actual compiler boundary; comma-separated concrete execution paths, never a deployable image.",
  ))
}
object NativeCheckpointShellCompilerProbeAnnotation extends HasShellOptions {
  val options = Seq(new ShellOption[String](
    longOption = "simulator-native-shell-compiler-probe",
    toAnnotationSeq = value => Seq(NativeCheckpointShellCompilerProbeAnnotation(value)),
    helpText = "Add local-only root/ancestor banks to the explicit regional compiler diagnostic; comma-separated concrete paths.",
  ))
}

/** Opt-in diagnostic on the ACTUAL pre/post-inline states. Artifacts are written
  * before the normal image guard, so a later rejection cannot erase evidence.
  * No caller can substitute a reference circuit, annotations, IDs or counts.
  * Successful readout/identity reconstruction still returns an incomplete image
  * to the unchanged production reservation guard.
  */
object NativeCheckpointRegionCompilerProbe {
  import SemanticStateInventory.{canonical, sha256}
  val directory = "checkpoint-region-compiler-probe"
  private val identifier = "[A-Za-z_][A-Za-z0-9_$]*"
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "native region compiler probe: " + why)

  /** Backend transforms can leave stale expression kind/flow/type metadata.
    * Normalize it once BEFORE the immutable inline pair is recorded, using the
    * same resolution passes as in-place banking. Never accept a declaration,
    * body, port, literal or info change hidden behind this preparation step.
    */
  def normalizeMetadata(input: CircuitState): CircuitState = {
    check(input.form == LowForm, "metadata normalization requires resolved LowForm")
    check(!input.annotations.exists(_.isInstanceOf[NativeCheckpointRegionCompilerMetadataAnnotation]),
      "metadata normalization already recorded")
    val working = firrtl.passes.ToWorkingIR.run(input.circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    val original = input.circuit.serialize
    check(flowed.serialize == original, "metadata normalization changed serialized original FIRRTL")
    val changes = input.circuit.modules.zip(flowed.modules).collect { case (before, after) if before != after =>
      JObject(List("module" -> JString(before.name),
        "first_structural_difference" -> JString(NativeCheckpointRegions.firstStructuralDifference(before, after))))
    }
    val receipt = canonical(JObject(List("normalization_version" -> JInt(1),
      "scope" -> JString("compiler_hidden_metadata_resolution_only"),
      "input_firrtl_sha256" -> JString(sha256(original)),
      "output_firrtl_sha256" -> JString(sha256(flowed.serialize)),
      "input_annotations_sha256" -> JString(sha256(JsonProtocol.serialize(input.annotations))),
      "passes" -> JArray(Seq("ToWorkingIR", "ResolveKinds", "InferTypes", "ResolveFlows", "CheckTypes", "CheckFlows")
        .map(JString(_)).toList), "changed_module_metadata" -> JArray(changes.toList),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    input.copy(circuit = flowed, annotations = input.annotations :+ NativeCheckpointRegionCompilerMetadataAnnotation(receipt))
  }
  def requested(as: AnnotationSeq): Boolean = as.exists(_.isInstanceOf[NativeCheckpointRegionCompilerProbeAnnotation])
  private def paths(value: String): Seq[Seq[String]] = {
    val items = value.split(",", -1).toVector
    val result = items.map(_.split("\\.", -1).toVector)
    check(result.nonEmpty && result.size <= 255 && result.forall(p => p.size >= 2 && p.forall(_.matches(identifier))),
      "expected comma-separated concrete root.instance paths")
    check(result.distinct.size == result.size && !result.indices.exists(i => result.indices.exists(j =>
      i != j && result(i).startsWith(result(j)))), "duplicate or overlapping executions")
    result
  }
  def validateCardinality(as: AnnotationSeq): Unit = {
    val requests = as.collect { case a: NativeCheckpointRegionCompilerProbeAnnotation => a }
    val shells = as.collect { case a: NativeCheckpointShellCompilerProbeAnnotation => a }
    check(shells.size <= 1 && (shells.isEmpty || requests.size == 1), "local shell diagnostic requires one regional request")
    shells.foreach(a => shellPaths(a.paths))
    if (requests.isEmpty) return
    check(requests.size == 1, "one explicit request required")
    paths(requests.head.paths)
    check(!as.exists {
      case SimulatorBankProbeAnnotation | NativeFunctionalBankProbeAnnotation | PhysicalClockCutWiringProbeAnnotation |
           SemanticFAMEStateMapAnnotation => true
      case _ => false
    }, "regional diagnostic conflicts with bank, native companion, clock wiring or semantic-map requests")
  }

  private def shellPaths(value: String): Seq[Seq[String]] = {
    val items = value.split(",", -1).toVector.map(_.split("\\.", -1).toVector)
    check(items.nonEmpty && items.size <= 255 && items.forall(p => p.nonEmpty && p.forall(_.matches(identifier))) &&
      items.distinct.size == items.size, "invalid local shell executions")
    items
  }
  private def shellTargets(state: CircuitState, value: String): Seq[firrtl.annotations.IsModule] = {
    shellPaths(value).map { parts =>
      check(parts.head == state.circuit.main, "local shell execution belongs to another circuit")
      if (parts.size == 1) ModuleTarget(state.circuit.main, state.circuit.main)
      else targets(state, parts.mkString(".")).head
    }
  }

  /** Resolve every hop against the actual post-lowering concrete hierarchy. */
  private[checkpoint] def targets(state: CircuitState, value: String): Seq[InstanceTarget] = {
    check(state.form == LowForm, "resolved LowForm required")
    val live = GeneratedExternalOwners.instances(state, state.circuit.main).toMap
    paths(value).map { components =>
      check(components.head == state.circuit.main, "execution belongs to another circuit")
      var current: firrtl.annotations.IsModule = ModuleTarget(state.circuit.main, state.circuit.main)
      components.indices.drop(1).foreach { i =>
        val path = components.take(i + 1).mkString(".")
        val definition = live.getOrElse(path,
          throw new IllegalArgumentException("native region compiler probe: stale concrete execution " + path))
        check(definition.isInstanceOf[Module], "opaque selected execution or ancestor " + path)
        current = current.instOf(components(i), definition.name)
      }
      current.asInstanceOf[InstanceTarget]
    }
  }

  def apply(before: CircuitState, after: CircuitState): CircuitState = {
    validateCardinality(before.annotations)
    if (!requested(before.annotations)) {
      check(!requested(after.annotations), "request appeared after inline")
      return after
    }
    val request = before.annotations.collect { case a: NativeCheckpointRegionCompilerProbeAnnotation => a }.head
    val shellRequest = before.annotations.collect { case a: NativeCheckpointShellCompilerProbeAnnotation => a }
    val dirs = before.annotations.collect { case TargetDirAnnotation(value) => value }
    check(dirs.size == 1 && dirs.head.nonEmpty, "one explicit compiler target directory required")
    val output = Paths.get(dirs.head).resolve(directory)
    Files.createDirectories(output.getParent)
    // Never overwrite an earlier diagnostic, even when the earlier run failed.
    Files.createDirectory(output)
    val hashes = scala.collection.mutable.LinkedHashMap.empty[String, String]
    def save(name: String, body: String): Unit = {
      val path = output.resolve(name)
      check(path.getParent == output && name != "status.json", "invalid diagnostic artifact name")
      Files.write(path, body.getBytes(UTF_8), StandardOpenOption.CREATE_NEW)
      hashes(name) = sha256(body)
    }
    def state(name: String, value: CircuitState): Unit = {
      save(name + ".fir", value.circuit.serialize)
      save(name + ".anno.json", JsonProtocol.serialize(value.annotations))
    }
    def status(stage: String, failure: Option[Throwable] = None): Unit = {
      val body = canonical(JObject(List(
        "probe_version" -> JInt(1), "scope" -> JString("actual_compiler_regional_hookup_diagnostic_only"),
        "status" -> JString(stage), "requested_paths" -> JString(request.paths),
        "requested_local_shells" -> JArray(shellRequest.map(a => JString(a.paths)).toList),
        "pre_inline_sha256" -> JString(sha256(before.circuit.serialize)),
        "post_inline_sha256" -> JString(sha256(after.circuit.serialize)),
        "artifacts" -> JObject(hashes.toList.map { case (name, hash) => name -> JString(hash) }),
        "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
        "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false),
        "physical_fpga_acceptance" -> JBool(false)) ++ failure.toList.flatMap(e => List(
          "error_class" -> JString(e.getClass.getName), "error" -> JString(Option(e.getMessage).getOrElse(""))))))
      val pending = output.resolve("status.pending")
      Files.write(pending, body.getBytes(UTF_8), StandardOpenOption.CREATE_NEW)
      Files.move(pending, output.resolve("status.json"), StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    }
    try {
      state("pre-inline", before); state("post-inline", after)
      before.annotations.collect { case a: NativeCheckpointRegionCompilerMetadataAnnotation => a }.foreach(a =>
        save("metadata-normalization.json", a.body))
      status("phase_snapshots_saved")
      check(after.annotations.collect { case a: NativeCheckpointRegionCompilerProbeAnnotation => a } == Seq(request),
        "request changed across inline")
      check(after.annotations.collect { case a: NativeCheckpointShellCompilerProbeAnnotation => a } == shellRequest,
        "local shell request changed across inline")
      val selected = targets(after, request.paths)
      save("targets.json", canonical(JArray(selected.map(t => JString(t.serialize)).toList)))
      val inventory = ReservedCheckpointInventory.forReservedWidgetSource(before, after)
      state("regional-source", inventory.source)
      save("inventory-permission.json", inventory.binding)
      val localTargets = shellRequest.flatMap(a => shellTargets(after, a.paths))
      val regions = NativeCheckpointRegions.generate(inventory.source, selected, Some(inventory), localTargets)
      state("regional-instrumented", regions.state)
      save("regions.json", regions.binding)
      save("descriptors.json", regions.descriptors.manifest)
      save("descriptor-binding.json", regions.descriptors.binding)
      save("descriptor-host.inc", regions.descriptors.cppManifest)
      save("routing.json", regions.routing.binding)
      regions.regions.zipWithIndex.foreach { case (region, index) =>
        val prefix = s"region-$index"
        state(prefix + "-reference", region.reference)
        state(prefix + "-instrumented", region.state)
        save(prefix + "-context.json", region.context)
        save(prefix + "-context-binding.json", region.binding)
        save(prefix + "-native-binding.json", region.native.binding)
        save(prefix + "-manifest.json", region.manifest)
        save(prefix + "-plan.json", region.plan)
        save(prefix + "-dependencies.json", region.dependencies)
        save(prefix + "-identity.json", region.native.identity.manifest)
        save(prefix + "-identity-binding.json", region.native.identity.binding)
      }
      regions.shells.zipWithIndex.foreach { case (shell, index) =>
        val prefix = s"shell-$index"
        state(prefix + "-reference", shell.reference)
        state(prefix + "-instrumented", shell.state)
        save(prefix + "-binding.json", shell.binding)
        save(prefix + "-mapping.json", shell.mapping)
        save(prefix + "-manifest.json", shell.manifest)
        save(prefix + "-banks-binding.json", shell.banks.binding)
      }
      status("regional_network_reconstructed_incomplete")
      save("transport-freeze-conflicts.json", canonical(NativeCheckpointRegionWidgetGraft.liveTransportConflicts(before, after, regions)))
      val bound = NativeCheckpointWidgetIdentity.bindRegions(before, after, regions)
      state("grafted", bound.connection.graft.state)
      bound.connection.graft.files.foreach(f => save("generated" + f.fileSuffix, f.body))
      state("connected", bound.connection.state)
      save("connection.json", bound.connection.binding)
      save("annotation-projection.json", bound.connection.annotationProjection)
      state("identity-bound", bound.state)
      save("identity-binding.json", bound.binding)
      status("identity_bound_incomplete_image_guard_required")
      bound.state
    } catch {
      case NonFatal(error) =>
        try status("rejected", Some(error)) catch {
          case NonFatal(reportFailure) => error.addSuppressed(reportFailure)
        }
        throw error
    }
  }
}
