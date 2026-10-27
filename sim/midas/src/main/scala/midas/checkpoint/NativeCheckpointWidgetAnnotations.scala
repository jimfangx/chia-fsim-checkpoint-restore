// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import firrtl.annotations.{CircuitName, JsonProtocol}
import firrtl.ir.Module
import org.json4s._

/** Project typed storage constraints against the COMPLETE enclosing host.
  * Concrete instance paths correlate shared definitions with their banked
  * replacements. In particular, native primitive constraints must remain on
  * original executions and must not broaden to the new clock mask gate.
  * Reservation, inline history, transport sources and compiler context are
  * retained verbatim. They are not storage contracts or capture owners.
  */
private[checkpoint] object NativeCheckpointWidgetAnnotations {
  import SemanticStateInventory.{canonical, sha256}
  case class Result(state: CircuitState, binding: String)
  private def json(as: firrtl.AnnotationSeq): String = JsonProtocol.serialize(as)

  /** The selected source has already been independently reconstructed by the
    * in-place graft. Carry its translated contracts into the enclosing circuit
    * using FIRRTL's typed circuit rename, never text substitution. Native pin
    * constraints were expanded over the original source executions before the
    * mask gate was inserted, so they cannot acquire that new execution here.
    */
  def mergeSource(source: CircuitState, graft: CircuitState, host: CircuitState): Result = {
    require(source.annotations.count(_ == firrtl.transforms.NoCircuitDedupAnnotation) == 1 &&
      source.annotations.forall(GeneratedStateBankAnnotations.supported),
      "checkpoint widget source annotations: explicit typed NoCircuitDedup policy required")
    GeneratedStateBankAnnotations.preflight(source)
    require(graft.annotations.forall(GeneratedStateBankAnnotations.supported),
      "checkpoint widget source annotations: unsupported generated policy")
    val renames = firrtl.RenameMap(Map(CircuitName(source.circuit.main) -> Seq(CircuitName(host.circuit.main))))
    val rebased = graft.annotations.flatMap(_.update(renames))
    val live = GeneratedExternalOwners.instances(host, host.circuit.main)
    val selected = live.filter(_._2.name == source.circuit.main).map(_._1)
    require(selected.size == 1, "checkpoint widget source annotations: ambiguous selected execution")
    rebased.foreach { a =>
      // DedupedResult.original is immutable history, not a current execution.
      val targets = a match {
        case h: firrtl.transforms.DedupedResult => h.duplicate.toSeq
        case _ => a.getTargets
      }
      targets.foreach { t =>
        val module = "^[~][^|]+[|]([^/>]+)".r.findFirstMatchIn(t.serialize).map(_.group(1))
        require(module.nonEmpty && live.filter(_._2.name == module.get).forall { case (path, _) =>
          path == selected.head || path.startsWith(selected.head + ".")
        }, "checkpoint widget source annotations: source contract would broaden outside selected execution")
      }
    }
    val result = host.copy(annotations = host.annotations ++ rebased.distinct.filterNot(host.annotations.contains))
    val storage = result.copy(annotations = result.annotations.filter(GeneratedStateBankAnnotations.supported))
    val clones = GeneratedExternalOwners.instances(result, result.circuit.main).collect { case (path, m: Module) =>
      GeneratedStateBankAnnotations.Clone(m.name, path, m.name, m)
    }
    // Re-run the real storage validator over the union: two individually valid
    // policies can conflict (global RUW/init policy or overlapping RAM files).
    val (validated, _) = GeneratedStateBankAnnotations.translate(storage, clones)
    require(validated.toSet == storage.annotations.toSet,
      "checkpoint widget source annotations: merged targets changed during validation")
    require(rebased.filter(_.getTargets.isEmpty).forall(host.annotations.contains),
      "checkpoint widget source annotations: global source policy absent from enclosing host")
    val binding = canonical(JObject(List(
      "source_policy_version" -> JInt(1), "scope" -> JString("typed_selected_source_contract_merge_only"),
      "source_sha256" -> JString(sha256(source.circuit.serialize)),
      "graft_sha256" -> JString(sha256(graft.circuit.serialize)),
      "connected_sha256" -> JString(sha256(host.circuit.serialize)),
      "source_annotations_sha256" -> JString(sha256(json(source.annotations))),
      "graft_annotations_sha256" -> JString(sha256(json(graft.annotations))),
      "host_annotations_sha256" -> JString(sha256(json(host.annotations))),
      "rebased_annotations_sha256" -> JString(sha256(json(rebased))),
      "merged_annotations_sha256" -> JString(sha256(json(result.annotations))),
      "rebased_annotations" -> JArray(rebased.map(a => JString(a.serialize)).toList),
      "complete_fpga_capture" -> JBool(false), "production_annotations_translated" -> JBool(false),
      "deployable_capture_image" -> JBool(false), "production_freeze_owners_bound" -> JBool(false))))
    Result(result, binding)
  }

  def project(before: CircuitState, after: CircuitState): Result = {
    require(before.circuit.main == after.circuit.main && before.form == after.form,
      "checkpoint widget annotations: changed host root or form")
    val original = GeneratedExternalOwners.instances(before, before.circuit.main)
    val emitted = GeneratedExternalOwners.instances(after, after.circuit.main).toMap
    val clones = original.collect { case (path, m: Module) =>
      val next = emitted.getOrElse(path, throw new IllegalArgumentException(
        "checkpoint widget annotations: lost original execution " + path))
      require(next.isInstanceOf[Module], "checkpoint widget annotations: changed concrete owner " + path)
      GeneratedStateBankAnnotations.Clone(m.name, path, next.name, m)
    }
    val (storage, retained) = before.annotations.partition(GeneratedStateBankAnnotations.supported)
    val (projected, translation) = GeneratedStateBankAnnotations.translate(before.copy(annotations = storage), clones)
    val result = after.copy(annotations = projected ++ retained)
    // Validate the emitted declarations with identity clones. Validation may
    // normalize target spelling, but must never remove or broaden a contract.
    val identity = emitted.toSeq.collect { case (path, m: Module) =>
      GeneratedStateBankAnnotations.Clone(m.name, path, m.name, m)
    }
    val (validated, _) = GeneratedStateBankAnnotations.translate(result.copy(annotations = projected), identity)
    require(validated.toSet == projected.toSet,
      "checkpoint widget annotations: emitted targets changed during validation")
    val binding = canonical(JObject(List(
      "projection_version" -> JInt(1), "scope" -> JString("enclosing_host_typed_storage_annotations_only"),
      "source_sha256" -> JString(sha256(before.circuit.serialize)),
      "connected_sha256" -> JString(sha256(after.circuit.serialize)),
      "source_annotations_sha256" -> JString(sha256(json(before.annotations))),
      "projected_annotations_sha256" -> JString(sha256(json(result.annotations))),
      "retained_annotations_sha256" -> JString(sha256(json(retained))),
      "translation" -> translation,
      "invalidated_path_caches" -> JArray(storage.collect {
        case a: firrtl.transforms.CombinationalPath => JString(a.serialize)
      }.toList),
      "complete_fpga_capture" -> JBool(false), "production_annotations_translated" -> JBool(false),
      "deployable_capture_image" -> JBool(false), "production_freeze_owners_bound" -> JBool(false))))
    Result(result, binding)
  }
}
