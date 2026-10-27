// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import firrtl.annotations.JsonProtocol
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
