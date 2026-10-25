// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import midas.InternalXDCAnnotation
import midas.passes.{WriteXDCFile, XDCOutputAnnotation}
import midas.stage.GoldenGateOutputFileAnnotation
import midas.targetutils.xdc.{XDCAnnotation, XDCPathToCircuitAnnotation}
import org.json4s._

/** Place the original constraints under the state-access wrapper's actual
  * `network` instance. Re-emit from validated annotations, never replace path
  * substrings in Tcl (which would also rewrite unrelated shell constraints).
  * This binds companion placement only; production attachment is not implied.
  */
object NativeCheckpointConstraints {
  import SemanticStateInventory.{canonical, sha256}

  def emit(wired: CircuitState, wrapper: String, placement: XDCPathToCircuitAnnotation):
      Seq[GoldenGateOutputFileAnnotation] = {
    require(wrapper == wired.circuit.main + "CheckpointStateAccess",
      "native constraints: changed state-access wrapper identity")
    def nested(path: Option[String]): Option[String] = {
      path.foreach(p => require(p.split("/", -1).forall(_.matches("[A-Za-z_][A-Za-z0-9_]*")),
        "native constraints: placement requires literal nonempty hierarchy segments"))
      Some((path.toSeq :+ "network").mkString("/"))
    }
    val rebased = XDCPathToCircuitAnnotation(nested(placement.preLinkPath), nested(placement.postLinkPath))
    // Check every target against the bank-specialized, clock-wired circuit
    // before WriteXDCFile can silently expand an unreachable module to nothing.
    GeneratedStateBankAnnotations.preflight(wired)
    val emitted = WriteXDCFile.execute(wired.copy(annotations = wired.annotations :+ rebased))
    val specs = wired.annotations.collect {
      case a: InternalXDCAnnotation => a
      case a: XDCAnnotation => InternalXDCAnnotation(a)
    }.map(a => JObject(List(
      "destination" -> JString(a.destinationFile.fileSuffix), "format" -> JString(a.formatString),
      "arguments" -> JArray(a.argumentList.map(t => JString(t.serialize)).toList))))
    val files = emitted.annotations.collect { case a: XDCOutputAnnotation =>
      GoldenGateOutputFileAnnotation(a.fileBody, ".native-state-access" + a.suffix.get)
    }
    require(files.size == 2, "native constraints: both synthesis and implementation outputs required")
    def path(p: Option[String]): JValue = p.map(JString(_)).getOrElse(JNull)
    val binding = canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString("native_state_access_constraint_placement_only"),
      "wrapper_module" -> JString(wrapper), "network_instance" -> JString("network"),
      "network_module" -> JString(wired.circuit.main),
      "wired_reference_sha256" -> JString(sha256(wired.circuit.serialize)),
      "translated_annotations_sha256" -> JString(GeneratedStateBankAnnotations.translatedHash(wired)),
      "constraint_specs" -> JArray(specs.toList),
      "original_pre_link_path" -> path(placement.preLinkPath),
      "original_post_link_path" -> path(placement.postLinkPath),
      "rebased_pre_link_path" -> path(rebased.preLinkPath),
      "rebased_post_link_path" -> path(rebased.postLinkPath),
      "production_constraints_attached" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false),
      "inputs" -> JObject(files.map(a => a.fileSuffix -> JString(sha256(a.body))).toList))))
    files :+ GoldenGateOutputFileAnnotation(binding, ".native-state-access-constraints.json")
  }
}
