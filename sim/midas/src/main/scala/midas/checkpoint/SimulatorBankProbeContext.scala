// See LICENSE for license details.
package midas.checkpoint

import firrtl.annotations.Annotation
import firrtl.options.TargetDirAnnotation
import firrtl.stage.{AllowUnrecognizedAnnotations, InfoModeAnnotation, OutputFileAnnotation}
import chisel3.experimental.EnumAnnotations.EnumDefAnnotation
import midas.stage.{ConfigPackageAnnotation, ConfigStringAnnotation, OutputBaseFilenameAnnotation}
import org.json4s._

/** Exact, inert compiler context at the post-FPGA-lowering bank boundary.
  * These values name parser/config/output choices or enum TYPE definitions;
  * none identifies an instance, schedules a pass or embeds a circuit. Preserve
  * the original annotations for their stage consumers and bind their complete
  * values separately from the storage translation. In particular, accepting
  * AllowUnrecognizedAnnotations here never admits an UnrecognizedAnnotation.
  */
private[checkpoint] object SimulatorBankProbeContext {
  import SemanticStateInventory.{canonical, sha256}
  private def row(a: Annotation): Option[(String, JValue)] = {
    def text(kind: String, value: String) = Some(kind -> JString(value))
    a match {
      case InfoModeAnnotation(value) => text("info_mode", value)
      case OutputFileAnnotation(value) => text("firrtl_output_file", value)
      case TargetDirAnnotation(value) => text("target_directory", value)
      case ConfigPackageAnnotation(value) => text("config_package", value)
      case ConfigStringAnnotation(value) => text("config_string", value)
      case OutputBaseFilenameAnnotation(value) => text("output_base_filename", value)
      case AllowUnrecognizedAnnotations => Some("allow_unrecognized_input_annotations" -> JBool(true))
      case EnumDefAnnotation(name, values) => Some("enum_type" -> JObject(List(
        "name" -> JString(name), "values" -> JObject(values.toList.map { case (k, v) => k -> JInt(v) }))))
      case _ => None
    }
  }
  def accepts(a: Annotation): Boolean = row(a).nonEmpty

  def snapshot(annotations: Seq[Annotation], reference: String, instrumented: String): String = {
    val rows = annotations.map(a => row(a).getOrElse(throw new IllegalArgumentException(
      s"Simulator bank context unsupported: ${a.getClass.getName}")))
    val singleton = rows.filterNot(_._1 == "enum_type").groupBy(_._1)
    require(!singleton.exists(_._2.size != 1), "Simulator bank context: duplicate compiler option")
    val enums = rows.collect { case ("enum_type", value) => value }.groupBy(_ \ "name")
    require(!enums.exists(_._2.distinct.size != 1), "Simulator bank context: conflicting enum type definition")
    canonical(JObject(List(
      "context_version" -> JInt(1), "scope" -> JString("compiler_context_only"),
      "deployable_capture_image" -> JBool(false),
      "reference_sha256" -> JString(sha256(reference)),
      "instrumented_sha256" -> JString(sha256(instrumented)),
      "annotations" -> JArray(rows.map { case (kind, value) =>
        JObject(List("kind" -> JString(kind), "value" -> value))
      }.sortBy(canonical).toList))))
  }
}
