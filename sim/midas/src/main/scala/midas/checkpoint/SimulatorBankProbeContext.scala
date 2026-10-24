// See LICENSE for license details.
package midas.checkpoint

import firrtl.annotations.{Annotation, DeletedAnnotation}
import firrtl.options.{InputAnnotationFileAnnotation, TargetDirAnnotation}
import firrtl.stage.{AllowUnrecognizedAnnotations, FirrtlFileAnnotation, InfoModeAnnotation, OutputFileAnnotation}
import chisel3.experimental.EnumAnnotations.EnumDefAnnotation
import midas.passes.ChannelClockInfoAnnotation
import midas.stage.{ConfigPackageAnnotation, ConfigStringAnnotation, OutputBaseFilenameAnnotation}
import org.json4s._

/** Exact, inert compiler context at the post-FPGA-lowering bank boundary.
  * These values name parser/config/output choices, enum TYPE definitions,
  * consumed input-file events or global channel clock-frequency metadata;
  * none identifies an instance, schedules a pass or embeds a circuit. Preserve
  * the original annotations for their stage consumers and bind their complete
  * values separately from the storage translation. In particular, accepting
  * AllowUnrecognizedAnnotations here never admits an UnrecognizedAnnotation.
  */
private[checkpoint] object SimulatorBankProbeContext {
  import SemanticStateInventory.{canonical, sha256}
  private def row(a: Annotation): Option[(String, JValue)] = {
    def text(kind: String, value: String) = Some(kind -> JString(value))
    def deletedInput(producer: String, nestedClass: String, file: String) = {
      require(file.nonEmpty && !file.contains('\u0000'), "Simulator bank context: invalid deleted input filename")
      Some("deleted_input" -> JObject(List("producer" -> JString(producer),
        "annotation_class" -> JString(nestedClass), "file" -> JString(file))))
    }
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
      // These exact stage producers already consumed the named input. A
      // DeletedAnnotation is NOT a generic exemption: unknown/deleted owners,
      // pass requests, nested deletions and mismatched producers still reject.
      // Keep the event literal; never reopen an old input or reapply it to the
      // specialized hierarchy.
      case DeletedAnnotation("firrtl.stage.phases.AddCircuit", nested: FirrtlFileAnnotation) =>
        deletedInput("firrtl.stage.phases.AddCircuit", classOf[FirrtlFileAnnotation].getName, nested.file)
      case DeletedAnnotation("firrtl.options.phases.GetIncludes", nested: InputAnnotationFileAnnotation) =>
        deletedInput("firrtl.options.phases.GetIncludes", classOf[InputAnnotationFileAnnotation].getName, nested.file)
      // Keys are FAME GLOBAL channel names, not FIRRTL hierarchy references.
      // UpdateBridgeClockInfo consumes these frequencies before SimulationMapping;
      // banking must retain every label and unreduced ratio, not reinterpret it
      // as a physical clock or proof of common-cut/clock-owner coverage.
      case ChannelClockInfoAnnotation(info) =>
        require(info.nonEmpty && info.forall { case (channel, clock) =>
          channel.nonEmpty && clock.name.nonEmpty && clock.multiplier > 0 && clock.divisor > 0
        }, "Simulator bank context: invalid channel clock frequency metadata")
        Some("channel_clock_info" -> JObject(info.toList.map { case (channel, clock) =>
          channel -> JObject(List("name" -> JString(clock.name),
            "multiplier" -> JInt(clock.multiplier), "divisor" -> JInt(clock.divisor)))
        }))
      case _ => None
    }
  }
  def accepts(a: Annotation): Boolean = row(a).nonEmpty

  def snapshot(annotations: Seq[Annotation], reference: String, instrumented: String): String = {
    val rows = annotations.map(a => row(a).getOrElse(throw new IllegalArgumentException(
      s"Simulator bank context unsupported: ${a.getClass.getName}")))
    val singleton = rows.filterNot(r => Set("enum_type", "deleted_input")(r._1)).groupBy(_._1)
    require(!singleton.exists(_._2.size != 1), "Simulator bank context: duplicate compiler option")
    val deleted = rows.filter(_._1 == "deleted_input")
    require(deleted.distinct.size == deleted.size, "Simulator bank context: duplicate deleted input event")
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
