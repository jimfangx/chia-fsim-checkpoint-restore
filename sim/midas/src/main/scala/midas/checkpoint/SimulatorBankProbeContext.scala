// See LICENSE for license details.
package midas.checkpoint

import firrtl.annotations.{Annotation, DeletedAnnotation}
import firrtl.options.{InputAnnotationFileAnnotation, TargetDirAnnotation}
import firrtl.stage.{AllowUnrecognizedAnnotations, FirrtlFileAnnotation, InfoModeAnnotation, OutputFileAnnotation}
import chisel3.experimental.EnumAnnotations.EnumDefAnnotation
import midas.passes.{ChannelClockInfoAnnotation, PreLinkRenamingResultAnnotation, BridgeExtractionResultAnnotation}
import midas.stage.{ConfigPackageAnnotation, ConfigStringAnnotation, OutputBaseFilenameAnnotation}
import org.json4s._

/** Exact, inert compiler context at the post-FPGA-lowering bank boundary.
  * These values name parser/config/output choices, enum TYPE definitions,
  * consumed input-file/pre-link events or global channel clock-frequency metadata;
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
      // Only the immutable result of the actual pass is inert history. The
      // Namespace request itself (including a wrapped/deleted request) remains
      // unsupported. These names are historical identities, not rename targets.
      case a: PreLinkRenamingResultAnnotation =>
        val pairs = a.moduleRenames
        def name(s: String): Boolean = s.matches("[A-Za-z_][A-Za-z0-9_$]*")
        require(name(a.circuit) && Seq(a.inputSHA256, a.outputSHA256).forall(_.matches("[0-9a-f]{64}")) &&
          pairs.nonEmpty && pairs.forall { case (from, to) => name(from) && name(to) } &&
          pairs.map(_._1).distinct.size == pairs.size && pairs.map(_._2).distinct.size == pairs.size &&
          pairs.contains(a.circuit -> a.circuit), "Simulator bank context: invalid consumed pre-link history")
        Some("pre_link_renaming" -> JObject(List(
          "producer" -> JString("midas.passes.PreLinkRenaming"), "circuit" -> JString(a.circuit),
          "input_sha256" -> JString(a.inputSHA256), "output_sha256" -> JString(a.outputSHA256),
          "module_renames" -> JArray(pairs.map { case (from, to) => JObject(List(
            "from" -> JString(from), "to" -> JString(to))) }.toList))))
      // Bridge extraction has removed this temporary wrapper and promoted
      // instances. Preserve their historical identities, never remap them to
      // the current simulator or infer that they cover physical bridge state.
      case a: BridgeExtractionResultAnnotation =>
        def name(s: String): Boolean = s.matches("[A-Za-z_][A-Za-z0-9_$]*")
        val instances = a.bridgeInstances
        require(name(a.circuit) && name(a.wrapper) && a.circuit != a.wrapper &&
          Seq(a.inputSHA256, a.promotedSHA256, a.outputSHA256, a.connectivitySHA256)
            .forall(_.matches("[0-9a-f]{64}")) && instances.nonEmpty &&
          instances.distinct.size == instances.size && instances.forall { value =>
            firrtl.annotations.Target.deserialize(value) match {
              case t: firrtl.annotations.InstanceTarget => t.circuit == a.wrapper &&
                t.module == a.wrapper && t.path.isEmpty && name(t.instance) && name(t.ofModule)
              case _ => false
            }
          }, "Simulator bank context: invalid consumed bridge extraction history")
        Some("bridge_extraction" -> JObject(List(
          "producer" -> JString("midas.passes.BridgeExtraction"),
          "circuit" -> JString(a.circuit), "wrapper" -> JString(a.wrapper),
          "input_sha256" -> JString(a.inputSHA256), "promoted_sha256" -> JString(a.promotedSHA256),
          "output_sha256" -> JString(a.outputSHA256), "connectivity_sha256" -> JString(a.connectivitySHA256),
          "bridge_instances" -> JArray(instances.map(JString(_)).toList))))
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
    val singleton = rows.filterNot(r => Set("enum_type", "deleted_input", "pre_link_renaming", "bridge_extraction")(r._1)).groupBy(_._1)
    require(!singleton.exists(_._2.size != 1), "Simulator bank context: duplicate compiler option")
    val deleted = rows.filter(_._1 == "deleted_input")
    require(deleted.distinct.size == deleted.size, "Simulator bank context: duplicate deleted input event")
    val prelinks = rows.filter(_._1 == "pre_link_renaming")
    require(prelinks.distinct.size == prelinks.size, "Simulator bank context: duplicate consumed pre-link history")
    val extractions = rows.filter(_._1 == "bridge_extraction")
    require(extractions.size <= 1, "Simulator bank context: duplicate consumed bridge extraction history")
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
