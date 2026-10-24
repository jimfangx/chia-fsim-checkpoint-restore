// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import firrtl.annotations.{JsonProtocol, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import firrtl.transforms.BlackBoxInlineAnno
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._

/** Explicitly select the exact plusarg model's FPGA branch. Ordinary metasim
  * compilation must retain its init-only myplus owners and runtime plusargs.
  */
case object SimulatorFPGASynthesisConstantsAnnotation
    extends NoTargetAnnotation with Unserializable with HasShellOptions {
  val options = Seq(new ShellOption[Unit](
    longOption = "simulator-fpga-synthesis-constants",
    toAnnotationSeq = (_: Unit) => Seq(SimulatorFPGASynthesisConstantsAnnotation),
    helpText = "Select source-checked FPGA plusarg constants and emit their complete provenance; requires whole-root simulator inventory.",
  ))
}

object SimulatorFPGASynthesisConstants {
  import SemanticStateInventory.{canonical, sha256}
  private val prefix = ".fpga-constants-"
  private val provenanceNames = Seq("original.fir", "prepared.fir", "sources.json", "preparation.json").map(prefix + _)
  def isProvenanceFile(a: GoldenGateOutputFileAnnotation): Boolean = provenanceNames.contains(a.fileSuffix)
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Simulator FPGA constants unsupported: $why")

  // Run before any compiler can canonicalize duplicated CLI annotations. The
  // selected generated root does not exist until after Golden Gate mapping.
  def validateCardinality(annotations: firrtl.AnnotationSeq): Unit = {
    val count = annotations.count(_ == SimulatorFPGASynthesisConstantsAnnotation)
    if (count == 0) return
    if (count != 1) fail("exactly one synthesis-constant request is required")
    if (annotations.count(_.isInstanceOf[SimulatorCheckpointInventoryAnnotation]) != 1)
      fail("exactly one complete circuit-root inventory is required")
    if (annotations.contains(SemanticFAMEStateMapAnnotation))
      fail("semantic tracking requires an explicit FPGA-constant mapping contract")
  }

  def validateRequest(state: CircuitState): Unit = {
    validateCardinality(state.annotations)
    if (!state.annotations.contains(SimulatorFPGASynthesisConstantsAnnotation)) return
    val roots = state.annotations.collect { case a: SimulatorCheckpointInventoryAnnotation => a.rootModule }
    if (roots != Seq(state.circuit.main)) fail("exactly one complete circuit-root inventory is required")
  }

  def apply(state: CircuitState): CircuitState = {
    validateRequest(state)
    if (!state.annotations.contains(SimulatorFPGASynthesisConstantsAnnotation)) return state
    if (state.annotations.exists {
      case a: GoldenGateOutputFileAnnotation => a.fileSuffix.startsWith(prefix)
      case _ => false
    }) fail("existing synthesis-constant provenance")
    val prepared = GeneratedSynthesisConstantAdapter.prepare(state)
    val constants = prepared.annotations.collect { case a: SynthesisPlusArgConstantAnnotation => a }.sortBy(_.target.module)
    if (constants.isEmpty) fail("no exact plusarg model to specialize")
    val original = state.circuit.modules.map(m => m.name -> m).toMap
    val instances = GeneratedExternalOwners.instances(state, state.circuit.main)
    val sources = state.annotations.collect { case a: BlackBoxInlineAnno if
      constants.exists(_.target.module == a.target.name) => a }.sortBy(_.target.name)
    val sourceJSON = JsonProtocol.serialize(sources)
    val rows = constants.map { a => JObject(List(
      "module" -> JString(a.target.module), "source_sha256" -> JString(a.sourceSHA256),
      "original_definition_sha256" -> JString(sha256(original(a.target.module).serialize)),
      "width" -> JInt(a.width), "default" -> JInt(a.default), "format" -> JString(a.format),
      "reachable_instances" -> JArray(instances.collect { case (path, m) if m.name == a.target.module =>
        path }.sorted.map(JString(_)).toList))) }
    val report = canonical(JObject(List(
      "preparation_version" -> JInt(1), "scope" -> JString("source_checked_fpga_plusarg_constants_only"),
      "root_module" -> JString(state.circuit.main), "complete_fpga_capture" -> JBool(false),
      "deployable_capture_image" -> JBool(false), "external_runtime_required" -> JBool(true),
      "original_reference_sha256" -> JString(sha256(state.circuit.serialize)),
      "prepared_reference_sha256" -> JString(sha256(prepared.circuit.serialize)),
      "inline_sources_sha256" -> JString(sha256(sourceJSON)),
      "constants" -> JArray(rows.toList))))
    prepared.copy(annotations = prepared.annotations ++ Seq(
      GoldenGateOutputFileAnnotation(state.circuit.serialize, prefix + "original.fir"),
      GoldenGateOutputFileAnnotation(prepared.circuit.serialize, prefix + "prepared.fir"),
      GoldenGateOutputFileAnnotation(sourceJSON, prefix + "sources.json"),
      GoldenGateOutputFileAnnotation(report, prefix + "preparation.json")))
  }

  def emissionBinding(state: CircuitState, verilog: String): Seq[GoldenGateOutputFileAnnotation] = {
    if (!state.annotations.contains(SimulatorFPGASynthesisConstantsAnnotation)) return Nil
    val files = state.annotations.collect { case a: GoldenGateOutputFileAnnotation => a.fileSuffix -> a.body }
    val hashes = provenanceNames.map { name =>
      val matches = files.filter(_._1 == name)
      if (matches.size != 1) fail(s"missing or duplicate provenance $name")
      name -> JString(sha256(matches.head._2))
    }
    Seq(GoldenGateOutputFileAnnotation(canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString("source_checked_fpga_plusarg_constants_only"),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "emitted_verilog_sha256" -> JString(sha256(verilog)),
      "inputs" -> JObject(hashes.toList)))), prefix + "emission.json"))
  }
}
