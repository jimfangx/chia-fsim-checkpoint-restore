// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._

/** Diagnostic whole-root emission only. The external bank controls are NOT a
  * production MMIO binding or an all-owner cut. In particular, applying this
  * around a live capture engine would freeze its own transport.
  */
case object SimulatorBankProbeAnnotation extends NoTargetAnnotation with Unserializable with HasShellOptions {
  val options = Seq(new ShellOption[Unit](
    longOption = "simulator-bank-probe",
    toAnnotationSeq = (_: Unit) => Seq(SimulatorBankProbeAnnotation),
    helpText = "Emit diagnostic generated-state bank ports after FPGA lowering; not a deployable capture image.",
  ))
}

object SimulatorBankProbe {
  import SemanticStateInventory.{canonical, sha256}
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Simulator bank probe unsupported: $why")

  /** Only known compiler bookkeeping may bypass the storage-annotation
    * translator. Unknown annotations, including NoTargetAnnotations with hidden
    * hierarchy references, must fail rather than silently lose their contract.
    */
  private def bookkeeping(a: firrtl.annotations.Annotation): Boolean = a match {
    case SimulatorBankProbeAnnotation | _: SimulatorCheckpointInventoryAnnotation |
         _: midas.stage.phases.ConfigParametersAnnotation |
         _: firrtl.EmitCircuitAnnotation |
         _: midas.targetutils.xdc.XDCPathToCircuitAnnotation => true
    case _ => false
  }

  def validateRequest(input: CircuitState): Unit = {
    val requests = input.annotations.count(_ == SimulatorBankProbeAnnotation)
    if (requests == 0) return
    if (requests != 1) fail("exactly one bank probe request is required")
    if (input.annotations.collect { case a: SimulatorCheckpointInventoryAnnotation => a.rootModule }
        .exists(_ != input.circuit.main)) fail("inventory must select the complete circuit root")
    if (input.annotations.contains(SemanticFAMEStateMapAnnotation))
      fail("semantic tracking annotations need an explicit specialization contract")
  }

  def apply(input: CircuitState): CircuitState = {
    validateRequest(input)
    if (!input.annotations.contains(SimulatorBankProbeAnnotation)) return input
    val (metadata, storage) = input.annotations.partition(bookkeeping)
    val reference = input.copy(annotations = storage)
    val manifest = SimulatorStateInventory.discover(reference)
    // generate() preflights every remaining annotation and translates all
    // supported targets. No user/physical constraint is dropped here.
    val generated = GeneratedStateBankAccess.generate(reference, manifest, separateAccessClock = true)
    val descriptors = midas.widgets.CheckpointCaptureDescriptors.fromGenerated(generated, manifest)
    val descriptorJSON = canonical(JObject(List(
      "descriptor_version" -> JInt(1), "scope" -> JString("generated_rtl_probe_only"),
      "external_runtime_required" -> JBool(true), "deployable_capture_image" -> JBool(false),
      "manifest_sha256" -> JString(descriptors.manifestSha256),
      "bank_binding_sha256" -> JString(descriptors.bindingSha256),
      "state" -> JArray(descriptors.entries.map(e => JObject(List(
        "state_id" -> JString(e.stateID), "bank" -> JInt(e.bank), "slot" -> JInt(e.slot),
        "width" -> JInt(e.width), "depth" -> JInt(e.depth)))).toList))))
    generated.state.copy(annotations = generated.state.annotations ++ metadata ++ Seq(
      GoldenGateOutputFileAnnotation(reference.circuit.serialize, ".bank-probe-reference.fir"),
      GoldenGateOutputFileAnnotation(manifest, ".bank-probe-state.json"),
      GoldenGateOutputFileAnnotation(generated.state.circuit.serialize, ".bank-probe-instrumented.fir"),
      GoldenGateOutputFileAnnotation(generated.binding, ".bank-probe-binding.json"),
      GoldenGateOutputFileAnnotation(descriptorJSON, ".bank-probe-descriptors.json")))
  }

  def emissionBinding(state: CircuitState, verilog: String): Seq[GoldenGateOutputFileAnnotation] = {
    if (!state.annotations.contains(SimulatorBankProbeAnnotation)) return Nil
    val files = state.annotations.collect { case a: GoldenGateOutputFileAnnotation => a.fileSuffix -> a.body }.toMap
    val names = Seq(".bank-probe-reference.fir", ".bank-probe-state.json", ".bank-probe-instrumented.fir",
      ".bank-probe-binding.json", ".bank-probe-descriptors.json")
    val hashes = names.map { n =>
      val body = files.getOrElse(n, fail(s"missing generated artifact $n"))
      n -> JString(sha256(body))
    }
    val binding = canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString("generated_rtl_probe_only"),
      "external_runtime_required" -> JBool(true), "deployable_capture_image" -> JBool(false),
      "emitted_verilog_sha256" -> JString(sha256(verilog)),
      "inputs" -> JObject(hashes.toList))))
    Seq(GoldenGateOutputFileAnnotation(binding, ".bank-probe-emission.json"))
  }
}
