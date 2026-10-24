// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._

case object NativeFunctionalBankProbeAnnotation extends NoTargetAnnotation with Unserializable with HasShellOptions {
  val options = Seq(new ShellOption[Unit](
    longOption = "simulator-native-functional-bank-probe",
    toAnnotationSeq = (_: Unit) => Seq(NativeFunctionalBankProbeAnnotation),
    helpText = "Emit separate functional readout banks retaining native BUFGCEs; not a deployable capture image.",
  ))
}

/** Separate sibling only. Native clock gates are never modeled as registers or
  * replaced by transparent emitted bodies. This does not wire the physical cut,
  * CE bank, transport or external owners into the generated functional banks.
  */
object NativeFunctionalBankProbe {
  import SemanticStateInventory.{canonical, sha256}
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Native functional bank probe unsupported: $why")

  def validateRequest(state: CircuitState): Unit = {
    val requests = state.annotations.count(_ == NativeFunctionalBankProbeAnnotation)
    if (requests == 0) return
    if (requests != 1 || state.annotations.count(_ == PhysicalClockCutPlanAnnotation) != 1 ||
        state.annotations.collect { case a: SimulatorCheckpointInventoryAnnotation => a.rootModule } != Seq(state.circuit.main))
      fail("one native probe, clock plan and complete root inventory required")
    if (state.annotations.contains(SimulatorBankProbeAnnotation) || state.annotations.contains(SemanticFAMEStateMapAnnotation))
      fail("native companion cannot admit ordinary bank capture or semantic mapping")
  }

  def apply(input: CircuitState): CircuitState = {
    validateRequest(input)
    if (!input.annotations.contains(NativeFunctionalBankProbeAnnotation)) return input
    val owners = input.annotations.filterNot {
      case NativeFunctionalBankProbeAnnotation | PhysicalClockCutPlanAnnotation | PhysicalClockCutWiringProbeAnnotation |
           SimulatorFPGASynthesisConstantsAnnotation | _: SimulatorCheckpointInventoryAnnotation |
           _: midas.stage.phases.ConfigParametersAnnotation | _: firrtl.EmitCircuitAnnotation |
           _: firrtl.transforms.BlackBoxTargetDirAnno | _: midas.targetutils.xdc.XDCPathToCircuitAnnotation => true
      case a: GoldenGateOutputFileAnnotation if SimulatorFPGASynthesisConstants.isProvenanceFile(a) => true
      case a if SimulatorBankProbeContext.accepts(a) => true
      case _ => false
    }
    val reference = input.copy(annotations = owners)
    val manifest = SimulatorStateInventory.discover(reference)
    val plan = PhysicalClockCutPlan.discover(reference, manifest)
    val deps = PhysicalClockCutDependencies.discover(reference, manifest, plan)
    val inventory = GeneratedStateBankAccess.physicalFunctionalInventory(reference, manifest, plan, deps)
    val generated = GeneratedStateBankAccess.generatePhysicalCompanion(reference, manifest, plan, deps)
    GeneratedStateBankAccess.validateAnnotationBinding(generated)
    // Consume physical cell/pin constraints through their real downstream
    // consumer before FIRRTL's emitter optimizations. InternalXDCAnnotation's
    // DontTouchAllTargets may name native instances, which DCE cannot treat as
    // ground signal vertices. Keep and emit both translated XDC files.
    val mappings = input.annotations.collect { case a: midas.targetutils.xdc.XDCPathToCircuitAnnotation => a }
    if (mappings.size != 1) fail("exactly one physical-constraint circuit path mapping required")
    val constrained = midas.passes.WriteXDCFile.execute(generated.state.copy(
      annotations = generated.state.annotations ++ mappings))
    val emitter = new SemanticCleanEmitter
    val emitted = new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(constrained.copy(annotations = constrained.annotations :+
        firrtl.EmitCircuitAnnotation(emitter.getClass)))
    val rtl = emitted.getEmittedCircuit.value
    val files = Seq(
      ".native-functional-reference.fir" -> reference.circuit.serialize,
      ".native-functional-simulator-state.json" -> manifest,
      ".native-functional-plan.json" -> plan,
      ".native-functional-dependencies.json" -> deps,
      ".native-functional-inventory.json" -> inventory,
      ".native-functional-instrumented.fir" -> generated.state.circuit.serialize,
      ".native-functional-binding.json" -> generated.binding,
      ".native-functional.sv" -> rtl) ++ constrained.annotations.collect {
        case a: midas.passes.XDCOutputAnnotation => (".native-functional" + a.suffix.get) -> a.fileBody
      }
    if (input.annotations.exists {
      case a: GoldenGateOutputFileAnnotation => a.fileSuffix.startsWith(".native-functional")
      case _ => false
    }) fail("existing native functional output")
    val binding = canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString("native_functional_readout_companion_only"),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false),
      "inputs" -> JObject(files.map { case (name, body) => name -> JString(sha256(body)) }.toList))))
    input.copy(annotations = input.annotations ++ files.map { case (name, body) =>
      GoldenGateOutputFileAnnotation(body, name)
    } :+ GoldenGateOutputFileAnnotation(binding, ".native-functional-emission.json"))
  }
}
