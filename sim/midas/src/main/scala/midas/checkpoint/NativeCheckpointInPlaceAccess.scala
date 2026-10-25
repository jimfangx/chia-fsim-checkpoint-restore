// See LICENSE for license details.
package midas.checkpoint

import firrtl.annotations.Target
import firrtl.ir.Module
import midas.stage.GoldenGateOutputFileAnnotation
import midas.targetutils.xdc.XDCPathToCircuitAnnotation
import midas.widgets.CheckpointCaptureDescriptors
import org.json4s._
import org.json4s.native.JsonMethods.parse
import scala.io.Source

/** Attach functional/physical readout to the ONE existing host execution.
  * The host's ancestor execution, router and both endpoints remain raw-clocked.
  * Captured CE/read permission must come from the common low-phase controller;
  * this attachment never invents acknowledgements for missing owners.
  *
  * This is a bounded readout connection, not complete simulator capture or a
  * restore adapter. The global descriptor manifest retains all coverage holes.
  */
object NativeCheckpointInPlaceAccess {
  import SemanticStateInventory.{canonical, sha256}

  /** Reconstruct global descriptors without exposing internal context policy. */
  def descriptors(result: NativeCheckpointInPlaceContext.Result): CheckpointCaptureDescriptors = {
    NativeCheckpointInPlaceContext.validate(result, result.state)
    val target = Target.deserialize((parse(result.binding) \ "target").asInstanceOf[JString].s) match {
      case t: firrtl.annotations.InstanceTarget => t
      case _ => throw new IllegalArgumentException("native in-place access: concrete target required")
    }
    CheckpointCaptureDescriptors.fromNativeInPlace(
      result.host.copy(annotations = result.host.annotations.filterNot(SimulatorBankProbeContext.accepts)),
      target, result.reference, result.manifest, result.plan, result.dependencies)
  }

  def emit(result: NativeCheckpointInPlaceContext.Result,
      placement: XDCPathToCircuitAnnotation = XDCPathToCircuitAnnotation(None, None)):
      Seq[GoldenGateOutputFileAnnotation] = {
    NativeCheckpointInPlaceContext.validate(result, result.state)
    val target = Target.deserialize((parse(result.binding) \ "target").asInstanceOf[JString].s) match {
      case t: firrtl.annotations.InstanceTarget => t
      case _ => throw new IllegalArgumentException("native in-place access: concrete target required")
    }
    // Context validation alone is not a structural proof. Reconstruct from the
    // original source so coherently rehashed circuit/identity changes reject.
    val rebuilt = NativeCheckpointInPlaceContext.generate(result.host, target)
    def sameState(a: firrtl.CircuitState, b: firrtl.CircuitState): Boolean =
      a.form == b.form && a.renames == b.renames && a.circuit == b.circuit &&
        a.annotations.toVector == b.annotations.toVector
    require(rebuilt.binding == result.binding && rebuilt.context == result.context &&
      sameState(rebuilt.state, result.state) && sameState(rebuilt.native.state, result.native.state) &&
      sameState(rebuilt.reference, result.reference) && rebuilt.manifest == result.manifest &&
      rebuilt.plan == result.plan && rebuilt.dependencies == result.dependencies &&
      rebuilt.native.binding == result.native.binding && rebuilt.native.identity == result.native.identity &&
      rebuilt.native.annotationProjection == result.native.annotationProjection &&
      rebuilt.native.generatedAnnotations.toVector == result.native.generatedAnnotations.toVector,
      "native in-place access: changed source, network, context or identity")
    val descriptors = NativeCheckpointInPlaceAccess.descriptors(result)
    val root = result.host.circuit.modules.find(_.name == result.host.circuit.main).get.asInstanceOf[Module]
    val name = root.name + "CheckpointStateAccess"
    val ce = PhysicalClockStateBank.emit(result.reference, result.manifest, result.plan, result.dependencies)
    val JString(ceModule) = parse(ce.head.body) \ "endpoint_module"
    require(!result.state.circuit.modules.exists(m => Set(name, ceModule, "CheckpointPhysicalStateRouter")(m.name)),
      "native in-place access: module name collision")
    require(root.ports.forall(p => p.name.matches("[a-zA-Z_][a-zA-Z0-9_$]*") && !p.name.startsWith("capture_")),
      "native in-place access: reserved or unsupported port name")
    val JString(clock) = parse(result.native.binding) \ "enclosing_raw_clock"
    val functional = descriptors.entries.filterNot(_.bank == PhysicalClockStateBank.bank)
    val rtl = NativeCheckpointStateAccess.render(root, name, clock, descriptors.physicalClockGates,
      functional, ceModule, NativeCheckpointInPlace.prefix)
    val resource = Source.fromResource("CheckpointPhysicalStateRouter.sv")
    val router = try resource.mkString finally resource.close()
    val binding = canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString("existing_instance_native_state_router_only"),
      "module" -> JString(name), "network_module" -> JString(root.name), "network_instance" -> JString("network"),
      "common_source_clock" -> JString(clock), "target" -> JString(target.serialize),
      "sideband_prefix" -> JString(NativeCheckpointInPlace.prefix),
      "functional_entries" -> JInt(functional.size), "physical_clock_gates" -> JInt(descriptors.physicalClockGates),
      "context_binding_sha256" -> JString(sha256(result.binding)),
      "instrumented_sha256" -> JString(sha256(result.state.circuit.serialize)),
      "identity_binding_sha256" -> JString(descriptors.bindingSha256),
      "descriptor_manifest_sha256" -> JString(descriptors.manifestSha256),
      "endpoint_sha256" -> JString(sha256(rtl)), "router_sha256" -> JString(sha256(router)),
      "physical_bank_sha256" -> JString(sha256(ce(1).body)),
      "reset_contract" -> JString("shared_raw_controller_reset_distinct_from_functional_reset"),
      "sample_contract" -> JString("common_low_phase_unmasked_captured_ce_with_read_enable"),
      "external_inputs_require_freeze" -> (parse(result.dependencies) \ "external_inputs_require_freeze"),
      "in_place_ce_endpoint_connected" -> JBool(true), "in_place_checkpoint_widget_connected" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    Seq(GoldenGateOutputFileAnnotation(rtl, ".native-inplace-access.sv"),
      GoldenGateOutputFileAnnotation(router, ".native-inplace-access-router.sv"),
      GoldenGateOutputFileAnnotation(ce(1).body, ".native-inplace-access-ce.sv"),
      GoldenGateOutputFileAnnotation(binding, ".native-inplace-access.json"),
      GoldenGateOutputFileAnnotation(descriptors.manifest, ".native-inplace-access-descriptors.json"),
      GoldenGateOutputFileAnnotation(descriptors.binding, ".native-inplace-access-descriptor-binding.json")) ++
      NativeCheckpointConstraints.emit(result.native.state, name, placement)
  }
}
