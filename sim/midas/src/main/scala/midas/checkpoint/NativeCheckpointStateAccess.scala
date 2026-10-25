// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import firrtl.ir._
import midas.stage.GoldenGateOutputFileAnnotation
import midas.widgets.CheckpointCaptureDescriptors
import org.json4s._
import org.json4s.native.JsonMethods.parse
import scala.io.Source

/** Synthesized connection of the native network, CE endpoint and transaction
  * router. This sibling keeps the original ports explicit: reset, external CE
  * and memory writers still require a complete all-owner cut before deployment.
  * The actual Widget supplies the common mask, captured CE and raw reset.
  */
object NativeCheckpointStateAccess {
  import SemanticStateInventory.{canonical, sha256}
  def emit(reference: CircuitState, manifest: String, plan: String, dependencies: String,
      banks: GeneratedStateBankAccess.Result, wired: PhysicalClockCutWiring.Result,
      placement: midas.targetutils.xdc.XDCPathToCircuitAnnotation =
        midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, None)):
      Seq[GoldenGateOutputFileAnnotation] = {
    val descriptors = CheckpointCaptureDescriptors.fromNativeGenerated(banks, reference, manifest, plan, dependencies)
    val regenerated = PhysicalClockCutWiring.generateNative(reference, manifest, plan, dependencies, banks)
    require(wired.binding == regenerated.binding && wired.state.circuit.serialize == regenerated.state.circuit.serialize,
      "native state access: changed native clock/bank network")
    val root = reference.circuit.modules.find(_.name == reference.circuit.main).get.asInstanceOf[Module]
    val name = root.name + "CheckpointStateAccess"
    require(!reference.circuit.modules.exists(m => m.name == name || m.name == "CheckpointPhysicalStateRouter"),
      "native state access: module name collision")
    require(root.ports.forall(p => p.name.matches("[a-zA-Z_][a-zA-Z0-9_$]*") && !p.name.startsWith("capture_")),
      "native state access: reserved or unsupported port name")
    def port(p: Port): String = {
      val (signed, width) = p.tpe match {
        case UIntType(IntWidth(w)) if w > 0 => ("", w)
        case SIntType(IntWidth(w)) if w > 0 => ("signed ", w)
        case ClockType | ResetType | AsyncResetType => ("", BigInt(1))
        case _ => throw new IllegalArgumentException("native state access: resolved ground ports required")
      }
      s"  ${if (p.direction == Input) "input" else "output"} wire $signed${if (width == 1) "" else s"[${width-1}:0] "}${p.name}"
    }
    val JString(clock) = parse(plan) \ "common_source_clock"
    val gates = descriptors.physicalClockGates
    val entries = banks.entries
    val cases = entries.zipWithIndex.map { case (e, i) =>
      s"      32'd$i: begin functional_descriptor_valid=1; functional_descriptor_bank=8'd${e.bank}; " +
        s"functional_descriptor_slot=16'd${e.slot}; functional_descriptor_width=16'd${e.width}; " +
        s"functional_descriptor_depth=32'd${e.depth}; end"
    }.mkString("\n")
    val ce = PhysicalClockStateBank.emit(reference, manifest, plan, dependencies)
    val JString(ceModule) = parse(ce.head.body) \ "endpoint_module"
    val source = Source.fromResource("CheckpointPhysicalStateRouter.sv")
    val router = try source.mkString finally source.close()
    val rtl = s"""// Generated native state/CE routing companion. Complete capture is unsupported.
module $name (
${root.ports.map(port).mkString(",\n")},
  input wire capture_reset, capture_clock_mask, capture_read_enable,
  input wire [${gates-1}:0] capture_captured_ce,
  output wire [${gates-1}:0] capture_logical_ce,
  input wire [31:0] capture_descriptor_entry,
  output wire capture_descriptor_valid,
  output wire [7:0] capture_descriptor_bank,
  output wire [15:0] capture_descriptor_slot, capture_descriptor_width,
  output wire [31:0] capture_descriptor_depth,
  input wire capture_request_valid,
  output wire capture_request_ready,
  input wire [7:0] capture_bank,
  input wire [15:0] capture_slot,
  input wire [31:0] capture_address,
  output wire capture_response_valid,
  input wire capture_response_ready,
  output wire [63:0] capture_response_data,
  output wire capture_response_error
);
  wire [31:0] functional_descriptor_entry, ce_descriptor_entry;
  reg functional_descriptor_valid;
  reg [7:0] functional_descriptor_bank;
  reg [15:0] functional_descriptor_slot, functional_descriptor_width;
  reg [31:0] functional_descriptor_depth;
  always @* begin
    functional_descriptor_valid=0; functional_descriptor_bank=0;
    functional_descriptor_slot=0; functional_descriptor_width=0; functional_descriptor_depth=0;
    case (functional_descriptor_entry)
$cases
      default: begin end
    endcase
  end
  wire ce_descriptor_valid;
  wire [7:0] ce_descriptor_bank;
  wire [15:0] ce_descriptor_slot, ce_descriptor_width;
  wire [31:0] ce_descriptor_depth;
  wire functional_request_valid, functional_request_ready, functional_response_valid, functional_response_ready;
  wire functional_response_error, ce_request_valid, ce_request_ready, ce_response_valid, ce_response_ready, ce_response_error;
  wire [63:0] functional_response_data, ce_response_data;
  wire [7:0] endpoint_bank;
  wire [15:0] endpoint_slot;
  wire [31:0] endpoint_address;
  CheckpointPhysicalStateRouter #(.ENTRIES(${entries.size}),.GATES($gates)) router(
    .clock($clock),.reset(capture_reset),
    .descriptor_entry(capture_descriptor_entry),.descriptor_valid(capture_descriptor_valid),
    .descriptor_bank(capture_descriptor_bank),.descriptor_slot(capture_descriptor_slot),
    .descriptor_width(capture_descriptor_width),.descriptor_depth(capture_descriptor_depth),
    .request_valid(capture_request_valid),.request_ready(capture_request_ready),
    .bank(capture_bank),.slot(capture_slot),.address(capture_address),
    .response_valid(capture_response_valid),.response_ready(capture_response_ready),
    .response_data(capture_response_data),.response_error(capture_response_error),.*);
  $ceModule ce_endpoint(
    .clock($clock),.reset(capture_reset),.read_enable(capture_read_enable),.captured_ce(capture_captured_ce),
    .descriptor_entry(ce_descriptor_entry),.descriptor_valid(ce_descriptor_valid),
    .descriptor_bank(ce_descriptor_bank),.descriptor_slot(ce_descriptor_slot),
    .descriptor_width(ce_descriptor_width),.descriptor_depth(ce_descriptor_depth),
    .request_valid(ce_request_valid),.request_ready(ce_request_ready),
    .bank(endpoint_bank),.slot(endpoint_slot),.address(endpoint_address),
    .response_valid(ce_response_valid),.response_ready(ce_response_ready),
    .response_data(ce_response_data),.response_error(ce_response_error));
  ${root.name} network(
${root.ports.map(p => s"    .${p.name}(${p.name})").mkString(",\n")},
    .checkpoint_physical_mask(capture_clock_mask),.checkpoint_physical_logical_ce(capture_logical_ce),
    .ckpt_clock($clock),.ckpt_reset(capture_reset),.ckpt_freeze(capture_clock_mask),
    .ckpt_bank(endpoint_bank),.ckpt_slot(endpoint_slot),.ckpt_address(endpoint_address),.ckpt_match(),.ckpt_data(),
    .ckpt_req_valid(functional_request_valid),.ckpt_req_ready(functional_request_ready),
    .ckpt_resp_valid(functional_response_valid),.ckpt_resp_ready(functional_response_ready),
    .ckpt_resp_data(functional_response_data),.ckpt_resp_error(functional_response_error));
endmodule
"""
    val binding = canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString("same_reference_native_state_router_companion_only"),
      "module" -> JString(name), "common_source_clock" -> JString(clock),
      "functional_entries" -> JInt(entries.size), "physical_clock_gates" -> JInt(gates),
      "same_reference_sha256" -> JString(sha256(reference.circuit.serialize)),
      "wired_binding_sha256" -> JString(sha256(wired.binding)),
      "descriptor_binding_sha256" -> JString(descriptors.bindingSha256),
      "descriptor_manifest_sha256" -> JString(descriptors.manifestSha256),
      "endpoint_sha256" -> JString(sha256(rtl)), "router_sha256" -> JString(sha256(router)),
      "physical_bank_sha256" -> JString(sha256(ce(1).body)),
      "reset_contract" -> JString("widget_controller_reset_shared_by_native_endpoint_router_and_ce_bank"),
      "external_inputs_require_freeze" -> (parse(dependencies) \ "external_inputs_require_freeze"),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    Seq(GoldenGateOutputFileAnnotation(rtl, ".native-state-access.sv"),
      GoldenGateOutputFileAnnotation(router, ".native-state-access-router.sv"),
      GoldenGateOutputFileAnnotation(binding, ".native-state-access.json")) ++
      NativeCheckpointConstraints.emit(wired.state, name, placement)
  }
}
