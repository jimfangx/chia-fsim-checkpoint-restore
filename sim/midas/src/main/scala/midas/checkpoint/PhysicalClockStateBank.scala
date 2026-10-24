// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Read-only endpoint for the UNMASKED logical CE samples from the common
  * low-phase controller. These have distinct physical StateIDs, never the
  * metasim O IDs. This companion to the wiring probe supplies no storage,
  * freeze-owner admission, bank arbitration or metasim restore adapter.
  */
object PhysicalClockStateBank {
  import SemanticStateInventory.{canonical, sha256}
  val bank = 255
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Physical clock state bank unsupported: $why")

  def emit(state: CircuitState, manifest: String, plan: String, dependencies: String):
      Seq[GoldenGateOutputFileAnnotation] = {
    if (plan != PhysicalClockCutPlan.discover(state, manifest) ||
        dependencies != PhysicalClockCutDependencies.discover(state, manifest, plan))
      fail("stale topology, inventory or original CE dependencies")
    if (state.annotations.exists { case a: GoldenGateOutputFileAnnotation =>
      a.fileSuffix.startsWith(".physical-clock-state-"); case _ => false }) fail("existing state bank output")
    val gates = (parse(plan) \ "physical_gates").asInstanceOf[JArray].arr
    if (gates.isEmpty || gates.size > 65536) fail("physical CE slot count exceeds 16 bits")
    val module = state.circuit.main + "CheckpointPhysicalCEBank"
    if (state.circuit.modules.exists(_.name == module)) fail("endpoint module name collision")
    val verilog = endpoint(module, gates.size)
    val entries = gates.zipWithIndex.map { case (g, i) => JObject(List(
      "state_id" -> (g \ "logical_latch_state_id"),
      "hierarchy" -> (g \ "logical_latch_hierarchy"),
      "ce_bit" -> JInt(i), "bank" -> JInt(bank), "slot" -> JInt(i),
      "width" -> JInt(1), "depth" -> JInt(1),
      "metasim_output_state_id" -> (g \ "metasim_output_state_id"),
      "restore_adapter" -> JString("unsupported"))) }
    val binding = canonical(JObject(List(
      "bank_version" -> JInt(1), "scope" -> JString("physical_ce_readout_companion_only"),
      "root_module" -> JString(state.circuit.main), "endpoint_module" -> JString(module),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false),
      "reference_sha256" -> JString(sha256(state.circuit.serialize)),
      "simulator_manifest_sha256" -> JString(sha256(manifest)),
      "plan_sha256" -> JString(sha256(plan)), "dependencies_sha256" -> JString(sha256(dependencies)),
      "endpoint_sha256" -> JString(sha256(verilog)),
      "sample_contract" -> JString("common_low_phase_unmasked_captured_ce_with_read_enable"),
      "access_clock" -> JString("ungated_common_source"),
      "reset_contract" -> JString("same_reset_as_sampler_transport_and_bank_endpoints"),
      "bank_reservation" -> JInt(bank), "state" -> JArray(entries))))
    Seq(GoldenGateOutputFileAnnotation(binding, ".physical-clock-state-bank.json"),
      GoldenGateOutputFileAnnotation(verilog, ".physical-clock-state-bank.sv"))
  }

  private[checkpoint] def endpoint(module: String, gates: Int): String = s"""// Generated physical CE readout companion; not complete simulator capture.
// Bank 255 must be reserved by the eventual complete bank arbiter.
// Connect captured_ce/read_enable and reset to the common low-phase sampler.
module $module (
  input wire clock, reset, read_enable,
  input wire [${gates-1}:0] captured_ce,
  input wire [31:0] descriptor_entry,
  output wire descriptor_valid,
  output wire [7:0] descriptor_bank,
  output wire [15:0] descriptor_slot, descriptor_width,
  output wire [31:0] descriptor_depth,
  input wire request_valid,
  output wire request_ready,
  input wire [7:0] bank,
  input wire [15:0] slot,
  input wire [31:0] address,
  output reg response_valid,
  input wire response_ready,
  output reg [63:0] response_data,
  output reg response_error
);
  assign descriptor_valid = descriptor_entry < 32'd$gates;
  assign descriptor_bank = descriptor_valid ? 8'd255 : 8'd0;
  assign descriptor_slot = descriptor_valid ? descriptor_entry[15:0] : 16'd0;
  assign descriptor_width = descriptor_valid ? 16'd1 : 16'd0;
  assign descriptor_depth = descriptor_valid ? 32'd1 : 32'd0;
  assign request_ready = !reset && (!response_valid || response_ready);
  wire selection_valid = bank == 8'd255 && {16'd0, slot} < 32'd$gates && address == 0;
  always @(posedge clock) begin
    if (reset) begin
      response_valid <= 0;
      response_data <= 0;
      response_error <= 0;
    end else begin
      if (response_ready) response_valid <= 0;
      if (request_valid && request_ready) begin
        response_valid <= 1;
        response_error <= !read_enable || !selection_valid;
        response_data <= read_enable && selection_valid ? {63'd0, captured_ce[slot]} : 64'd0;
      end
    end
  end
endmodule
"""
}
