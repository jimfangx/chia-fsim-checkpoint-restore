// Atomic observation of a generated regional cut's COMPLETE control vector.
// Connect live_control_state to capture_cut_control_state (GATES+71 bits),
// source_pending to capture_cut_pending and every source bank handshake here.
// The existing common-low-phase controller remains the only clock authority.
// Its reset and this module's reset must be the SAME running raw-clock reset.
//
// This bounded adapter owns neither bank/router payload nor external writers,
// host state, target-time authority or complete simulator/semantic coverage.
// The additional staging owner is exposed separately, with restore disabled.
module CheckpointRegionCutSnapshot #(
    parameter integer CONTROL_BITS = 75
) (
    input wire clock, reset,
    input wire host_freeze_request, snapshot_request, snapshot_release,
    input wire physical_read_enable, physical_failed, source_pending,
    input wire [63:0] target_cycle,
    input wire [CONTROL_BITS-1:0] live_control_state,
    output wire cut_freeze_request,

    input wire request_valid,
    output wire request_ready,
    output wire source_request_valid,
    input wire source_request_ready,
    input wire source_response_valid,
    output wire source_response_ready,
    input wire [63:0] source_response_data,
    input wire source_response_error,
    output wire response_valid,
    input wire response_ready,
    output wire [63:0] response_data,
    output wire response_error,

    input wire [31:0] word_address,
    output wire snapshot_ready, snapshot_valid, snapshot_failed,
    output wire [63:0] word_data, cut_cycle,
    output wire word_error,
    output wire [5:0] snapshot_error,
    output wire [CONTROL_BITS-1:0] captured_control_state,
    output wire [CONTROL_BITS+70:0] staging_control_state
);
    wire staged_valid, borrow_enable, staged_word_error;
    wire [63:0] staged_word_data;
    wire permission = borrow_enable && physical_read_enable && !physical_failed;

    // Occupancy/error are registered state, not the live fault cone: avoid
    // feeding a controller's own read-enable/failure comparison back into its
    // freeze request. Host withdrawal cannot release an occupied epoch.
    assign cut_freeze_request = host_freeze_request || snapshot_request ||
        staging_control_state[CONTROL_BITS] || (|snapshot_error);
    assign source_request_valid = request_valid && host_freeze_request && permission;
    assign request_ready = source_request_ready && host_freeze_request && permission;
    assign response_valid = source_response_valid && permission;
    assign source_response_ready = response_ready && permission;
    assign response_data = permission ? source_response_data : 64'b0;
    assign response_error = permission && source_response_error;
    assign snapshot_valid = staged_valid && physical_read_enable && !physical_failed;
    assign word_data = snapshot_valid ? staged_word_data : 64'b0;
    assign word_error = staged_word_error || !snapshot_valid;

    // No bank acceptance can coincide with staging: borrow_enable is false
    // until the complete pre-edge vector is occupied. New faults revoke both
    // handshakes combinationally, including invalid-selector responses.
    // Keep the explicit staging ABI visible after synthesis, including fields
    // that a healthy cut constrains to zero. The independent native audit
    // checks every bit and its raw clock rather than inferring an encoding.
    (* keep_hierarchy = "yes", dont_touch = "true" *)
    CheckpointNativeStateSnapshot #(.BITS(CONTROL_BITS), .RESTORE_ENABLE(0)) staging (
        .clock(clock), .reset(reset),
        .restore_hold(1'b0), .restore_load(1'b0),
        .restore_state({(CONTROL_BITS+71){1'b0}}), .restore_ready(),
        .request(snapshot_request), .release_snapshot(snapshot_release),
        .owners_present(!physical_failed),
        .functional_frozen(physical_read_enable),
        .reader_idle(!source_pending && !source_response_valid),
        .borrow_attempt(request_valid && host_freeze_request),
        .target_cycle(target_cycle), .live_state(live_control_state),
        .word_address(word_address), .request_ready(snapshot_ready),
        .valid(staged_valid), .borrow_enable(borrow_enable), .failed(snapshot_failed),
        .word_data(staged_word_data), .word_error(staged_word_error),
        .snapshot_state(captured_control_state), .cut_cycle(cut_cycle),
        .error(snapshot_error), .control_state(staging_control_state)
    );
endmodule
