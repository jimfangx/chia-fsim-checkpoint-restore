// Route original bank traffic and an immutable live-shell/native staging copy
// through ONE request/response interface. This is local readout, not a global
// freeze or restore owner. All state uses the original running raw clock.
module CheckpointLiveShellSnapshotReadout #(
    parameter integer BITS = 557,
    parameter [7:0] SNAPSHOT_BANK = 8'd1,
    parameter integer RESTORE_ENABLE = 0
) (
    input wire clock, reset, permission,
    // Complete LOCAL routing/response owner. Original banks, payload, target
    // cut and every other owner must be parked/restored independently.
    input wire restore_hold, restore_load,
    input wire [68:0] restore_state,
    output wire restore_ready,
    input wire [BITS-1:0] snapshot_state,
    input wire request_valid,
    output wire request_ready,
    input wire [7:0] bank,
    input wire [15:0] slot,
    input wire [31:0] address,
    output wire response_valid,
    input wire response_ready,
    output wire [63:0] response_data,
    output wire response_error,
    output wire original_request_valid,
    input wire original_request_ready,
    output wire [7:0] original_bank,
    output wire [15:0] original_slot,
    output wire [31:0] original_address,
    input wire original_response_valid,
    output wire original_response_ready,
    input wire [63:0] original_response_data,
    input wire original_response_error,
    output wire idle,
    // Original endpoint state remains separately inventoried. New live owner:
    // LSB first: pending, snapshot_owner, snapshot bank's 67 response/control bits.
    output wire [68:0] control_state
);
    reg pending, snapshot_owner;
    wire select_snapshot = bank == SNAPSHOT_BANK;
    wire restoring = RESTORE_ENABLE != 0 && restore_hold;
    wire load_attempt = RESTORE_ENABLE != 0 && restore_load;
    wire allowed = permission && !reset && !restoring && !load_attempt;
    wire snapshot_request_ready, snapshot_response_valid, snapshot_response_ready;
    wire [63:0] snapshot_response_data;
    wire snapshot_response_error, snapshot_failed;
    wire [66:0] snapshot_control;
    wire snapshot_restore_ready;
    // Snapshot pending must have exactly one matching retained route owner.
    // All inactive data/error/owner bits remain data, even when not visible.
    wire shape_valid = restore_state[67] == (restore_state[0] && restore_state[1]);
    wire safe_restore = !permission && !request_valid && !response_ready;
    assign restore_ready = restoring && safe_restore && shape_valid && snapshot_restore_ready && !reset;
    // Original reader phases are checked independently at the staging owner.
    // Its public request_ready is permission-gated, so using it here would
    // create a capture/permission loop before the first staged copy exists.
    assign idle = !pending && !snapshot_control[65] && !snapshot_failed && !restoring && !load_attempt;
    assign request_ready = allowed && !pending &&
        (select_snapshot ? snapshot_request_ready : original_request_ready);
    assign original_request_valid = allowed && !pending && !select_snapshot && request_valid;
    assign original_bank = bank;
    assign original_slot = slot;
    assign original_address = address;
    assign response_valid = allowed && pending &&
        (snapshot_owner ? snapshot_response_valid : original_response_valid);
    assign response_data = response_valid ?
        (snapshot_owner ? snapshot_response_data : original_response_data) : 64'b0;
    assign response_error = response_valid &&
        (snapshot_owner ? snapshot_response_error : original_response_error);
    assign original_response_ready = allowed && pending && !snapshot_owner && response_ready;
    assign snapshot_response_ready = allowed && pending && snapshot_owner && response_ready;
    assign control_state = {snapshot_control, snapshot_owner, pending};
    CheckpointSnapshotStateBank #(.BITS(BITS), .BANK(SNAPSHOT_BANK), .SLOT(16'd0),
        .RESTORE_ENABLE(RESTORE_ENABLE)) snapshot_bank(
        .clock(clock), .reset(reset), .read_enable(allowed), .snapshot_state(snapshot_state),
        .restore_hold(restoring),
        .restore_load(load_attempt && restoring && safe_restore && shape_valid && !reset),
        .restore_state(restore_state[68:2]), .restore_ready(snapshot_restore_ready),
        .request_valid(allowed && !pending && select_snapshot && request_valid),
        .request_ready(snapshot_request_ready), .bank(bank), .slot(slot), .address(address),
        .response_valid(snapshot_response_valid), .response_ready(snapshot_response_ready),
        .response_data(snapshot_response_data), .response_error(snapshot_response_error),
        .failed(snapshot_failed), .control_state(snapshot_control));
    always @(posedge clock) begin
        if (reset) begin
            pending <= 0;
            snapshot_owner <= 0;
        end else if (load_attempt && restore_ready) begin
            pending <= restore_state[0];
            snapshot_owner <= restore_state[1];
        end else begin
            if (request_valid && request_ready) begin
                pending <= 1;
                snapshot_owner <= select_snapshot;
            end
            if (response_valid && response_ready) pending <= 0;
        end
    end
    initial begin
        if (BITS < 1 || SNAPSHOT_BANK == 255) $fatal(1, "live shell snapshot readout geometry");
    end
endmodule
