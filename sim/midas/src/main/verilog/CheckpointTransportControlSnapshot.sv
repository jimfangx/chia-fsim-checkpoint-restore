// Atomic shadow of CheckpointPhysicalCaptureTransport.control_state (layout v1).
// Component only. The caller MUST acknowledge every cut/reset/write/host owner
// before asserting cut_valid. This module cannot establish that ownership.
// All inputs are synchronous to the ungated transport clock. The source vector
// is sampled BEFORE the accepting posedge; it includes the settled falling-edge
// mask. Reading this immutable copy introduces no further source sampling edges.
// This is not a restore adapter and does not include vendor FSM encodings,
// stream/MMIO queues, functional memory, or host state. Production admission
// must continue to reject until those owners and the shadow's own state map.
module CheckpointTransportControlSnapshot #(
    parameter integer GATES = 3
) (
    input wire clock, reset,
    input wire [GATES+480:0] live_state,
    input wire [63:0] target_cycle,
    input wire cut_valid, owner_error,
    input wire request_valid,
    output wire request_ready,
    input wire release_snapshot, abort_snapshot,
    output wire snapshot_valid,
    output reg [63:0] snapshot_cycle,
    // Sticky until reset: release without snapshot, owner fault at acceptance,
    // missing acknowledged cut at acceptance. No partial snapshot is published.
    output reg [2:0] error,
    input wire [31:0] word_index,
    output wire word_valid,
    output wire [63:0] word_data
);
    localparam integer BITS = 481 + GATES;
    localparam integer WORDS = (BITS + 63) / 64;
    reg occupied;
    reg [GATES+480:0] saved_state;
    wire accept = request_valid && request_ready;
    // Release/abort cannot also accept a new epoch on the same edge. A request
    // held through backpressure must wait for the following edge to be accepted.
    assign request_ready = !occupied && !(|error) && !reset &&
        !release_snapshot && !abort_snapshot;
    assign snapshot_valid = occupied && !(|error) && !reset &&
        !release_snapshot && !abort_snapshot;
    assign word_valid = snapshot_valid && word_index < WORDS;
    // Unsized multiply would wrap a 32-bit word_index; the bound is checked
    // before using it and every invalid address produces an all-zero response.
    assign word_data = word_valid ? (saved_state >> (word_index * 64)) : 64'b0;

    always @(posedge clock) begin
        if (reset) begin
            occupied <= 1'b0;
            saved_state <= 0;
            snapshot_cycle <= 0;
            error <= 0;
        end else if (abort_snapshot) begin
            // A discarded epoch never erases the host's previously published
            // history. Abort does not forgive a sticky admission/protocol fault.
            occupied <= 1'b0;
        end else if (release_snapshot) begin
            occupied <= 1'b0;
            if (!occupied) error <= error | 3'b100;
        end else if (accept) begin
            if (!cut_valid || owner_error) begin
                error <= {1'b0, owner_error, !cut_valid};
            end else begin
                saved_state <= live_state;
                snapshot_cycle <= target_cycle;
                occupied <= 1'b1;
            end
        end
    end
endmodule
