// Atomic capture coordinator component. Not yet connected to the Rocket image.
// Every generated simulator/DUT/host owner must participate; owner_present is
// supplied by a complete generated ownership manifest, never a runtime subset.
// The scheduler and all write/reset owners must honor freeze on its accepting
// edge. The coordinator/transport clock continues while simulator state freezes.
module CheckpointCaptureCut #(
    parameter integer OWNERS = 1,
    // A physical gate can stop AFTER request acceptance. In this mode the
    // complete owner_frozen vector is the post-stop acknowledgement; capture
    // its settled label, rather than the MMIO/request accepting-edge label.
    parameter integer DEFERRED_CUT = 0
) (
    input  wire clock,
    input  wire reset,
    input  wire request_valid,
    output wire request_ready,
    input  wire [63:0] target_cycle,
    input  wire [OWNERS-1:0] owner_present,
    input  wire [OWNERS-1:0] owner_frozen,
    input  wire [OWNERS-1:0] owner_error,
    input  wire capture_complete,
    input  wire publish_release,
    input  wire abort_capture,
    output wire freeze,
    output wire read_enable,
    output wire complete,
    output wire failed,
    output reg  [63:0] cut_cycle,
    // missing owner, owner error, lost freeze, cycle drift, early release
    output reg  [4:0] error,
    // Exact live sequential state, LSB-first layout version 1. Observation
    // only: the ungated coordinator does not stop with the functional clocks.
    output wire [71:0] control_state
);
    localparam [2:0] IDLE=0, FREEZING=1, READING=2, COMPLETE=3, FAILED=4;
    localparam [OWNERS-1:0] ALL_OWNERS = {OWNERS{1'b1}};
    // Checkpoint ABI stores the source encoding. Extra inferred FSM storage
    // cannot be reconstructed from that vector during a zero-edge restore.
    (* fsm_encoding = "none", dont_touch = "true" *) reg [2:0] state;
    assign control_state = {error, cut_cycle, state};
    wire accept = request_valid && request_ready;
    wire all_present = owner_present == ALL_OWNERS;
    wire all_frozen = owner_frozen == ALL_OWNERS;
    wire active = state != IDLE;
    wire [4:0] faults = {
        publish_release && state != COMPLETE,
        (!DEFERRED_CUT || state == READING || state == COMPLETE) && target_cycle != cut_cycle,
        (state == READING || state == COMPLETE) && !all_frozen,
        |owner_error,
        !all_present
    };

    assign request_ready = state == IDLE && !reset;
    // Include the accepting edge, before the state register changes.
    assign freeze = !reset && (active || accept);
    // No read or publication may succeed on an edge with a new fault.
    assign read_enable = state == READING && !(|faults) && !abort_capture && !reset;
    assign complete = state == COMPLETE && !(|faults) && !abort_capture && !reset;
    assign failed = state == FAILED;

    always @(posedge clock) begin
        if (reset) begin
            state <= IDLE;
            cut_cycle <= 0;
            error <= 0;
        end else if (abort_capture) begin
            // Host discards this incomplete transfer. Previously published
            // snapshots are owned by the host and are never overwritten here.
            state <= IDLE;
        end else if (state == IDLE) begin
            if (accept) begin
                if (!DEFERRED_CUT) cut_cycle <= target_cycle;
                error <= {3'b000, |owner_error, !all_present};
                state <= (!all_present || |owner_error) ? FAILED : FREEZING;
            end
        end else if (state != FAILED) begin
            if (|faults) begin
                state <= FAILED;
                error <= error | faults;
            end else if (state == FREEZING && all_frozen) begin
                if (DEFERRED_CUT) cut_cycle <= target_cycle;
                state <= READING;
            end else if (state == READING && capture_complete) begin
                state <= COMPLETE;
            end else if (state == COMPLETE && publish_release) begin
                state <= IDLE;
            end
        end
    end
endmodule
