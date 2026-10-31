// Common-source cut with an explicitly owned synchronous functional reset.
// controller_reset initializes the collector; reset_in belongs to the target
// execution and MUST NOT reset this collector. Each reset source is synchronous
// to clock and stable at its sampling edges. Asynchronous resets, pulses between
// source edges and unowned external writers require a different contract.
//
// This primitive supplies no production ownership manifest or restore adapter.
// Its complete live state is exposed for a future separately frozen owner;
// observing that state does not capture the running collector atomically.
module CheckpointResetOwnedClockCut #(
    parameter integer GATES = 3,
    parameter integer OWNERS = 1,
    parameter integer RESETS = 1,
    parameter integer EXTERNAL_CLOCK_NETWORK = 0
) (
    input wire clock, controller_reset, freeze_request,
    input wire [GATES-1:0] ce,
    input wire [63:0] target_cycle,
    input wire [OWNERS-1:0] owner_present,
    input wire reset_writer_present,
    input wire [RESETS-1:0] reset_in,
    output wire [RESETS-1:0] functional_reset,
    output wire root_clock,
    output wire [GATES-1:0] gate_clock,
    output wire clock_mask, stopped, read_enable, failed,
    output wire [GATES-1:0] captured_ce,
    output wire [63:0] cut_cycle,
    output wire [2:0] physical_error,
    // Low RESETS bits: changed reset sources; high bit: missing reset writer.
    output reg [RESETS:0] reset_error,
    output reg [RESETS-1:0] captured_reset,
    output wire [GATES+2*RESETS+70:0] control_state
);
    wire physical_read_enable, physical_failed;
    wire [GATES+69:0] physical_state;
    // Sample at the SAME falling edge that closes the physical clock mask.
    // While running this is a transparent wire path: no reset latency is added.
    // When stopped, no raw reset transition reaches the synchronous consumers.
    assign functional_reset = clock_mask ? captured_reset : reset_in;
    always @(negedge clock) begin
        if (controller_reset) captured_reset <= 0;
        else if (!clock_mask) captured_reset <= reset_in;
    end

    wire [RESETS:0] faults = {!reset_writer_present, reset_in ^ captured_reset};
    wire reset_failed = (|reset_error) || (clock_mask && |faults);
    always @(posedge clock) begin
        if (controller_reset) reset_error <= 0;
        else if (clock_mask) reset_error <= reset_error | faults;
    end

    // A newly observed reset fault withdraws read permission combinationally.
    // It also holds the falling-edge mask even if host release and the reset
    // change coincide. Sticky errors prevent a later return to the original
    // reset value, request withdrawal or abort from reopening functional clocks.
    assign failed = physical_failed || reset_failed;
    assign read_enable = physical_read_enable && !reset_failed;
    assign control_state = {reset_error, captured_reset, physical_state};
    CheckpointPhysicalClockCut #(.GATES(GATES), .OWNERS(OWNERS+1),
        .EXTERNAL_CLOCK_NETWORK(EXTERNAL_CLOCK_NETWORK), .RESTORE_ENABLE(0)) physical (
        .clock(clock), .reset(controller_reset),
        .restore_hold(1'b0), .restore_load_pos(1'b0), .restore_load_neg(1'b0),
        .restore_state({(GATES+70){1'b0}}),
        // Only narrow external-owner faults cross into the falling-edge mask.
        // The existing CE/cycle comparisons retain their registered positive-
        // edge validation; feeding their wide comparator back here would add
        // a gated-to-raw half-cycle timing path.
        .freeze_request(freeze_request || reset_failed || (clock_mask && !(&owner_present))), .ce(ce),
        .target_cycle(target_cycle),
        .owner_present({reset_writer_present && !reset_failed, owner_present}),
        .root_clock(root_clock), .gate_clock(gate_clock), .clock_mask(clock_mask),
        .stopped(stopped), .read_enable(physical_read_enable), .failed(physical_failed),
        .captured_ce(captured_ce), .cut_cycle(cut_cycle), .error(physical_error),
        .control_state(physical_state)
    );
endmodule
