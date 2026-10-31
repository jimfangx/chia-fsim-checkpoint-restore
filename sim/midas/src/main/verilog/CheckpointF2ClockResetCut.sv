// F2 shell adapter for the compiler-generated common-source clock/reset network.
// Native gates stay in the generated hierarchy; this adapter creates NO gates.
// Shell synchronizers and AXI resets stay on their original ungated paths.
//
// Connect clock_mask, logical_ce and functional_reset to the corresponding
// generated checkpoint ports. writer_state is the four ORIGINAL shell flops,
// in {ddr_sync, ddr_meta, reset_sync, reset_meta} order. controller_reset is
// independent collector initialization, never the shell or simulator reset.
//
// This connection supplies neither complete owners nor an ungated transport.
// All owner_present bits must come from separately audited, frozen owners.
// Exposed collector state is not an atomic snapshot of the running collector.
module CheckpointF2ClockResetCut #(
    parameter integer GATES = 3,
    parameter integer OWNERS = 1
) (
    input wire clock, controller_reset, freeze_request,
    input wire rst_main_n, ddr_ready,
    input wire [3:0] writer_state,
    input wire [GATES-1:0] logical_ce,
    input wire [63:0] target_cycle,
    input wire [OWNERS-1:0] owner_present,
    output wire functional_reset, clock_mask, stopped, read_enable, failed,
    output wire [GATES-1:0] captured_ce,
    output wire [3:0] captured_writers,
    output wire [63:0] cut_cycle,
    output wire [2:0] physical_error,
    output wire [1:0] reset_error,
    output wire [GATES+78:0] control_state
);
    wire writer_present, guard_failed, cut_failed;
    wire [5:0] guard_state;
    wire [GATES+72:0] cut_state;
    wire simulator_reset = !writer_state[1] || !writer_state[3];

    CheckpointF2ResetGuard guard (
        .clock(clock), .controller_reset(controller_reset), .clock_mask(clock_mask),
        .rst_main_n(rst_main_n), .ddr_ready(ddr_ready), .writer_state(writer_state),
        .writer_present(writer_present), .failed(guard_failed),
        .captured_writers(captured_writers), .control_state(guard_state)
    );
    CheckpointResetOwnedClockCut #(.GATES(GATES), .OWNERS(OWNERS), .RESETS(1),
        .EXTERNAL_CLOCK_NETWORK(1)) cut (
        .clock(clock), .controller_reset(controller_reset),
        .freeze_request(freeze_request || (clock_mask && guard_failed)),
        .ce(logical_ce), .target_cycle(target_cycle), .owner_present(owner_present),
        .reset_writer_present(writer_present), .reset_in(simulator_reset),
        .functional_reset(functional_reset), .clock_mask(clock_mask), .stopped(stopped),
        .read_enable(read_enable), .failed(cut_failed), .captured_ce(captured_ce),
        .cut_cycle(cut_cycle), .physical_error(physical_error), .reset_error(reset_error),
        .control_state(cut_state)
    );
    assign failed = cut_failed || guard_failed;
    assign control_state = {guard_state, cut_state};
endmodule
