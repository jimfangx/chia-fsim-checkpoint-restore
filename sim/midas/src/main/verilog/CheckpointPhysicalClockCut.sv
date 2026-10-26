// Component for default SYNC BUFGCEs with ONE common ungated source clock.
// EXTERNAL_CLOCK_NETWORK connects clock_mask to the compiler-generated mask
// network, retaining native gates at their original hierarchy. The default
// component instantiates its own gates. Neither mode admits a Rocket image.
// All positive-edge simulator
// owners, including every CE driver and target-cycle counter, must use either
// root_clock or gate_clock. The transport/control clock remains ungated.
// Negative-edge, transparent-latch, unrelated-clock and external write owners
// need separate contracts; owner_present must come from a complete manifest.
module CheckpointPhysicalClockCut #(
    parameter integer GATES = 3,
    parameter integer OWNERS = 1,
    parameter integer EXTERNAL_CLOCK_NETWORK = 0,
    parameter integer RESTORE_ENABLE = 0
) (
    input wire clock,
    input wire reset,
    // Enabled only for a dedicated parked destination. Hold must be asserted
    // before its first source edge and retained permanently; there is no release.
    input wire restore_hold, restore_load_pos, restore_load_neg,
    input wire [GATES+69:0] restore_state,
    input wire freeze_request,
    input wire [GATES-1:0] ce,
    input wire [63:0] target_cycle,
    input wire [OWNERS-1:0] owner_present,
    output wire root_clock,
    output wire [GATES-1:0] gate_clock,
    output wire clock_mask,
    output wire stopped,
    output wire read_enable,
    output wire failed,
    output reg [GATES-1:0] captured_ce,
    output reg [63:0] cut_cycle,
    // missing owner, logical CE drift, target-cycle drift; sticky until reset
    output reg [2:0] error,
    // Includes the falling-edge mask, not the vendor primitive's hidden CE
    // latch (which remains a distinct generated physical owner).
    output wire [GATES+69:0] control_state
);
    reg freeze_low;
    reg sampled;
    reg release_pending;
    assign control_state = {error, cut_cycle, captured_ce, release_pending,
        sampled, freeze_low};
    wire restoring = RESTORE_ENABLE != 0 && restore_hold;
    wire missing_owner = !(&owner_present);
    wire [2:0] faults = {sampled && target_cycle != cut_cycle,
                         sampled && ce != captured_ce, missing_owner};
    assign failed = |error || (sampled && |faults);
    assign stopped = freeze_low || restoring;
    assign clock_mask = freeze_low || restoring;
    // Publication/read permission disappears combinationally on a new fault.
    assign read_enable = sampled && freeze_low && freeze_request && !failed && !reset && !restoring;

    // In capture mode the mask changes only at a source falling edge. The
    // opt-in parked adapter keeps its additional mask asserted permanently. Native
    // BUFGCE's transparent-low latch settles before the next rising edge.
    // A failed cut stays stopped even if the host drops freeze_request.
    always @(negedge clock) begin
        if (reset) freeze_low <= 1'b0;
        else if (restoring && restore_load_neg) freeze_low <= restore_state[0];
        else if (restoring) begin end
        else if (freeze_request || |error) freeze_low <= 1'b1;
        else if (release_pending) freeze_low <= 1'b0;
    end
    // The external mode must instantiate no duplicate clock gates.
    generate if (EXTERNAL_CLOCK_NETWORK == 0) begin : internal_clock_network
    BUFGCE root_gate (.I(clock), .CE(!clock_mask), .O(root_clock));
    genvar g;
    for (g = 0; g < GATES; g = g + 1) begin : gates
        BUFGCE gate (.I(clock), .CE(ce[g] && !clock_mask), .O(gate_clock[g]));
    end
    end else begin : external_clock_network
        assign root_clock = 1'b0;
        assign gate_clock = {GATES{1'b0}};
    end endgenerate

    // This ungated positive edge is AFTER the stopped low phase. CE drivers
    // have settled from the last functional edge and cannot now advance.
    // captured_ce reconstructs the UNMASKED logical latch at that low cut;
    // the stopped physical primitives contain zero and O is zero. Neither
    // physical internal state nor metasim's O register is directly read here.
    always @(posedge clock) begin
        if (reset) begin
            sampled <= 1'b0;
            release_pending <= 1'b0;
            captured_ce <= 0;
            cut_cycle <= 0;
            error <= 0;
        end else if (restoring && restore_load_pos) begin
            sampled <= restore_state[1];
            release_pending <= restore_state[2];
            captured_ce <= restore_state[GATES+2:3];
            cut_cycle <= restore_state[GATES+66:GATES+3];
            error <= restore_state[GATES+69:GATES+67];
        end else if (!restoring) begin
            // Release is checked on a full control edge before the mask can
            // open. In particular a fault concurrent with host withdrawal
            // cannot race an unregistered 64-bit compare into the half cycle.
            // Control/owner inputs obey this source-clock synchronous contract.
            release_pending <= freeze_low && sampled && !freeze_request && !failed;
            if (freeze_low) begin
            if (!sampled) begin
                captured_ce <= ce;
                cut_cycle <= target_cycle;
                sampled <= 1'b1;
                error <= error | {2'b00, missing_owner};
            end else begin
                error <= error | faults;
            end
            end else begin
                sampled <= 1'b0;
            end
        end
    end
endmodule
