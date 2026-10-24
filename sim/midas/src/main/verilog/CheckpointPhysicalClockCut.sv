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
    parameter integer EXTERNAL_CLOCK_NETWORK = 0
) (
    input wire clock,
    input wire reset,
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
    output reg [2:0] error
);
    reg freeze_low;
    reg sampled;
    reg release_pending;
    wire missing_owner = !(&owner_present);
    wire [2:0] faults = {sampled && target_cycle != cut_cycle,
                         sampled && ce != captured_ce, missing_owner};
    assign failed = |error || (sampled && |faults);
    assign stopped = freeze_low;
    assign clock_mask = freeze_low;
    // Publication/read permission disappears combinationally on a new fault.
    assign read_enable = sampled && freeze_low && freeze_request && !failed && !reset;

    // The mask may change only at the common source's falling edge. Native
    // BUFGCE's transparent-low latch settles before the next rising edge.
    // A failed cut stays stopped even if the host drops freeze_request.
    always @(negedge clock) begin
        if (reset) freeze_low <= 1'b0;
        else if (freeze_request || |error) freeze_low <= 1'b1;
        else if (release_pending) freeze_low <= 1'b0;
    end
    // The external mode must instantiate no duplicate clock gates.
    generate if (EXTERNAL_CLOCK_NETWORK == 0) begin : internal_clock_network
    BUFGCE root_gate (.I(clock), .CE(!freeze_low), .O(root_clock));
    genvar g;
    for (g = 0; g < GATES; g = g + 1) begin : gates
        BUFGCE gate (.I(clock), .CE(ce[g] && !freeze_low), .O(gate_clock[g]));
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
        end else begin
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
