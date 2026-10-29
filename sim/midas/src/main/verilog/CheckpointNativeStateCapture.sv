// Common-source physical cut -> atomic native staging -> reader drain/release.
// Component integration only, not a complete Rocket capture/restore owner.
// Transport, snapshot, borrowed native readers and this sequencer use clock.
// Original raw-clock native owners MUST implement functional_hold at every
// original write/advance site. Other positive-edge owners use root/gate_clock.
// owner_present/frozen must come from independently complete owner contracts;
// this module cannot establish coverage of memory, bridges, host or pre-FAME.
module CheckpointNativeStateCapture #(
    parameter integer BITS = 1036,
    parameter integer GATES = 3,
    parameter integer OWNERS = 1,
    parameter integer EXTERNAL_CLOCK_NETWORK = 0
) (
    input wire clock, reset,
    input wire capture_request, release_request,
    output wire capture_ready,
    input wire [GATES-1:0] ce,
    input wire [63:0] target_cycle,
    input wire [OWNERS-1:0] owner_present, owner_frozen, owner_error,
    input wire reader_idle, borrow_attempt,
    input wire [BITS-1:0] live_state,
    input wire [31:0] word_address,
    output wire root_clock,
    output wire [GATES-1:0] gate_clock,
    output wire clock_mask, functional_hold,
    output wire valid, borrow_enable, failed,
    output wire [63:0] word_data, cut_cycle,
    output wire word_error,
    output wire [BITS-1:0] snapshot_state,
    output wire [GATES-1:0] captured_ce,
    output reg [2:0] phase,
    // missing/error owner, lost/failed physical cut, lost functional freeze,
    // invalid command, snapshot fault, unexpected borrow. Sticky until reset.
    output reg [5:0] error,
    // Every added register: snapshot payload/control, physical cut (including
    // falling-edge mask), sequencer phase/error. Physical CE is separately
    // mapped by captured_ce; this is neither a coherent self-snapshot nor a
    // production restore ABI. No restore or deployment admission is implied.
    output wire [BITS+GATES+149:0] control_state
);
    localparam [2:0] RUN = 0, WAIT_CUT = 1, SAMPLE = 2, READ = 3,
        DRAIN = 4, RELEASE = 5, FAULT = 6;
    wire cut_request = phase != RUN && phase != RELEASE;
    wire physical_stopped, physical_ready, physical_failed;
    wire [63:0] physical_cycle;
    wire [2:0] physical_error;
    wire [GATES+69:0] physical_state;
    wire snapshot_ready, snapshot_valid, snapshot_borrow, snapshot_failed;
    wire [63:0] snapshot_word;
    wire snapshot_word_error;
    wire [5:0] snapshot_error;
    wire [BITS+70:0] staging_state;
    wire active = phase != RUN;
    wire cut_required = phase == SAMPLE || phase == READ || phase == DRAIN;
    wire safe_owners = (&owner_present) && !(|owner_error);
    wire frozen = &owner_frozen;
    // Compute fault qualification independently of failed/output permission:
    // a rejected attempted borrow must not form a combinational grant loop.
    wire read_safe = phase == READ && snapshot_valid && physical_ready &&
        !physical_failed && frozen && safe_owners && !(|error) && !reset && !release_request;
    wire [5:0] faults = {
        borrow_attempt && !read_safe,
        snapshot_failed || (phase == SAMPLE && !snapshot_ready),
        (capture_request && phase != RUN && phase != WAIT_CUT) ||
            (release_request && phase != READ && phase != DRAIN && phase != RELEASE),
        cut_required && !frozen,
        (active && physical_failed) || (cut_required && !physical_ready),
        (active || capture_request) && !safe_owners
    };
    assign failed = |error || |faults || phase == FAULT;
    // Independent of capture_request: command-pulse producers may qualify
    // their request with ready. Missing owners must not create ready/request
    // feedback through the attempted-command fault qualification.
    assign capture_ready = phase == RUN && safe_owners && !(|error) &&
        !snapshot_failed && !physical_failed && !reset && !release_request && !borrow_attempt;
    // RELEASE drops only the physical request. Local owners remain held until
    // the native mask actually opens, after physical release validation.
    assign functional_hold = cut_request || physical_stopped;
    // An unauthorized attempted borrow already has read_safe=0. Do not feed
    // its fault back into its own grant; endpoints may report an actual
    // accepted request combinationally from borrow_enable and ready.
    assign valid = read_safe && !faults[3];
    assign borrow_enable = valid && snapshot_borrow;
    assign word_error = !valid || snapshot_word_error;
    assign word_data = word_error ? 64'b0 : snapshot_word;
    assign control_state = {error, phase, physical_state, staging_state};

    CheckpointPhysicalClockCut #(.GATES(GATES), .OWNERS(OWNERS),
        .EXTERNAL_CLOCK_NETWORK(EXTERNAL_CLOCK_NETWORK)) physical (
        .clock(clock), .reset(reset), .restore_hold(1'b0),
        .restore_load_pos(1'b0), .restore_load_neg(1'b0), .restore_state({(GATES+70){1'b0}}),
        .freeze_request(cut_request), .ce(ce), .target_cycle(target_cycle),
        .owner_present(owner_present), .root_clock(root_clock), .gate_clock(gate_clock),
        .clock_mask(clock_mask), .stopped(physical_stopped), .read_enable(physical_ready),
        .failed(physical_failed), .captured_ce(captured_ce), .cut_cycle(physical_cycle),
        .error(physical_error), .control_state(physical_state));

    CheckpointNativeStateSnapshot #(.BITS(BITS)) snapshot (
        .clock(clock), .reset(reset), .restore_hold(1'b0), .restore_load(1'b0),
        .restore_state({(BITS+71){1'b0}}), .restore_ready(),
        // A rejected sampling command must preserve the previous released
        // payload/label as well as withholding all read/borrow permission.
        // Qualify from independent inputs, never from failed/request_ready.
        .request(phase == SAMPLE && !capture_request && !release_request),
        .release_snapshot(phase == DRAIN && reader_idle),
        .owners_present(safe_owners), .functional_frozen(frozen && physical_ready),
        .reader_idle(reader_idle), .borrow_attempt(borrow_attempt),
        .target_cycle(target_cycle), .live_state(live_state), .word_address(word_address),
        .request_ready(snapshot_ready), .valid(snapshot_valid), .borrow_enable(snapshot_borrow),
        .failed(snapshot_failed), .word_data(snapshot_word), .word_error(snapshot_word_error),
        .snapshot_state(snapshot_state), .cut_cycle(cut_cycle), .error(snapshot_error),
        .control_state(staging_state));

    always @(posedge clock) begin
        if (reset) begin phase <= RUN; error <= 0; end
        else begin
            error <= error | faults;
            if (failed) phase <= FAULT;
            else case (phase)
                RUN: if (capture_request) phase <= WAIT_CUT;
                WAIT_CUT: if (physical_ready && frozen && reader_idle) phase <= SAMPLE;
                SAMPLE: phase <= READ;
                READ: if (release_request) phase <= DRAIN;
                DRAIN: if (reader_idle) phase <= RELEASE;
                RELEASE: if (!physical_stopped) phase <= RUN;
                default: phase <= FAULT;
            endcase
        end
    end
endmodule
