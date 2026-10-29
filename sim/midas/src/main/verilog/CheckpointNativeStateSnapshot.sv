// Atomic staging for a complete native-reader observation vector. This is a
// component, not a global Rocket freeze/restore owner. All vector fields and
// control inputs must belong to this SAME running raw positive-edge clock.
// The caller must freeze original storage and finish previous bank traffic
// before requesting a snapshot. Gate every borrowed-reader request with
// borrow_enable; no borrow may coincide with the snapshot accepting edge.
// Values and target_cycle are sampled BEFORE that edge (ordinary NBA semantics).
module CheckpointNativeStateSnapshot #(
    parameter integer BITS = 518
) (
    input wire clock, reset,
    input wire request, release_snapshot,
    input wire owners_present, functional_frozen, reader_idle, borrow_attempt,
    input wire [63:0] target_cycle,
    input wire [BITS-1:0] live_state,
    input wire [31:0] word_address,
    output wire request_ready, valid, borrow_enable, failed,
    output wire [63:0] word_data,
    output wire word_error,
    output wire [BITS-1:0] snapshot_state,
    output reg [63:0] cut_cycle,
    // missing owner, lost freeze, busy reader at capture/release, premature
    // borrow, cycle drift, overlapping request/release. Sticky until reset.
    output reg [5:0] error,
    // Staging/control is a separate owner; exposing it does not restore it.
    output wire [BITS+70:0] control_state
);
    localparam integer WORDS = (BITS + 63) / 64;
    localparam integer PAD_BITS = WORDS * 64;
    reg occupied;
    reg [BITS-1:0] payload;
    wire capture = request && !occupied;
    wire active = occupied || request || borrow_attempt || release_snapshot;
    wire [5:0] faults = {
        (request && occupied) || (request && release_snapshot) || (release_snapshot && !occupied),
        occupied && target_cycle != cut_cycle,
        borrow_attempt && !occupied,
        ((capture || release_snapshot) && !reader_idle) || (capture && borrow_attempt),
        active && !functional_frozen,
        active && !owners_present
    };
    assign failed = |error || |faults;
    assign request_ready = !occupied && !failed && !reset;
    // Release revokes permission before the edge. A new fault revokes both
    // serial read and borrow permission combinationally, before acknowledgement.
    assign valid = occupied && !failed && !reset && !release_snapshot;
    assign borrow_enable = valid;
    assign snapshot_state = payload;
    wire [PAD_BITS-1:0] padded = {{(PAD_BITS-BITS){1'b0}}, payload};
    assign word_error = !valid || word_address >= WORDS;
    assign word_data = word_error ? 64'b0 : padded[word_address * 64 +: 64];
    assign control_state = {error, cut_cycle, occupied, payload};

    always @(posedge clock) begin
        if (reset) begin
            occupied <= 1'b0;
            payload <= 0;
            cut_cycle <= 0;
            error <= 0;
        end else begin
            error <= error | faults;
            if (!failed) begin
                if (request && request_ready) begin
                    payload <= live_state;
                    cut_cycle <= target_cycle;
                    occupied <= 1'b1;
                end else if (release_snapshot) begin
                    occupied <= 1'b0;
                end
            end
        end
    end
endmodule
