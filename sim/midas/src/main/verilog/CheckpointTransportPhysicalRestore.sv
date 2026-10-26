// One-shot physical register apply for a permanently parked destination.
// This supervisor deliberately has NO release: arbitrary source encodings can
// be tested without advancing a target or resuming unrepresented stream/host
// obligations. It is not a production checkpoint owner or continuation adapter.
// The staging producer retains its offer until the final apply_ready handshake.
// A failed partial load is quarantined until reset, never rolled back/resumed.
module CheckpointTransportPhysicalRestore #(
    parameter integer GATES = 3
) (
    input wire clock, reset,
    input wire apply_valid,
    output wire apply_ready,
    input wire [GATES+480:0] apply_state,
    input wire [63:0] apply_cycle, expected_cycle,
    input wire cut_valid, owners_quiescent, owner_error,
    input wire [GATES+480:0] observed_state,
    output wire restore_hold, restore_load_pos, restore_load_neg,
    output wire [GATES+480:0] restore_state,
    output wire busy,
    output reg applied,
    // specialization, readback, source cycle, offer changed/lost, owner.
    output reg [4:0] error
);
    localparam [2:0] IDLE=0, COPY_POS=1, COPY_NEG=2, VERIFY=3,
        ACK=4, DONE=5, FAILED=6;
    (* fsm_encoding = "none", dont_touch = "true" *) reg [2:0] state;
    reg [GATES+480:0] saved_state;
    reg [63:0] saved_cycle;
    reg neg_done;
    wire active = state == COPY_POS || state == COPY_NEG ||
        state == VERIFY || state == ACK;
    wire owner_ok = cut_valid && owners_quiescent && !owner_error;
    wire [4:0] fault_now;
    assign fault_now[0] = (apply_valid || active) && !owner_ok;
    assign fault_now[1] = active && (!apply_valid ||
        apply_state != saved_state || apply_cycle != saved_cycle);
    assign fault_now[2] = (apply_valid || active) && apply_cycle != expected_cycle;
    assign fault_now[3] = (state == VERIFY || state == ACK) &&
        observed_state != saved_state;
    assign fault_now[4] = apply_valid && (GATES < 1 || GATES > 65535);
    wire safe = !reset && !(|error) && !(|fault_now);

    // Permanent park also masks gates while reset is asserted. In particular,
    // restoring freeze_low=0 cannot accidentally open a physical clock.
    assign restore_hold = 1'b1;
    assign restore_state = saved_state;
    assign restore_load_pos = state == COPY_POS && safe;
    assign restore_load_neg = state == COPY_NEG && !neg_done && safe;
    assign apply_ready = state == ACK && safe;
    assign busy = state != IDLE;

    // Exactly this edge writes the physical controller's falling-edge mask.
    // All positive-edge state has already loaded and remains held throughout.
    always @(negedge clock) begin
        if (reset) neg_done <= 1'b0;
        else if (restore_load_neg) neg_done <= 1'b1;
    end

    always @(posedge clock) begin
        if (reset) begin
            state <= IDLE;
            saved_state <= 0;
            saved_cycle <= 0;
            applied <= 0;
            error <= 0;
        end else begin
            applied <= 0;
            if (|fault_now) begin
                error <= error | fault_now;
                state <= FAILED;
            end else if (!(|error)) begin
                case (state)
                    IDLE: if (apply_valid) begin
                        saved_state <= apply_state;
                        saved_cycle <= apply_cycle;
                        state <= COPY_POS;
                    end
                    COPY_POS: state <= COPY_NEG;
                    COPY_NEG: if (neg_done) state <= VERIFY;
                    VERIFY: state <= ACK;
                    ACK: if (apply_valid && apply_ready) begin
                        state <= DONE;
                        applied <= 1'b1;
                    end
                    default: ;
                endcase
            end
        end
    end
endmodule
