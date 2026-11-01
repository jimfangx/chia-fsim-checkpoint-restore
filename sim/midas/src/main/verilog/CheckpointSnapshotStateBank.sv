// Banked readout of an ALREADY atomically staged owner, including its control
// bits. This does not freeze or discover state. read_enable must certify the
// complete immutable vector and its common cut for the entire transaction.
// The response registers below are a separate live readout owner; they are
// not silently appended to the staged vector. Opt-in restore below covers
// only this response owner; it cannot restore or freeze the staged vector.
module CheckpointSnapshotStateBank #(
    parameter integer BITS = 6727,
    parameter [7:0] BANK = 8'd254,
    parameter [15:0] SLOT = 16'd0,
    parameter integer RESTORE_ENABLE = 0,
    parameter integer CAPTURE_ENABLE = 0
) (
    input wire clock, reset, read_enable,
    // Local response-owner restore only. The caller parks all bank traffic
    // and separately restores the immutable payload and original endpoint.
    input wire restore_hold, restore_load,
    input wire [66:0] restore_state,
    output wire restore_ready,
    // Independent local parking for an external read-only observation port.
    // No response, fault or retained byte changes while held, including an
    // in-flight response. This does not freeze the supplied snapshot_state.
    input wire capture_hold,
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
    output reg failed,
    // Exact storage, LSB first: data, error, pending, failed.
    output wire [66:0] control_state
);
    localparam integer WORDS = (BITS + 63) / 64;
    localparam integer PAD_BITS = WORDS * 64;
    reg pending;
    reg [63:0] saved_data;
    reg saved_error;
    wire [PAD_BITS-1:0] padded = {{(PAD_BITS-BITS){1'b0}}, snapshot_state};
    wire restoring = RESTORE_ENABLE != 0 && restore_hold;
    wire capturing = CAPTURE_ENABLE != 0 && capture_hold;
    wire load_attempt = RESTORE_ENABLE != 0 && restore_load;
    wire unsafe_restore = (load_attempt && !restoring) ||
        (restoring && (read_enable || request_valid || response_ready));
    wire fault = (pending && !read_enable && !restoring && !capturing) || unsafe_restore;
    wire permission = read_enable && !failed && !fault && !reset && !restoring && !load_attempt && !capturing;
    assign restore_ready = restoring && !unsafe_restore && !reset && !capturing;
    wire invalid = bank != BANK || slot != SLOT || address >= WORDS;
    assign request_ready = permission && !pending;
    assign response_valid = permission && pending;
    assign response_data = response_valid ? saved_data : 64'b0;
    assign response_error = response_valid && saved_error;
    assign control_state = {failed, pending, saved_error, saved_data};
    always @(posedge clock) begin
        if (reset) begin
            pending <= 0;
            saved_data <= 0;
            saved_error <= 0;
            failed <= 0;
        end else if (load_attempt && restore_ready) begin
            saved_data <= restore_state[63:0];
            saved_error <= restore_state[64];
            pending <= restore_state[65];
            failed <= restore_state[66];
        end else if (!capturing) begin
            if (fault) failed <= 1;
            if (request_valid && request_ready) begin
                pending <= 1;
                saved_error <= invalid;
                saved_data <= invalid ? 64'b0 : padded[address * 64 +: 64];
            end
            if (response_valid && response_ready) pending <= 0;
        end
    end
    initial begin
        if (BITS < 1 || BANK == 255) $fatal(1, "checkpoint snapshot bank geometry/physical selector");
    end
endmodule
