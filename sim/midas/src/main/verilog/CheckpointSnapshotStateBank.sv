// Banked readout of an ALREADY atomically staged owner, including its control
// bits. This does not freeze or discover state. read_enable must certify the
// complete immutable vector and its common cut for the entire transaction.
// The response registers below are a separate live readout owner; they are
// not silently appended to the staged vector or claimed to be restorable.
module CheckpointSnapshotStateBank #(
    parameter integer BITS = 6727,
    parameter [7:0] BANK = 8'd254,
    parameter [15:0] SLOT = 16'd0
) (
    input wire clock, reset, read_enable,
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
    // Exact new storage, LSB first: data, error, pending, failed. No restore.
    output wire [66:0] control_state
);
    localparam integer WORDS = (BITS + 63) / 64;
    localparam integer PAD_BITS = WORDS * 64;
    reg pending;
    reg [63:0] saved_data;
    reg saved_error;
    wire [PAD_BITS-1:0] padded = {{(PAD_BITS-BITS){1'b0}}, snapshot_state};
    wire fault = pending && !read_enable;
    wire permission = read_enable && !failed && !fault && !reset;
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
        end else begin
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
