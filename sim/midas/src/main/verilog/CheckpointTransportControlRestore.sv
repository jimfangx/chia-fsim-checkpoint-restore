// Transactional staging of the layout-v1 transport register image.
// COMPONENT ONLY: apply_state is an adapter interface, NOT a physical restore.
// The production consumer still has to restore each posedge/negedge register,
// outstanding stream/MMIO obligations, host owners and shadow storage before
// releasing the common cut. No inventory/image admission is supplied here.
// Commands are mutually exclusive one-cycle strobes (not ready/valid queues).
// An epoch requires begin, every ascending word, then a separate commit.
// CRC32C covers all padded 64-bit words, least-significant bit first; the
// header supplies the complemented CRC. Identity binds the exact layout/build.
// Source cycle is checked against expected_cycle, NOT the destination counter.
module CheckpointTransportControlRestore #(
    parameter integer GATES = 3,
    parameter [255:0] SCHEMA_SHA256 = 0
) (
    input wire clock, reset,
    input wire begin_restore, write_word, commit_restore, abort_restore,
    input wire [255:0] header_schema,
    input wire [31:0] header_bits, header_words,
    input wire [63:0] header_cycle, expected_cycle,
    input wire [31:0] header_crc32c,
    input wire [31:0] word_index,
    input wire [63:0] word_data,
    // Certificates supplied by an eventual complete destination owner adapter.
    // They must remain true from begin through apply, including stalled edges.
    input wire cut_valid, owners_quiescent, owner_error,
    output wire busy,
    output wire apply_valid,
    input wire apply_ready,
    output wire [GATES+480:0] apply_state,
    output wire [63:0] apply_cycle,
    output reg applied,
    // Sticky until reset: cycle, CRC, incomplete, padding, address, owner,
    // header/unsupported specialization, command-order/conflict.
    output reg [7:0] error
);
    localparam integer BITS = 481 + GATES;
    localparam integer WORDS = (BITS + 63) / 64;
    localparam [1:0] IDLE=0, LOAD=1, OFFER=2, DONE=3;
    (* fsm_encoding = "none", dont_touch = "true" *) reg [1:0] state;
    reg [GATES+480:0] saved_state;
    reg [63:0] saved_cycle;
    reg [31:0] saved_crc, running_crc, next_word;
    wire active = state == LOAD || state == OFFER;
    wire owner_ok = cut_valid && owners_quiescent && !owner_error;
    wire commands = begin_restore || write_word || commit_restore;
    wire conflict = (begin_restore && write_word) ||
        (begin_restore && commit_restore) || (write_word && commit_restore) ||
        (abort_restore && commands);
    wire [7:0] fault_now;
    assign fault_now[0] = conflict || (begin_restore && state != IDLE) ||
        (write_word && state != LOAD) || (commit_restore && state != LOAD);
    assign fault_now[1] = begin_restore && (GATES < 1 || GATES > 65535 ||
        SCHEMA_SHA256 == 0 || header_schema != SCHEMA_SHA256 ||
        header_bits != BITS || header_words != WORDS);
    assign fault_now[2] = (begin_restore || active) && !owner_ok;
    assign fault_now[3] = write_word && state == LOAD &&
        (word_index != next_word || word_index >= WORDS);
    // Shifting by 64 is legal for a full last word and yields zero padding.
    assign fault_now[4] = write_word && state == LOAD && word_index == WORDS-1 &&
        (word_data >> (BITS - 64*(WORDS-1))) != 0;
    assign fault_now[5] = commit_restore && state == LOAD && next_word != WORDS;
    assign fault_now[6] = commit_restore && state == LOAD && next_word == WORDS &&
        saved_crc != ~running_crc;
    assign fault_now[7] = (begin_restore && header_cycle != expected_cycle) ||
        (active && saved_cycle != expected_cycle);
    assign busy = state != IDLE;
    // A current owner/command fault must suppress a handshake BEFORE its edge,
    // even when error was zero on the preceding edge.
    assign apply_valid = state == OFFER && !(|error) && !(|fault_now) &&
        !reset && !abort_restore;
    assign apply_state = apply_valid ? saved_state : 0;
    assign apply_cycle = apply_valid ? saved_cycle : 0;

    function automatic [31:0] crc32c_word(input [31:0] previous, input [63:0] data);
        reg [31:0] value;
        integer i;
        begin
            value = previous;
            for(i=0;i<64;i=i+1)
                value = (value >> 1) ^ ((value[0] ^ data[i]) ? 32'h82f63b78 : 32'b0);
            crc32c_word = value;
        end
    endfunction

    always @(posedge clock) begin
        if(reset) begin
            state <= IDLE;
            saved_state <= 0;
            saved_cycle <= 0;
            saved_crc <= 0;
            running_crc <= 0;
            next_word <= 0;
            applied <= 0;
            error <= 0;
        end else begin
            applied <= 0;
            if(|fault_now) error <= error | fault_now;
            if(abort_restore) begin
                // Abort discards staging, never a previously applied epoch or
                // a sticky failure. Reset is required after any failure.
                state <= IDLE;
                saved_state <= 0;
                saved_cycle <= 0;
                saved_crc <= 0;
                running_crc <= 0;
                next_word <= 0;
            end else if (!(|error) && !(|fault_now)) begin
                if(begin_restore) begin
                    state <= LOAD;
                    saved_state <= 0;
                    saved_cycle <= header_cycle;
                    saved_crc <= header_crc32c;
                    running_crc <= 32'hffffffff;
                    next_word <= 0;
                end else if(write_word) begin
                    // Index was checked before this bounded variable shift.
                    saved_state <= saved_state | ({{(BITS-64){1'b0}},word_data} << (word_index*64));
                    running_crc <= crc32c_word(running_crc, word_data);
                    next_word <= next_word + 1;
                end else if(commit_restore) state <= OFFER;
                else if(apply_valid && apply_ready) begin
                    state <= DONE;
                    applied <= 1;
                end
            end
        end
    end
endmodule
