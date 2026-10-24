// Synthesizable transport component; not yet wired into a CPU BridgeModule.
// See docs/checkpointing/fpga-capture-transport.md in the Chipyard repository.
// A generated immutable descriptor ROM and real state-bank endpoints are
// required. Neither DMA nor this component discovers arbitrary FPGA state.
module CheckpointCaptureTransport #(
    parameter integer OWNERS = 1,
    parameter integer ENTRIES = 1,
    parameter integer DEFERRED_CUT = 0,
    parameter [255:0] MANIFEST_SHA256 = 256'b0
) (
    input wire clock, reset,
    input wire [63:0] target_cycle,
    input wire [OWNERS-1:0] owner_present, owner_frozen, owner_error,
    output wire freeze,
    // Single-request MMIO adapter, independent of the frozen simulator clock.
    input wire mmio_valid, mmio_write,
    output wire mmio_ready,
    input wire [7:0] mmio_address,
    input wire [31:0] mmio_wdata,
    output reg mmio_response_valid,
    input wire mmio_response_ready,
    output reg [31:0] mmio_rdata,
    output reg mmio_response_error,
    // Combinational generated ROM, ordinal order from the bound manifest.
    output reg [31:0] descriptor_entry,
    input wire descriptor_valid,
    input wire [7:0] descriptor_bank,
    input wire [15:0] descriptor_slot, descriptor_width,
    input wire [31:0] descriptor_depth,
    // Exactly one outstanding logical bank read; responses may be delayed.
    output wire bank_request_valid,
    input wire bank_request_ready,
    output wire [7:0] bank,
    output wire [15:0] slot,
    output reg [31:0] address,
    input wire bank_response_valid,
    output wire bank_response_ready,
    input wire [63:0] bank_response_data,
    input wire bank_response_error,
    // 64-byte frames. Abort invalidates/flushes the entire unpublished stream.
    output wire dma_valid,
    input wire dma_ready,
    output reg [511:0] dma_data,
    output wire dma_last, dma_abort
);
    localparam [3:0] IDLE=0, WAIT_CUT=1, HEADER=2, DESCRIPTOR=3,
        REQUEST=4, RESPONSE=5, DATA=6, TRAILER=7, WAIT_PUBLISH=8,
        FAILED=9, DRAIN=10;
    localparam [63:0] MAGIC = 64'h3154504b434d5346; // bytes "FSMCKPT1"
    reg [3:0] state;
    reg [7:0] saved_bank;
    reg [15:0] saved_slot, saved_width;
    reg [31:0] saved_depth;
    reg [63:0] saved_data, words;
    reg [31:0] stream_crc;
    reg outstanding;
    // Invalid descriptor, bank failure, noncanonical data, sequence overflow.
    reg [3:0] transport_error;
    wire cut_ready, read_enable, cut_complete, cut_failed;
    wire [63:0] cut_cycle;
    wire [4:0] cut_error;
    wire mmio_fire = mmio_valid && mmio_ready;
    wire control = mmio_fire && mmio_write && mmio_address == 0;
    wire idle_ready = state == IDLE && !outstanding && cut_ready;
    wire start = control && mmio_wdata == 1 && idle_ready;
    wire publish = control && mmio_wdata == 2 && state != IDLE;
    wire abort = control && mmio_wdata == 4;
    wire transport_fault = |transport_error;
    wire fault = cut_failed || transport_fault;
    wire request_fire = bank_request_valid && bank_request_ready;
    wire response_fire = bank_response_valid && bank_response_ready;
    wire stream_fire = dma_valid && dma_ready;

    // Reflected CRC32C, byte order identical to DMA's little-endian words.
    // Covers every header/data bit; trailer contains the complemented value.
    function automatic [31:0] crc32c_frame(input [31:0] previous, input [511:0] frame);
        reg [31:0] value;
        integer bit_index;
        begin
            value = previous;
            for (bit_index=0; bit_index<512; bit_index=bit_index+1)
                value = (value >> 1) ^ ((value[0] ^ frame[bit_index]) ? 32'h82f63b78 : 32'b0);
            crc32c_frame = value;
        end
    endfunction

    CheckpointCaptureCut #(.OWNERS(OWNERS+1), .DEFERRED_CUT(DEFERRED_CUT)) cut (
        .clock(clock), .reset(reset), .request_valid(start),
        .request_ready(cut_ready), .target_cycle(target_cycle),
        .owner_present({1'b1, owner_present}),
        .owner_frozen({1'b1, owner_frozen}),
        .owner_error({transport_fault, owner_error}),
        .capture_complete(stream_fire && state == TRAILER),
        .publish_release(publish), .abort_capture(abort),
        .freeze(freeze), .read_enable(read_enable), .complete(cut_complete),
        .failed(cut_failed), .cut_cycle(cut_cycle), .error(cut_error)
    );

    assign mmio_ready = !reset && (!mmio_response_valid || mmio_response_ready);
    assign bank = saved_bank;
    assign slot = saved_slot;
    assign bank_request_valid = state == REQUEST && read_enable && !abort && !reset;
    // Fault/abort cannot orphan an already accepted read. Drain it before a new
    // command, even if owners have resumed. No stale response enters a new cut.
    assign bank_response_ready = outstanding && !reset;
    assign dma_valid = (state == HEADER || state == DATA || state == TRAILER)
                       && read_enable && !abort && !reset;
    assign dma_last = state == TRAILER;
    assign dma_abort = abort || fault || (freeze && !read_enable &&
                       state != WAIT_CUT && state != WAIT_PUBLISH && state != IDLE);

    always @* begin
        dma_data = 0;
        dma_data[63:0] = MAGIC;
        dma_data[79:64] = 16'd1;
        dma_data[95:80] = state == HEADER ? 16'd1 : state == DATA ? 16'd2 : 16'd3;
        dma_data[159:96] = cut_cycle;
        dma_data[223:160] = words;
        if (state == HEADER) begin
            dma_data[255:224] = ENTRIES;
            dma_data[511:256] = MANIFEST_SHA256;
        end else if (state == DATA) begin
            dma_data[255:224] = descriptor_entry;
            dma_data[287:256] = address;
            dma_data[303:288] = saved_width;
            dma_data[311:304] = saved_bank;
            dma_data[327:312] = saved_slot;
            dma_data[399:336] = saved_data;
        end else dma_data[255:224] = ~stream_crc;
    end

    // MMIO responses are captured once and remain stable under backpressure.
    always @(posedge clock) begin
        if (reset) begin
            mmio_response_valid <= 0;
            mmio_rdata <= 0;
            mmio_response_error <= 0;
        end else begin
            if (mmio_response_ready) mmio_response_valid <= 0;
            if (mmio_fire) begin
                mmio_response_valid <= 1;
                mmio_rdata <= 0;
                mmio_response_error <= 0;
                if (mmio_write) begin
                    if (mmio_address != 0 ||
                        !((mmio_wdata == 1 && idle_ready) || mmio_wdata == 4 ||
                          (mmio_wdata == 2 && state == WAIT_PUBLISH && cut_complete)))
                        mmio_response_error <= 1;
                end else begin
                    case (mmio_address)
                        8'h04: mmio_rdata <= {26'b0, state == DRAIN,
                            fault, cut_complete, read_enable, freeze, idle_ready};
                        8'h08: mmio_rdata <= {12'b0, transport_error, 11'b0, cut_error};
                        8'h10: mmio_rdata <= cut_cycle[31:0];
                        8'h14: mmio_rdata <= cut_cycle[63:32];
                        8'h18: mmio_rdata <= words[31:0];
                        8'h1c: mmio_rdata <= words[63:32];
                        8'h20: mmio_rdata <= ENTRIES;
                        8'h24: mmio_rdata <= ~stream_crc;
                        8'h40,8'h44,8'h48,8'h4c,8'h50,8'h54,8'h58,8'h5c:
                            mmio_rdata <= MANIFEST_SHA256 >> (32 * ((mmio_address-8'h40) >> 2));
                        default: mmio_response_error <= 1;
                    endcase
                end
            end
        end
    end

    always @(posedge clock) begin
        if (reset) begin
            state <= IDLE;
            outstanding <= 0;
            descriptor_entry <= 0;
            address <= 0;
            saved_bank <= 0;
            saved_slot <= 0;
            saved_width <= 0;
            saved_depth <= 0;
            saved_data <= 0;
            words <= 0;
            transport_error <= 0;
            stream_crc <= 32'hffffffff;
        end else begin
            if (stream_fire && state != TRAILER)
                stream_crc <= crc32c_frame(stream_crc, dma_data);
            if (request_fire) outstanding <= 1;
            if (response_fire) outstanding <= 0;
            if (abort) begin
                state <= outstanding && !response_fire ? DRAIN : IDLE;
                transport_error <= 0;
            end else if (state == DRAIN) begin
                if (!outstanding || response_fire) state <= IDLE;
            end else if (state != IDLE && fault) begin
                state <= FAILED;
            end else begin
                case (state)
                    IDLE: if (start) begin
                        state <= WAIT_CUT;
                        descriptor_entry <= 0;
                        address <= 0;
                        words <= 0;
                        transport_error <= 0;
                        stream_crc <= 32'hffffffff;
                    end
                    WAIT_CUT: if (read_enable) state <= HEADER;
                    HEADER: if (stream_fire) state <= DESCRIPTOR;
                    DESCRIPTOR: begin
                        if (!descriptor_valid || descriptor_width == 0 ||
                            descriptor_width > 64 || descriptor_depth == 0 ||
                            ENTRIES < 1 || ENTRIES > 65535 || MANIFEST_SHA256 == 0) begin
                            transport_error[0] <= 1;
                            state <= FAILED;
                        end else begin
                            saved_bank <= descriptor_bank;
                            saved_slot <= descriptor_slot;
                            saved_width <= descriptor_width;
                            saved_depth <= descriptor_depth;
                            state <= REQUEST;
                        end
                    end
                    REQUEST: if (request_fire) state <= RESPONSE;
                    RESPONSE: if (response_fire && read_enable) begin
                        if (bank_response_error) begin
                            transport_error[1] <= 1;
                            state <= FAILED;
                        end else if ((bank_response_data >> saved_width) != 0) begin
                            transport_error[2] <= 1;
                            state <= FAILED;
                        end else begin
                            saved_data <= bank_response_data;
                            state <= DATA;
                        end
                    end
                    DATA: if (stream_fire) begin
                        words <= words + 1;
                        if (&words) begin
                            transport_error[3] <= 1;
                            state <= FAILED;
                        end else if (address == saved_depth - 1) begin
                            address <= 0;
                            if (descriptor_entry == ENTRIES - 1) state <= TRAILER;
                            else begin
                                descriptor_entry <= descriptor_entry + 1;
                                state <= DESCRIPTOR;
                            end
                        end else begin
                            address <= address + 1;
                            state <= REQUEST;
                        end
                    end
                    TRAILER: if (stream_fire) state <= WAIT_PUBLISH;
                    WAIT_PUBLISH: if (publish && cut_complete) state <= IDLE;
                    default: ; // FAILED retains freeze until explicit abort.
                endcase
            end
        end
    end
endmodule
