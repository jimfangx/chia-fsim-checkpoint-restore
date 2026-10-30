// See LICENSE for license details.
// One atomic native vector and one transport across all selected queue readers.
// Original hold/write sites and every owner acknowledgement remain caller-owned.
// This component does not grant complete simulator capture or restoration.
module CheckpointNativeQueueCapture #(
    parameter integer QUEUES = 4,
    parameter integer BITS = 6656,
    parameter integer GATES = 3,
    parameter integer OWNERS = 4,
    parameter integer ENTRIES = 20,
    parameter integer EXTERNAL_CLOCK_NETWORK = 0,
    parameter [QUEUES*8-1:0] BANK_BASE = 32'h03020100,
    parameter [QUEUES*8-1:0] BANK_COUNT = 32'h01010101,
    parameter [7:0] SNAPSHOT_BANK = 8'hfc,
    parameter [255:0] MANIFEST_SHA256 = 256'b0
) (
    input wire clock, reset,
    input wire [GATES-1:0] ce,
    input wire [63:0] target_cycle,
    input wire [OWNERS-1:0] owner_present, owner_frozen, owner_error,
    // Includes outstanding responses in the original raw-clock bank network.
    input wire [BITS-1:0] live_state,
    output wire root_clock,
    output wire [GATES-1:0] gate_clock,
    output wire clock_mask, freeze, functional_hold, native_valid, native_failed,
    output wire [63:0] native_cut_cycle,
    output wire [GATES-1:0] captured_ce,
    output wire [BITS-1:0] snapshot_state,
    input wire mmio_valid, mmio_write,
    output wire mmio_ready,
    input wire [7:0] mmio_address,
    input wire [31:0] mmio_wdata,
    output wire mmio_response_valid,
    input wire mmio_response_ready,
    output wire [31:0] mmio_rdata,
    output wire mmio_response_error,
    // Original functional descriptor ROM, ENTRIES entries. SNAPSHOT_BANK is
    // reserved and cannot alias an original descriptor. The complete manifest
    // must additionally bind the automatically appended staging descriptors:
    // slot 0: BITS/64 full words (if any); slot 1: one BITS%64-bit tail (if any).
    output wire [31:0] descriptor_entry,
    input wire descriptor_valid,
    input wire [7:0] descriptor_bank,
    input wire [15:0] descriptor_slot, descriptor_width,
    input wire [31:0] descriptor_depth,
    output reg [QUEUES-1:0] queue_request_valid,
    input wire [QUEUES-1:0] queue_request_ready,
    output reg [7:0] queue_bank,
    output wire [15:0] queue_slot,
    output wire [31:0] queue_address,
    input wire [QUEUES-1:0] queue_response_valid,
    output reg [QUEUES-1:0] queue_response_ready,
    input wire [QUEUES*64-1:0] queue_response_data,
    input wire [QUEUES-1:0] queue_response_error,
    output wire dma_valid,
    input wire dma_ready,
    output wire [511:0] dma_data,
    output wire dma_last, dma_abort,
    // Every new register, including the response holding register. This is a
    // live control vector, not an atomic self-snapshot or a restore ABI.
    output wire route_pending,
    output wire [BITS+GATES+628+(QUEUES > 1 ? $clog2(QUEUES) : 1):0] control_state
);

    wire request_valid, request_ready, response_valid, response_ready;
    wire [7:0] bank;
    wire [15:0] slot;
    wire [31:0] address;
    wire [63:0] response_data;
    wire response_error;
    localparam integer OWNER_BITS = QUEUES > 1 ? $clog2(QUEUES) : 1;
    reg pending, invalid;
    reg [OWNER_BITS-1:0] owner;
    reg route_valid;
    reg [OWNER_BITS-1:0] route;
    wire [BITS+GATES+626:0] transport_state;
    // Ready alone is not an idle proof: a route may own an accepted request
    // whose response has not arrived, including an invalid-bank error response.
    wire reader_idle = !pending && (&queue_request_ready) && !(|queue_response_valid);
    assign route_pending = pending;
    assign control_state = {owner, invalid, pending, transport_state};
    assign queue_address = address;
    assign queue_slot = slot;
    assign request_ready = !reset && !pending && (!route_valid || queue_request_ready[route]);
    assign response_valid = !reset && pending && (invalid || queue_response_valid[owner]);
    assign response_data = invalid || !pending ? 64'b0 : queue_response_data[owner*64 +: 64];
    assign response_error = pending && (invalid || queue_response_error[owner]);
    always @* begin
        route_valid = 0;
        route = 0;
        queue_bank = 0;
        for (integer q=0; q<QUEUES; q=q+1) begin
            if (bank >= BANK_BASE[q*8 +: 8] &&
                {1'b0,bank} < {1'b0,BANK_BASE[q*8 +: 8]} + {1'b0,BANK_COUNT[q*8 +: 8]}) begin
                route_valid = 1;
                route = q;
                queue_bank = bank - BANK_BASE[q*8 +: 8];
            end
        end
        queue_request_valid = 0;
        queue_response_ready = 0;
        if (!reset && !pending && route_valid) queue_request_valid[route] = request_valid;
        if (!reset && pending && !invalid) queue_response_ready[owner] = response_ready;
    end
    always @(posedge clock) begin
        if (reset) begin
            pending <= 0;
            invalid <= 0;
            owner <= 0;
        end else begin
            if (request_valid && request_ready) begin
                pending <= 1;
                invalid <= !route_valid;
                owner <= route;
            end
            if (response_valid && response_ready) pending <= 0;
        end
    end

    CheckpointNativeCaptureTransport #(.BITS(BITS), .GATES(GATES), .OWNERS(OWNERS),
        .ENTRIES(ENTRIES), .EXTERNAL_CLOCK_NETWORK(EXTERNAL_CLOCK_NETWORK),
        .SNAPSHOT_BANK(SNAPSHOT_BANK), .MANIFEST_SHA256(MANIFEST_SHA256)) transport (
        .clock(clock),
        .reset(reset),
        .ce(ce),
        .target_cycle(target_cycle),
        .owner_present(owner_present),
        .owner_frozen(owner_frozen),
        .owner_error(owner_error),
        .reader_idle(reader_idle),
        .live_state(live_state),
        .root_clock(root_clock),
        .gate_clock(gate_clock),
        .clock_mask(clock_mask),
        .freeze(freeze),
        .functional_hold(functional_hold),
        .native_valid(native_valid),
        .native_failed(native_failed),
        .native_cut_cycle(native_cut_cycle),
        .captured_ce(captured_ce),
        .snapshot_state(snapshot_state),
        .mmio_valid(mmio_valid),
        .mmio_write(mmio_write),
        .mmio_ready(mmio_ready),
        .mmio_address(mmio_address),
        .mmio_wdata(mmio_wdata),
        .mmio_response_valid(mmio_response_valid),
        .mmio_response_ready(mmio_response_ready),
        .mmio_rdata(mmio_rdata),
        .mmio_response_error(mmio_response_error),
        .descriptor_entry(descriptor_entry),
        .descriptor_valid(descriptor_valid),
        .descriptor_bank(descriptor_bank),
        .descriptor_slot(descriptor_slot),
        .descriptor_width(descriptor_width),
        .descriptor_depth(descriptor_depth),
        .dma_valid(dma_valid),
        .dma_ready(dma_ready),
        .dma_data(dma_data),
        .dma_last(dma_last),
        .dma_abort(dma_abort),
        .bank_request_valid(request_valid),
        .bank_request_ready(request_ready),
        .bank_response_valid(response_valid),
        .bank_response_ready(response_ready),
        .bank_response_data(response_data),
        .bank_response_error(response_error),
        .bank(bank),
        .slot(slot),
        .address(address),
        .control_state(transport_state));

    initial begin
        if (QUEUES < 1 || QUEUES > 255) $fatal(1, "native queue capture count");
        for (integer q=0; q<QUEUES; q=q+1) begin
            if (BANK_COUNT[q*8 +: 8] == 0 ||
                int'(BANK_BASE[q*8 +: 8]) + int'(BANK_COUNT[q*8 +: 8]) > 255)
                $fatal(1, "native queue bank range");
            for (integer r=0; r<q; r=r+1)
                if (int'(BANK_BASE[q*8 +: 8]) < int'(BANK_BASE[r*8 +: 8]) + int'(BANK_COUNT[r*8 +: 8]) &&
                    int'(BANK_BASE[r*8 +: 8]) < int'(BANK_BASE[q*8 +: 8]) + int'(BANK_COUNT[q*8 +: 8]))
                    $fatal(1, "native queue bank overlap");
            if (int'(SNAPSHOT_BANK) >= int'(BANK_BASE[q*8 +: 8]) &&
                int'(SNAPSHOT_BANK) < int'(BANK_BASE[q*8 +: 8]) + int'(BANK_COUNT[q*8 +: 8]))
                $fatal(1, "native queue snapshot bank overlaps original endpoint");
        end
    end
endmodule
