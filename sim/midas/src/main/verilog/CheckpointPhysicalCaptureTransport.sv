// Common-source, positive-edge physical cut integrated with bank/MMIO/DMA.
// Component only: the Rocket compiler still rejects unmapped physical owners.
// All functional/CE-driver owners MUST use root_clock/gate_clock; the bank
// endpoint, descriptor ROM, MMIO/DMA and this controller use ungated clock.
// External owner_frozen covers memory, host and other separately stopped owners.
// The generated descriptor/bank network must include captured_ce as logical
// default-SYNC CE-latch state, and bind every owner at the same semantic cut.
module CheckpointPhysicalCaptureTransport #(
    parameter integer GATES = 3,
    parameter integer OWNERS = 1,
    parameter integer ENTRIES = 1,
    // Use the compiler's hierarchy-preserving native gate network. Control,
    // transport and the network's source clock must remain ungated.
    parameter integer EXTERNAL_CLOCK_NETWORK = 0,
    parameter [255:0] MANIFEST_SHA256 = 256'b0
) (
    input wire clock, reset,
    input wire [GATES-1:0] ce,
    output wire root_clock,
    output wire [GATES-1:0] gate_clock,
    output wire clock_mask,
    output wire [GATES-1:0] captured_ce,
    output wire physical_stopped, physical_read_enable, physical_failed,
    output wire [63:0] physical_cut_cycle,
    output wire [2:0] physical_error,
    input wire [63:0] target_cycle,
    input wire [OWNERS-1:0] owner_present, owner_frozen, owner_error,
    output wire freeze,
    // Single-request MMIO adapter, independent of the frozen simulator clock.
    input wire mmio_valid, mmio_write,
    output wire mmio_ready,
    input wire [7:0] mmio_address,
    input wire [31:0] mmio_wdata,
    output wire mmio_response_valid,
    input wire mmio_response_ready,
    output wire [31:0] mmio_rdata,
    output wire mmio_response_error,
    // Combinational generated ROM, ordinal order from the bound manifest.
    output wire [31:0] descriptor_entry,
    input wire descriptor_valid,
    input wire [7:0] descriptor_bank,
    input wire [15:0] descriptor_slot, descriptor_width,
    input wire [31:0] descriptor_depth,
    // Exactly one outstanding logical bank read; responses may be delayed.
    output wire bank_request_valid,
    input wire bank_request_ready,
    output wire [7:0] bank,
    output wire [15:0] slot,
    output wire [31:0] address,
    input wire bank_response_valid,
    output wire bank_response_ready,
    input wire [63:0] bank_response_data,
    input wire bank_response_error,
    // 64-byte frames. Abort invalidates/flushes the entire unpublished stream.
    output wire dma_valid,
    input wire dma_ready,
    output wire [511:0] dma_data,
    output wire dma_last, dma_abort,
    // Exact live register readout. This port neither freezes transport state
    // nor includes external primitive/stream/host owners.
    output wire [GATES+480:0] control_state
);
    wire transport_freeze;
    wire [410:0] transport_control_state;
    wire [GATES+69:0] physical_control_state;
    assign control_state = {physical_control_state, transport_control_state};
    // Separately stopped owners must not resume while the physical release is
    // still being validated. They and the gated owners resume at the same
    // source edge, after the common falling-edge unmask. Feeding this extended
    // hold back into freeze_request would deadlock release.
    assign freeze = transport_freeze || physical_stopped;
    CheckpointPhysicalClockCut #(.GATES(GATES), .OWNERS(OWNERS),
        .EXTERNAL_CLOCK_NETWORK(EXTERNAL_CLOCK_NETWORK)) physical (
        .clock(clock), .reset(reset), .freeze_request(transport_freeze), .ce(ce),
        .target_cycle(target_cycle), .owner_present(owner_present),
        .root_clock(root_clock), .gate_clock(gate_clock),
        .clock_mask(clock_mask),
        .stopped(physical_stopped), .read_enable(physical_read_enable),
        .failed(physical_failed), .captured_ce(captured_ce),
        .cut_cycle(physical_cut_cycle), .error(physical_error),
        .control_state(physical_control_state)
    );
    // The physical sampler acknowledges only AFTER the common low phase and
    // its post-stop sample. The deferred coordinator then binds the settled
    // target label while waiting for every additional memory/host owner.
    // Comparing the independently sampled physical label also rejects a label
    // change during a delayed non-clock-owner acknowledgement.
    wire physical_owner_error = physical_failed ||
        (physical_read_enable && target_cycle != physical_cut_cycle);
    CheckpointCaptureTransport #(.OWNERS(OWNERS+1), .ENTRIES(ENTRIES),
        .DEFERRED_CUT(1), .MANIFEST_SHA256(MANIFEST_SHA256)) transport (
        .owner_present({1'b1, owner_present}),
        .owner_frozen({physical_read_enable, owner_frozen}),
        .owner_error({physical_owner_error, owner_error}),
        .clock(clock),
        .reset(reset),
        .target_cycle(target_cycle),
        .freeze(transport_freeze),
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
        .bank_request_valid(bank_request_valid),
        .bank_request_ready(bank_request_ready),
        .bank(bank),
        .slot(slot),
        .address(address),
        .bank_response_valid(bank_response_valid),
        .bank_response_ready(bank_response_ready),
        .bank_response_data(bank_response_data),
        .bank_response_error(bank_response_error),
        .dma_valid(dma_valid),
        .dma_ready(dma_ready),
        .dma_data(dma_data),
        .dma_last(dma_last),
        .dma_abort(dma_abort),
        .control_state(transport_control_state)
    );
endmodule
