// Coordinated physical/native capture connected to the MMIO/DMA stream.
// Component integration only. The caller must bind every original native
// hold/write site, external owner, functional bank and complete descriptor
// manifest. This module does not admit a production Rocket image or restore.
module CheckpointNativeCaptureTransport #(
    parameter integer BITS = 1036,
    parameter integer GATES = 3,
    parameter integer OWNERS = 1,
    parameter integer ENTRIES = 1,
    parameter integer EXTERNAL_CLOCK_NETWORK = 0,
    parameter [7:0] SNAPSHOT_BANK = 8'hfc,
    parameter [255:0] MANIFEST_SHA256 = 256'b0,
    // Explicit ABI opt-in: append one descriptor for the COMPLETE stable
    // native owner (staging, physical cut and sequencer), not only payload.
    // The exact manifest must include this bank. Its own 67 live readout bits
    // below remain separately owned; this is not transport self-checkpointing.
    parameter integer CAPTURE_NATIVE_CONTROL = 0,
    parameter [7:0] NATIVE_CONTROL_BANK = 8'hfb
) (
    input wire clock, reset,
    input wire [GATES-1:0] ce,
    input wire [63:0] target_cycle,
    input wire [OWNERS-1:0] owner_present, owner_frozen, owner_error,
    // Includes outstanding responses in the original raw-clock bank network.
    input wire reader_idle,
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
    output wire bank_request_valid,
    input wire bank_request_ready,
    output wire [7:0] bank,
    output wire [15:0] slot,
    output wire [31:0] address,
    input wire bank_response_valid,
    output wire bank_response_ready,
    input wire [63:0] bank_response_data,
    input wire bank_response_error,
    output wire dma_valid,
    input wire dma_ready,
    output wire [511:0] dma_data,
    output wire dma_last, dma_abort,
    // Every new register, including the response holding register. This is a
    // live control vector, not an atomic self-snapshot or a restore ABI.
    output wire [BITS+GATES+626:0] control_state,
    output wire [66:0] native_control_bank_state
);
    localparam integer FULL_WORDS = BITS / 64;
    localparam integer TAIL_BITS = BITS % 64;
    localparam integer SNAPSHOT_ENTRIES = (FULL_WORDS != 0) + (TAIL_BITS != 0);
    localparam integer NATIVE_CONTROL_BITS = BITS + GATES + 150;
    localparam integer NATIVE_CONTROL_WORDS = (NATIVE_CONTROL_BITS + 63) / 64;
    localparam integer CONTROL_ENTRIES = CAPTURE_NATIVE_CONTROL != 0;
    wire transport_freeze, native_hold, native_ready, native_borrow;
    wire [2:0] native_phase;
    wire [5:0] native_error;
    wire [BITS+GATES+149:0] native_control_state;
    wire [410:0] transport_control_state;
    wire transport_mmio_ready;
    wire is_start = mmio_write && mmio_address == 0 && mmio_wdata == 1;
    // A new transaction cannot overtake physical release after publish/abort.
    // Other commands, including abort/status, remain available while draining.
    // Use the registered phase and independent owner inputs, not capture_ready:
    // capture_ready also qualifies release_request, which itself depends on
    // transport_freeze (including an accepting start command). Feeding that
    // output back into MMIO would create a structural combinational loop even
    // though RUN and READ are mutually exclusive at runtime.
    wire start_allowed = native_phase == 0 && (&owner_present) && !(|owner_error) && !reset;
    wire command_allowed = !is_start || start_allowed;
    assign mmio_ready = transport_mmio_ready && command_allowed;
    assign freeze = transport_freeze || native_hold;
    // Original raw-clock functional write guards use the sequencer hold, not
    // the transport's accepting-edge request. Native owners and the physical
    // root must both execute that last accepting edge before the falling cut.
    assign functional_hold = native_hold;

    wire request_valid, request_ready, response_valid, response_ready;
    wire [63:0] response_data;
    wire response_error;
    wire snapshot_selected = bank == SNAPSHOT_BANK;
    wire control_selected = CONTROL_ENTRIES != 0 && bank == NATIVE_CONTROL_BANK;
    wire control_request_ready, control_response_valid, control_response_error, control_bank_failed;
    wire [63:0] control_response_data;
    reg snapshot_response_valid;
    reg [63:0] snapshot_response_data;
    reg snapshot_response_error;
    wire [31:0] snapshot_address = slot == 0 ? address : FULL_WORDS;
    wire [63:0] snapshot_word;
    wire snapshot_word_error;
    // Registered transport REQUEST intent, before the native grant. Using the
    // already grant-qualified request_valid/ready handshake here would feed
    // native_failed -> transport read permission -> borrow_attempt -> failed
    // back into itself. Bits [3:0] are the existing transport live-state ABI;
    // REQUEST is its literal source encoding 4 (FSM recoding is disabled).
    // Do not qualify intent with bank_request_ready either: real generated
    // endpoints may themselves qualify readiness with native read permission.
    wire borrow_intent = transport_control_state[3:0] == 4 && !snapshot_selected && !control_selected;
    wire snapshot_selector_valid = (slot == 0 && FULL_WORDS != 0 && address < FULL_WORDS) ||
        (slot == 1 && TAIL_BITS != 0 && address == 0);
    assign bank_request_valid = request_valid && !snapshot_selected && !control_selected && native_borrow;
    assign bank_response_ready = response_ready && !snapshot_selected && !control_selected;
    assign request_ready = snapshot_selected ? !snapshot_response_valid && native_valid :
        control_selected ? control_request_ready : bank_request_ready && native_borrow;
    assign response_valid = snapshot_selected ? snapshot_response_valid : control_selected ? control_response_valid : bank_response_valid;
    assign response_data = snapshot_selected ? snapshot_response_data : control_selected ? control_response_data : bank_response_data;
    assign response_error = snapshot_selected ? snapshot_response_error : control_selected ? control_response_error : bank_response_error;
    assign control_state = {snapshot_response_error, snapshot_response_data,
        snapshot_response_valid, transport_control_state, native_control_state};

    generate if (CAPTURE_NATIVE_CONTROL != 0) begin : native_owner_bank
        CheckpointSnapshotStateBank #(.BITS(NATIVE_CONTROL_BITS), .BANK(NATIVE_CONTROL_BANK)) endpoint (
            .clock(clock), .reset(reset), .read_enable(native_valid), .snapshot_state(native_control_state),
            .request_valid(request_valid && control_selected), .request_ready(control_request_ready),
            .bank(bank), .slot(slot), .address(address), .response_valid(control_response_valid),
            .response_ready(response_ready && control_selected), .response_data(control_response_data),
            .response_error(control_response_error), .failed(control_bank_failed),
            .control_state(native_control_bank_state));
    end else begin : no_native_owner_bank
        assign control_request_ready = 1'b0;
        assign control_response_valid = 1'b0;
        assign control_response_data = 64'b0;
        assign control_response_error = 1'b0;
        assign control_bank_failed = 1'b0;
        assign native_control_bank_state = 67'b0;
    end endgenerate

    // The response register remains stable under backpressure and drains even
    // after abort. No staging response is borrowed from an original RAM reader.
    always @(posedge clock) begin
        if (reset) begin
            snapshot_response_valid <= 0;
            snapshot_response_data <= 0;
            snapshot_response_error <= 0;
        end else begin
            if (snapshot_response_valid && response_ready && snapshot_selected)
                snapshot_response_valid <= 0;
            if (request_valid && request_ready && snapshot_selected) begin
                snapshot_response_valid <= 1;
                snapshot_response_data <= snapshot_word;
                snapshot_response_error <= snapshot_word_error || !snapshot_selector_valid;
            end
        end
    end

    CheckpointNativeStateCapture #(.BITS(BITS), .GATES(GATES), .OWNERS(OWNERS),
        .EXTERNAL_CLOCK_NETWORK(EXTERNAL_CLOCK_NETWORK)) native_capture (
        .clock(clock), .reset(reset),
        .capture_request(transport_freeze && native_phase == 0),
        .release_request(!transport_freeze && native_phase == 3),
        .capture_ready(native_ready), .ce(ce), .target_cycle(target_cycle),
        .owner_present(owner_present), .owner_frozen(owner_frozen),
        .owner_error(owner_error | {OWNERS{control_bank_failed}}),
        .reader_idle(reader_idle && !snapshot_response_valid && !native_control_bank_state[65]),
        .borrow_attempt(borrow_intent),
        .live_state(live_state), .word_address(snapshot_address),
        .root_clock(root_clock), .gate_clock(gate_clock), .clock_mask(clock_mask),
        .functional_hold(native_hold), .valid(native_valid), .borrow_enable(native_borrow),
        .failed(native_failed), .word_data(snapshot_word), .cut_cycle(native_cut_cycle),
        .word_error(snapshot_word_error), .snapshot_state(snapshot_state),
        .captured_ce(captured_ce), .phase(native_phase), .error(native_error),
        .control_state(native_control_state));

    wire control_descriptor = CONTROL_ENTRIES != 0 && descriptor_entry == ENTRIES;
    wire full_descriptor = FULL_WORDS != 0 && descriptor_entry == ENTRIES + CONTROL_ENTRIES;
    wire tail_descriptor = TAIL_BITS != 0 && descriptor_entry == ENTRIES + CONTROL_ENTRIES + (FULL_WORDS != 0);
    wire original_descriptor = descriptor_entry < ENTRIES;
    wire bound_descriptor_valid = original_descriptor ? descriptor_valid && descriptor_bank != SNAPSHOT_BANK &&
        (CONTROL_ENTRIES == 0 || descriptor_bank != NATIVE_CONTROL_BANK) : control_descriptor || full_descriptor || tail_descriptor;
    wire [7:0] bound_descriptor_bank = original_descriptor ? descriptor_bank : control_descriptor ? NATIVE_CONTROL_BANK : SNAPSHOT_BANK;
    wire [15:0] bound_descriptor_slot = original_descriptor ? descriptor_slot : control_descriptor || full_descriptor ? 0 : 1;
    wire [15:0] bound_descriptor_width = original_descriptor ? descriptor_width : control_descriptor || full_descriptor ? 64 : TAIL_BITS;
    wire [31:0] bound_descriptor_depth = original_descriptor ? descriptor_depth : control_descriptor ? NATIVE_CONTROL_WORDS : full_descriptor ? FULL_WORDS : 1;
    CheckpointCaptureTransport #(.OWNERS(OWNERS+1), .ENTRIES(ENTRIES+CONTROL_ENTRIES+SNAPSHOT_ENTRIES),
        .DEFERRED_CUT(1), .MANIFEST_SHA256(MANIFEST_SHA256)) transport (
        .clock(clock), .reset(reset), .restore_hold(1'b0), .restore_load_pos(1'b0), .restore_state(411'b0),
        .target_cycle(target_cycle), .owner_present({1'b1, owner_present}),
        .owner_frozen({native_valid, owner_frozen}), .owner_error({native_failed || control_bank_failed, owner_error}),
        .freeze(transport_freeze), .mmio_valid(mmio_valid && command_allowed), .mmio_write(mmio_write),
        .mmio_ready(transport_mmio_ready), .mmio_address(mmio_address), .mmio_wdata(mmio_wdata),
        .mmio_response_valid(mmio_response_valid), .mmio_response_ready(mmio_response_ready),
        .mmio_rdata(mmio_rdata), .mmio_response_error(mmio_response_error),
        .descriptor_entry(descriptor_entry), .descriptor_valid(bound_descriptor_valid),
        .descriptor_bank(bound_descriptor_bank), .descriptor_slot(bound_descriptor_slot),
        .descriptor_width(bound_descriptor_width), .descriptor_depth(bound_descriptor_depth),
        .bank_request_valid(request_valid), .bank_request_ready(request_ready),
        .bank(bank), .slot(slot), .address(address),
        .bank_response_valid(response_valid), .bank_response_ready(response_ready),
        .bank_response_data(response_data), .bank_response_error(response_error),
        .dma_valid(dma_valid), .dma_ready(dma_ready), .dma_data(dma_data),
        .dma_last(dma_last), .dma_abort(dma_abort), .control_state(transport_control_state));
    initial begin
        if (CAPTURE_NATIVE_CONTROL != 0 && (NATIVE_CONTROL_BANK == SNAPSHOT_BANK || NATIVE_CONTROL_BANK == 255))
            $fatal(1, "native control bank aliases payload or physical state");
    end
endmodule
