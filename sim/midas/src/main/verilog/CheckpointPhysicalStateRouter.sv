// Read-only composition of generated simulator banks and the physical CE bank.
// All endpoints, this router and the physical sampler MUST share an ungated
// source clock and reset. The upstream transport owns cut/abort/publication.
// An accepted request pins its destination until the response is drained, even
// if capture abort releases the owners or upstream selectors subsequently move.
// This does not admit complete production ownership or supply a restore path.
module CheckpointPhysicalStateRouter #(
    parameter integer ENTRIES = 1,
    parameter integer GATES = 3
) (
    input wire clock, reset,
    input wire [31:0] descriptor_entry,
    output wire descriptor_valid,
    output wire [7:0] descriptor_bank,
    output wire [15:0] descriptor_slot, descriptor_width,
    output wire [31:0] descriptor_depth,
    output wire [31:0] functional_descriptor_entry, ce_descriptor_entry,
    input wire functional_descriptor_valid, ce_descriptor_valid,
    input wire [7:0] functional_descriptor_bank, ce_descriptor_bank,
    input wire [15:0] functional_descriptor_slot, functional_descriptor_width,
    input wire [15:0] ce_descriptor_slot, ce_descriptor_width,
    input wire [31:0] functional_descriptor_depth, ce_descriptor_depth,
    input wire request_valid,
    output wire request_ready,
    input wire [7:0] bank,
    input wire [15:0] slot,
    input wire [31:0] address,
    output wire response_valid,
    input wire response_ready,
    output wire [63:0] response_data,
    output wire response_error,
    output wire functional_request_valid, ce_request_valid,
    input wire functional_request_ready, ce_request_ready,
    output wire [7:0] endpoint_bank,
    output wire [15:0] endpoint_slot,
    output wire [31:0] endpoint_address,
    input wire functional_response_valid, ce_response_valid,
    output wire functional_response_ready, ce_response_ready,
    input wire [63:0] functional_response_data, ce_response_data,
    input wire functional_response_error, ce_response_error
);
    // Physical bank 255 cannot alias a generated functional-bank descriptor.
    // Reject malformed CE layouts before the transport issues their reads.
    wire geometry_valid = ENTRIES > 0 && GATES > 0 && GATES <= 65536 &&
                          ENTRIES <= 65535-GATES;
    wire select_ce_descriptor = descriptor_entry >= ENTRIES;
    wire [31:0] ce_ordinal = descriptor_entry - ENTRIES;
    assign functional_descriptor_entry = select_ce_descriptor ? 32'hffffffff : descriptor_entry;
    assign ce_descriptor_entry = select_ce_descriptor ? ce_ordinal : 32'hffffffff;
    assign descriptor_valid = !reset && geometry_valid &&
        (select_ce_descriptor ? (ce_ordinal < GATES && ce_descriptor_valid &&
            ce_descriptor_bank == 8'd255 && {16'd0, ce_descriptor_slot} == ce_ordinal &&
            ce_descriptor_width == 1 && ce_descriptor_depth == 1) :
            (functional_descriptor_valid && functional_descriptor_bank != 8'd255 &&
             functional_descriptor_width > 0 && functional_descriptor_width <= 64 &&
             functional_descriptor_depth > 0));
    assign descriptor_bank = !descriptor_valid ? 8'd0 : select_ce_descriptor ? ce_descriptor_bank : functional_descriptor_bank;
    assign descriptor_slot = !descriptor_valid ? 16'd0 : select_ce_descriptor ? ce_descriptor_slot : functional_descriptor_slot;
    assign descriptor_width = !descriptor_valid ? 16'd0 : select_ce_descriptor ? ce_descriptor_width : functional_descriptor_width;
    assign descriptor_depth = !descriptor_valid ? 32'd0 : select_ce_descriptor ? ce_descriptor_depth : functional_descriptor_depth;

    reg pending, selected_ce;
    wire route_ce = bank == 8'd255;
    assign endpoint_bank = bank;
    assign endpoint_slot = slot;
    assign endpoint_address = address;
    assign request_ready = !reset && !pending && geometry_valid &&
                           (route_ce ? ce_request_ready : functional_request_ready);
    assign functional_request_valid = !reset && !pending && geometry_valid && request_valid && !route_ce;
    assign ce_request_valid = !reset && !pending && geometry_valid && request_valid && route_ce;
    assign response_valid = !reset && pending &&
                            (selected_ce ? ce_response_valid : functional_response_valid);
    assign response_data = !response_valid ? 64'd0 : selected_ce ? ce_response_data : functional_response_data;
    assign response_error = response_valid && (selected_ce ? ce_response_error : functional_response_error);
    assign functional_response_ready = !reset && pending && !selected_ce && response_ready;
    assign ce_response_ready = !reset && pending && selected_ce && response_ready;
    always @(posedge clock) begin
        if (reset) begin
            pending <= 0;
            selected_ce <= 0;
        end else begin
            if (response_valid && response_ready) pending <= 0;
            if (request_valid && request_ready) begin
                pending <= 1;
                selected_ce <= route_ce;
            end
        end
    end
endmodule
