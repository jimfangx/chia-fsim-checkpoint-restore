// See LICENSE for license details.
// One outstanding transaction across independently banked native regions.
// All endpoints MUST share this raw clock and reset. The caller supplies the
// common atomic freeze; routing does not acknowledge or freeze any owner.
module CheckpointRegionStateRouter #(
  parameter integer REGIONS = 2,
  parameter [REGIONS*8-1:0] BANK_BASE = 16'h0100,
  parameter [REGIONS*8-1:0] BANK_COUNT = 16'h0101,
  parameter [REGIONS*16-1:0] CE_BASE = 32'h00010000,
  parameter [REGIONS*16-1:0] CE_COUNT = 32'h00010001
)(
  input wire clock, reset,
  input wire request_valid,
  output wire request_ready,
  input wire [7:0] bank,
  input wire [15:0] slot,
  input wire [31:0] address,
  output wire response_valid,
  input wire response_ready,
  output wire [63:0] response_data,
  output wire response_error,
  output reg [REGIONS-1:0] region_request_valid,
  input wire [REGIONS-1:0] region_request_ready,
  output reg [7:0] region_bank,
  output reg [15:0] region_slot,
  output wire [31:0] region_address,
  input wire [REGIONS-1:0] region_response_valid,
  output reg [REGIONS-1:0] region_response_ready,
  input wire [REGIONS*64-1:0] region_response_data,
  input wire [REGIONS-1:0] region_response_error
);
  localparam integer OWNER_WIDTH = REGIONS > 1 ? $clog2(REGIONS) : 1;
  reg pending, invalid;
  reg [OWNER_WIDTH-1:0] owner;
  reg route_valid;
  reg [OWNER_WIDTH-1:0] route;
  // Bank 255 belongs exclusively to physical CE slots. Functional banks use
  // disjoint half-open ranges; local slot/address bounds remain endpoint-owned.
  always @* begin
    route_valid = 0;
    route = 0;
    region_bank = 0;
    region_slot = 0;
    for (integer k=0; k<REGIONS; k=k+1) begin
      if (bank != 255 && bank >= BANK_BASE[k*8 +: 8] &&
          {1'b0,bank} < {1'b0,BANK_BASE[k*8 +: 8]} + {1'b0,BANK_COUNT[k*8 +: 8]}) begin
        route_valid = 1;
        route = k;
        region_bank = bank - BANK_BASE[k*8 +: 8];
        region_slot = slot;
      end
      if (bank == 255 && {1'b0,slot} >= {1'b0,CE_BASE[k*16 +: 16]} &&
          {1'b0,slot} < {1'b0,CE_BASE[k*16 +: 16]} + {1'b0,CE_COUNT[k*16 +: 16]}) begin
        route_valid = 1;
        route = k;
        region_bank = 255;
        region_slot = slot - CE_BASE[k*16 +: 16];
      end
    end
  end
  assign region_address = address;
  assign request_ready = !reset && !pending && (!route_valid || region_request_ready[route]);
  assign response_valid = !reset && pending && (invalid || region_response_valid[owner]);
  assign response_data = invalid || !pending ? 64'b0 : region_response_data[owner*64 +: 64];
  assign response_error = pending && (invalid || region_response_error[owner]);
  always @* begin
    region_request_valid = 0;
    region_response_ready = 0;
    if (!reset && !pending && route_valid) region_request_valid[route] = request_valid;
    if (!reset && pending && !invalid) region_response_ready[owner] = response_ready;
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
  // These are elaboration errors, never a runtime fallback to a different owner.
  initial begin
    if (REGIONS < 1 || REGIONS > 255) $fatal(1,"checkpoint region count");
    for (integer a=0; a<REGIONS; a=a+1) begin
      if (BANK_COUNT[a*8 +: 8] == 0 ||
          int'(BANK_BASE[a*8 +: 8]) + int'(BANK_COUNT[a*8 +: 8]) > 255 ||
          int'(CE_BASE[a*16 +: 16]) + int'(CE_COUNT[a*16 +: 16]) > 65536)
        $fatal(1,"checkpoint region range");
      for (integer b=0; b<a; b=b+1) begin
        if (int'(BANK_BASE[a*8 +: 8]) < int'(BANK_BASE[b*8 +: 8]) + int'(BANK_COUNT[b*8 +: 8]) &&
            int'(BANK_BASE[b*8 +: 8]) < int'(BANK_BASE[a*8 +: 8]) + int'(BANK_COUNT[a*8 +: 8]))
          $fatal(1,"checkpoint functional bank overlap");
        if (CE_COUNT[a*16 +: 16] != 0 && CE_COUNT[b*16 +: 16] != 0 &&
            int'(CE_BASE[a*16 +: 16]) < int'(CE_BASE[b*16 +: 16]) + int'(CE_COUNT[b*16 +: 16]) &&
            int'(CE_BASE[b*16 +: 16]) < int'(CE_BASE[a*16 +: 16]) + int'(CE_COUNT[a*16 +: 16]))
          $fatal(1,"checkpoint physical slot overlap");
      end
    end
  end
endmodule
