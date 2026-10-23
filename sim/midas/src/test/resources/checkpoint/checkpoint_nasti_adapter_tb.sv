`timescale 1ns/1ps
module checkpoint_nasti_adapter_tb;
  reg clock = 0; always #5 clock = ~clock;
  reg reset = 1;
  reg awvalid=0, wvalid=0, arvalid=0, bready=0, rready=0;
  reg [31:0] awaddr=0, araddr=0, wdata=0;
  reg [7:0] awlen=0, arlen=0;
  reg [2:0] awsize=2, arsize=2;
  reg [3:0] awid=3, arid=9, strb=15;
  reg wlast=1;
  wire awready, wready, arready, bvalid, rvalid;
  wire [1:0] bresp, rresp;
  wire [3:0] bid, rid;
  wire [31:0] rdata;
  wire rlast, requestValid, requestWrite, responseReady;
  wire [7:0] requestAddress;
  wire [31:0] requestData;
  reg requestReady=1, responseValid=0, responseError=0;
  reg [31:0] responseData=0;
  integer commands=0, checks=0, timeout=0;
  CheckpointNastiAdapter dut (
    .clock(clock), .reset(reset),
    .io_ctrl_aw_valid(awvalid), .io_ctrl_aw_ready(awready),
    .io_ctrl_aw_bits_addr(awaddr), .io_ctrl_aw_bits_id(awid),
    .io_ctrl_aw_bits_len(awlen), .io_ctrl_aw_bits_size(awsize),
    .io_ctrl_aw_bits_burst(2'd1), .io_ctrl_aw_bits_lock(1'b0),
    .io_ctrl_aw_bits_cache(4'd0), .io_ctrl_aw_bits_prot(3'd0),
    .io_ctrl_aw_bits_qos(4'd0), .io_ctrl_aw_bits_region(4'd0), .io_ctrl_aw_bits_user(1'b0),
    .io_ctrl_w_valid(wvalid), .io_ctrl_w_ready(wready),
    .io_ctrl_w_bits_data(wdata), .io_ctrl_w_bits_id(4'd0),
    .io_ctrl_w_bits_last(wlast), .io_ctrl_w_bits_strb(strb), .io_ctrl_w_bits_user(1'b0),
    .io_ctrl_b_valid(bvalid), .io_ctrl_b_ready(bready), .io_ctrl_b_bits_id(bid), .io_ctrl_b_bits_resp(bresp),
    .io_ctrl_ar_valid(arvalid), .io_ctrl_ar_ready(arready),
    .io_ctrl_ar_bits_addr(araddr), .io_ctrl_ar_bits_id(arid),
    .io_ctrl_ar_bits_len(arlen), .io_ctrl_ar_bits_size(arsize),
    .io_ctrl_ar_bits_burst(2'd1), .io_ctrl_ar_bits_lock(1'b0),
    .io_ctrl_ar_bits_cache(4'd0), .io_ctrl_ar_bits_prot(3'd0),
    .io_ctrl_ar_bits_qos(4'd0), .io_ctrl_ar_bits_region(4'd0), .io_ctrl_ar_bits_user(1'b0),
    .io_ctrl_r_valid(rvalid), .io_ctrl_r_ready(rready), .io_ctrl_r_bits_id(rid),
    .io_ctrl_r_bits_data(rdata), .io_ctrl_r_bits_resp(rresp), .io_ctrl_r_bits_last(rlast),
    .io_request_valid(requestValid), .io_request_ready(requestReady),
    .io_request_bits_write(requestWrite), .io_request_bits_address(requestAddress),
    .io_request_bits_data(requestData), .io_response_valid(responseValid),
    .io_response_ready(responseReady), .io_response_bits_data(responseData), .io_response_bits_error(responseError)
  );
  // Variable-latency backend response, held until accepted; no write side effects
  // are permitted for invalid AXI metadata, strobes or malformed bursts.
  integer delay=0;
  always @(posedge clock) begin
    if (reset) begin responseValid<=0; delay<=0; commands<=0; end
    else begin
      if (responseValid && responseReady) responseValid<=0;
      if (requestValid && requestReady) begin
        if (delay || responseValid) $fatal(1,"multiple backend requests");
        commands<=commands+1;
        delay<=3;
        responseData<={24'h123456,requestAddress};
        responseError<=requestAddress==8'hfc;
      end
      if (delay>0) begin
        delay<=delay-1;
        if (delay==1) responseValid<=1;
      end
    end
  end
  task tick; begin @(posedge clock); #1; end endtask
  task check(input bit cond, input string why); begin
    checks=checks+1; if (!cond) $fatal(1,"%s",why);
  end endtask
  task aw(input [31:0] addr); begin
    @(negedge clock); awaddr=addr; awvalid=1;
    do tick(); while (!dut.haveAW);
    @(negedge clock); awvalid=0;
  end endtask
  task w(input [31:0] data, input bit last); begin
    @(negedge clock); wdata=data; wlast=last; wvalid=1;
    do begin @(posedge clock); end while (!wready);
    #1; @(negedge clock); wvalid=0;
  end endtask
  task ar(input [31:0] addr); begin
    @(negedge clock); araddr=addr; arvalid=1;
    do tick(); while (!dut.haveAR);
    @(negedge clock); arvalid=0;
  end endtask
  task b(input [1:0] expected); begin
    while (!bvalid) tick();
    repeat(4) begin tick(); check(bvalid && bresp==expected && bid==awid,"held B response"); end
    @(negedge clock); bready=1; tick(); @(negedge clock); bready=0;
  end endtask
  task r(input [1:0] expected, input bit last, input [31:0] data); begin
    while (!rvalid) tick();
    repeat(3) begin tick(); check(rvalid && rresp==expected && rid==arid && rlast==last,"held R response"); end
    if (expected==0) check(rdata==data,"read data");
    @(negedge clock); rready=1; tick(); @(negedge clock); rready=0;
  end endtask
  initial begin
    repeat(3) tick(); @(negedge clock); reset=0;
    // W before AW, full global address routed by FireSim, backend stalled.
    requestReady=0; w(32'h1,1); repeat(3) tick(); check(!requestValid,"W waits for AW");
    aw(32'h12340000); repeat(4) begin tick(); check(requestValid && requestWrite && requestData==1 && requestAddress==0,"stable stalled write"); end
    @(negedge clock); requestReady=1; b(0); check(commands==1,"write exactly once");
    // AW before W, read queued behind the partially assembled write.
    aw(32'h12340000); ar(32'h12340004); repeat(3) begin tick(); check(!requestValid,"AR cannot bypass partial write"); end
    w(32'h2,1); b(0); r(0,1,32'h12345604); check(commands==3,"write then read once");
    // Byte strobes, unaligned address and unsupported access width fail locally.
    strb=3; aw(0); w(1,1); b(2); check(commands==3,"partial command rejected"); strb=15;
    aw(1); w(1,1); b(2); check(commands==3,"unaligned write rejected");
    arsize=1; ar(4); r(2,1,0); arsize=2; check(commands==3,"narrow read rejected");
    awlen=1; aw(0); w(1,0); repeat(3) begin tick(); check(!bvalid && !requestValid,"write burst drains"); end
    w(2,1); b(2); awlen=0; check(commands==3,"burst no side effects");
    arlen=2; ar(4); r(2,0,0); r(2,0,0); r(2,1,0); arlen=0; check(commands==3,"read burst rejected with complete response");
    ar(32'h123400fc); r(2,1,0); check(commands==4,"backend error propagated");
    // A read held against backend backpressure cannot be superseded by AW/W.
    requestReady=0; ar(4); repeat(2) tick();
    @(negedge clock); awvalid=1; awaddr=0; wvalid=1; wdata=1; wlast=1;
    repeat(4) begin tick(); check(requestValid && !requestWrite && requestAddress==4 && !awready && !wready,"stalled read owns request"); end
    @(negedge clock); awvalid=0; wvalid=0; requestReady=1; r(0,1,32'h12345604);
    check(commands==5,"stalled read executes exactly once");
    // Reset cancels pending frontend state; coordinated backend reset is required.
    aw(0); @(negedge clock); reset=1; repeat(2) tick();
    check(!bvalid && !rvalid && !requestValid,"reset cancels frontend pending transaction");
    @(negedge clock); reset=0; ar(4); r(0,1,32'h12345604); check(commands==1,"post reset fresh transaction");
    $display("CHECKPOINT_NASTI_PASS checks=%0d",checks); $finish;
  end
  initial begin #20000; $fatal(1,"watchdog timeout"); end
endmodule
