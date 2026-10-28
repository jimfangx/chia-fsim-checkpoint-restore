`timescale 1ns/1ps
module checkpoint_capture_widget_tb;
  import "DPI-C" function void checkpoint_widget_failed();
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
  wire controllerReset, freeze, bankValid, bankResponseReady, streamValid, streamLast, streamAbort;
  wire [31:0] entry, address;
  wire [7:0] bank;
  wire [15:0] slot;
  wire [511:0] streamData;
  reg [1:0] ownerPresent=3;
  reg streamReady=0, bankResponseValid=0;
  reg [63:0] bankResponseData=0;
  integer bankDelay=0, frames=0;
  reg [511:0] heldFrame;
  reg [638:0] shadowExpected;
  integer shadowCaptures=0, shadowWords=0, sw;
  reg [63:0] expectedWord;
  // Independent original-register oracle. No shadow storage or exported
  // control_state vector constructs the expected image.
  always @(posedge clock) begin
    if (!reset && dut.adapter.io_request_valid && dut.adapter.io_request_ready &&
        dut.adapter.io_request_bits_write && dut.adapter.io_request_bits_address=='h78 &&
        dut.adapter.io_request_bits_data==1 && !dut.snapshot.io_error) begin
      shadowExpected = {dut.localError,dut.localData,dut.localValid,dut.counterOverflow,
        dut.hostDrainedFrames,dut.producedSnapshot,dut.producedFrames,dut.invalid,
        dut.CheckpointCaptureTransport.cut.error,dut.CheckpointCaptureTransport.cut.cut_cycle,
        dut.CheckpointCaptureTransport.cut.state,dut.CheckpointCaptureTransport.address,
        dut.CheckpointCaptureTransport.descriptor_entry,dut.CheckpointCaptureTransport.mmio_response_error,
        dut.CheckpointCaptureTransport.mmio_rdata,dut.CheckpointCaptureTransport.mmio_response_valid,
        dut.CheckpointCaptureTransport.transport_error,dut.CheckpointCaptureTransport.outstanding,
        dut.CheckpointCaptureTransport.stream_crc,dut.CheckpointCaptureTransport.words,
        dut.CheckpointCaptureTransport.saved_data,dut.CheckpointCaptureTransport.saved_depth,
        dut.CheckpointCaptureTransport.saved_width,dut.CheckpointCaptureTransport.saved_slot,
        dut.CheckpointCaptureTransport.saved_bank,dut.CheckpointCaptureTransport.state};
      shadowCaptures++;
    end
  end
  CheckpointCaptureWidget dut (
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
    .streamToHostCPU_ready(streamReady), .streamToHostCPU_valid(streamValid), .streamToHostCPU_bits(streamData),
    .capture_controllerReset(controllerReset), .capture_targetCycle(64'd42),
    .capture_ownerPresent(ownerPresent), .capture_ownerFrozen({2{freeze}}), .capture_ownerError(2'b0),
    .capture_freeze(freeze), .capture_descriptorEntry(entry), .capture_descriptorValid(entry<3),
    .capture_descriptorBank(8'd0), .capture_descriptorSlot(entry[15:0]),
    .capture_descriptorWidth(16'd8), .capture_descriptorDepth(32'd1),
    .capture_bankRequestValid(bankValid), .capture_bankRequestReady(1'b1),
    .capture_bank(bank), .capture_slot(slot), .capture_address(address),
    .capture_bankResponseValid(bankResponseValid), .capture_bankResponseReady(bankResponseReady),
    .capture_bankResponseData(bankResponseData), .capture_bankResponseError(1'b0),
    .capture_streamLast(streamLast), .capture_streamAbort(streamAbort)
  );
  always @(posedge clock) begin
    if (controllerReset) begin bankDelay<=0; bankResponseValid<=0; frames<=0; end
    else begin
      if (bankResponseValid && bankResponseReady) bankResponseValid<=0;
      if (bankValid) begin
        if (bankDelay || bankResponseValid) $fatal(1,"overlapping bank requests");
        if (bank!=0 || slot>2 || address!=0) $fatal(1,"wrong selectors");
        bankDelay<=3; bankResponseData<=64'ha0+slot;
      end
      if (bankDelay>0) begin bankDelay<=bankDelay-1; if (bankDelay==1) bankResponseValid<=1; end
      if (streamValid && streamReady) begin
        if (streamData[63:0]!=64'h3154504b434d5346 || streamData[159:96]!=42) $fatal(1,"wrong envelope");
        if (frames==0 && streamData[95:80]!=1) $fatal(1,"missing header");
        if (frames>=1 && frames<=3 && (streamData[95:80]!=2 || streamData[399:336]!=64'h9f+frames)) $fatal(1,"wrong state word");
        if (frames==4 && (!streamLast || streamData[95:80]!=3)) $fatal(1,"missing trailer");
        frames<=frames+1;
      end
    end
  end
  task tick; begin @(posedge clock); #1; end endtask
  task check(input bit cond, input string why); begin
    checks=checks+1; if (!cond) begin
      $display("WIDGET_DIVERGENCE cut=42 word=%0d reason=%s",sw,why);
      checkpoint_widget_failed(); $finish;
    end
  end endtask
  task aw(input [31:0] addr); begin
    @(negedge clock); awaddr=addr; awvalid=1;
    do tick(); while (!dut.adapter.haveAW);
    @(negedge clock); awvalid=0;
  end endtask
  task w(input [31:0] data, input bit last); begin
    @(negedge clock); wdata=data; wlast=last; wvalid=1;
    do begin @(posedge clock); end while (!wready);
    #1; @(negedge clock); wvalid=0;
  end endtask
  task ar(input [31:0] addr); begin
    @(negedge clock); araddr=addr; arvalid=1;
    do tick(); while (!dut.adapter.haveAR);
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
  task command(input [31:0] value, input [1:0] expected); begin aw(0); w(value,1); b(expected); end endtask
  task readReg(input [31:0] offset, input [31:0] value); begin ar(offset); r(0,1,value); end endtask
  task writeReg(input [31:0] offset, input [31:0] value, input [1:0] status); begin
    aw(offset); w(value,1); b(status);
  end endtask
  task readShadow; begin
    readReg('h88,1); readReg('h8c,42); readReg('h90,0); readReg('h94,10);
    for(sw=0;sw<10;sw++) begin
      writeReg('h7c,sw,0); expectedWord=shadowExpected>>(sw*64);
      readReg('h80,expectedWord[31:0]); readReg('h84,expectedWord[63:32]);
      shadowWords++;
    end
    writeReg('h7c,32'hffffffff,2); readReg('h7c,9);
    writeReg('h7c,10,2); readReg('h7c,9);
  end endtask
  initial begin
    $dumpfile("checkpoint-widget.vcd"); $dumpvars(0,checkpoint_capture_widget_tb);
    repeat(3) tick(); @(negedge clock); reset=0;
    readReg('h88,0); readReg('h94,10);
    writeReg('h78,1,2); ar('h80); r(2,1,0); // cannot capture an idle, unfrozen source
    writeReg('h78,2,2); writeReg('h78,3,2); // release-empty/invalid command
    command(1,0); check(freeze,"start freezes owners");
    while (!streamValid) tick(); heldFrame=streamData;
    repeat(6) begin tick(); check(streamValid && streamData==heldFrame,"backpressured frame stable"); end
    readReg('h60,0); // initialize the original unreset local response payload
    writeReg('h78,1,0); check(shadowCaptures==1,"one accepted shadow epoch"); readShadow();
    writeReg('h78,1,2); readShadow(); // occupied capture preserves every old word
    command(4,0); check(!freeze,"abort releases freeze");
    readShadow(); // later source abort cannot change the retained epoch
    writeReg('h78,2,0); readReg('h88,0); ar('h84); r(2,1,0);
    readReg('h60,1); command(1,2); check(!freeze,"unacknowledged invalid stream blocks start");
    aw('h64); w(2,1); b(2); readReg('h60,1);
    aw('h64); w(1,1); b(0); readReg('h60,0);
    @(negedge clock); streamReady=1;
    command(1,0); while(frames<5) tick(); repeat(3) tick();
    check(freeze,"complete remains frozen pending publication"); readReg(4,10);
    readReg('h18,3); readReg('h20,3); command(2,0); check(!freeze,"publication releases freeze");
    readReg('h68,5); readReg('h6c,0);
    // Explicitly missing second owner fails frozen, no fake complete snapshot.
    @(negedge clock); ownerPresent=1;
    command(1,0); repeat(6) tick(); check(freeze,"missing owner fails frozen");
    writeReg('h78,1,2); readReg('h88,0); // freeze alone cannot certify missing owners
    readReg(8,1); readReg('h60,1);
    command(4,0); check(!freeze,"abort failed cut");
    // Old consumed-frame count cannot acknowledge the five accepted frames.
    aw('h64); w(1,1); b(2); readReg('h60,1);
    aw('h70); w(4,1); b(0); aw('h74); w(0,1); b(0);
    aw('h64); w(1,1); b(2); readReg('h60,1);
    aw('h70); w(5,1); b(0); readReg('h70,5); readReg('h74,0);
    aw('h64); w(1,1); b(0); readReg('h60,0);
    check(shadowCaptures==1 && shadowWords==30,"complete immutable shadow coverage");
    writeReg('h78,4,0); readReg('h88,0);
    $display("CHECKPOINT_WIDGET_SHADOW_PASS captures=%0d words=%0d",shadowCaptures,shadowWords);
    $display("CHECKPOINT_WIDGET_PASS checks=%0d frames=%0d",checks,frames); $finish;
  end
  initial begin #30000; $fatal(1,"watchdog timeout"); end
endmodule
