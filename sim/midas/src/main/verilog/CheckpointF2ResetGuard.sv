// F2 shell reset boundary for an explicitly owned common-source clock cut.
// The shell's original four reset/DDR synchronizer bits and AXI reset paths
// remain UNGATED. A cut is valid only while those writers stay unchanged.
// Loss of either external source cancels immediately and is retained even if
// the source returns before another raw edge (or stops the clock generator).
//
// controller_reset is separate collector initialization, after both sources
// are ready and BEFORE a capture request. Reinitializing discards a failed
// session; it is not checkpoint recovery. There is no shell restore adapter.
// The guard does not supply complete simulator/transport/memory ownership.
module CheckpointF2ResetGuard (
    input wire clock, controller_reset, clock_mask,
    input wire rst_main_n, ddr_ready,
    // {ddr_ready_sync, ddr_ready_pre_sync_meta,
    //  rst_firesim_n_sync, pre_sync_rst_n_firesim}
    input wire [3:0] writer_state,
    output wire writer_present, failed,
    output reg [3:0] captured_writers,
    output reg writer_drift,
    output reg source_lost,
    output wire [5:0] control_state
);
    wire sources_ready = rst_main_n && ddr_ready;
    // This asynchronously SET cancellation flop is intentionally not reset
    // by rst_main_n. A reset pulse must not erase evidence of its own fault.
    always @(posedge clock or negedge sources_ready) begin
        if (!sources_ready) source_lost <= 1'b1;
        else if (controller_reset) source_lost <= 1'b0;
    end
    always @(negedge clock) begin
        if (controller_reset) captured_writers <= 0;
        else if (!clock_mask) captured_writers <= writer_state;
    end
    wire drift = clock_mask && (writer_state != captured_writers);
    always @(posedge clock) begin
        if (controller_reset) writer_drift <= 1'b0;
        else writer_drift <= writer_drift || drift;
    end
    assign failed = !sources_ready || source_lost || writer_drift || drift;
    assign writer_present = !failed;
    assign control_state = {source_lost, writer_drift, captured_writers};
endmodule
