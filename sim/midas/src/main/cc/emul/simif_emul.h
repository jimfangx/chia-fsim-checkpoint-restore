// See LICENSE for license details.

#ifndef __SIMIF_EMUL_H
#define __SIMIF_EMUL_H

#include <atomic>
#include <condition_variable>
#include <iostream>
#include <memory>
#include <mutex>
#include <thread>
#include <vector>

#include "bridges/cpu_managed_stream.h"
#include "bridges/fpga_managed_stream.h"
#include "core/simif.h"
#include "core/driver_boundary_diagnostic.h"
#include "emul/mm.h"
#include "emul/host_timing.h"
#include "emul/driver_thread.h"
#include "emul/mmio.h"
#include "emul/qsfp.h"

class mm_t;
class mmio_t;
class qsfp_t;

/**
 * Simulation implementation bridging an RTL DUT to a simulation.
 */
class simif_emul_t : public simif_t {
public:
  simif_emul_t(const TargetConfig &config,
               const std::vector<std::string> &args);
  virtual ~simif_emul_t();

  /**
   * End the simulation.
   *
   * This function joins the simulation thread. Before calling it, the
   * simulation should finish either by returning from its loop or by having
   * called finish with some other error condition.
   */
  int end();

  /**
   * Start the driver thread.
   *
   * This thread is synchronized by the main thread driven by Verilator/VCS
   * which simulates the RTL and invokes the tick function through DPI. At
   * any point in time, only one of the two is allowed to run and the
   * pthread synchronization primitives switch between the two contexts.
   *
   * When the thread starts, it is put to sleep.
   *
   * The simulation thread is woken by the target thread in the tick function,
   * after it posts data received from the RTL. The simulation thread then
   * runs for one cycle, handling the AXI transactions, before switching control
   * back to the target thread that reads outputs into the RTL.
   */
  void start_driver(simulation_t &sim);

  // Rebuild just the dispatcher after separately loading every component at
  // a low-clock returned-operation cut. Runs to the next normal yield without
  // an initial AXI tick, delay draw, fastloadmem or simulation initialization.
  // The caller must publish outgoing banks and refresh incoming banks before
  // the first rising edge. Not a whole-runtime checkpoint loading API.
  bool start_restored_driver(simulation_t &sim, uint32_t bits,
                             const uint32_t *banks);

  // Component coordinator: arm only after loading/prevalidating every owner.
  // The VCS harness holds its clock until its loader verifies the RTL/banks
  // and invokes checkpoint_resume_restored. No production restore plusarg.
  void arm_restored_driver(simulation_t &sim);
  // Reserve the fresh harness clock hold before loading any owner. VPI
  // handles/clocks settle after entry returns; the coordinator can then
  // prevalidate everything, load/read back, and arm this same driver.
  void begin_restored_load(simulation_t &sim);
  // Optional creator reservation under that hold. Callbacks run synchronously
  // on the eventual driver worker; they must return owned observations only.
  // No MMIO, RTL ticks or simulation flow is allowed during preparation. This
  // does not admit unsupported CPU owners or supply process-wide exclusion.
  void reserve_restored_creator(simulation_t &sim);
  void prepare_on_restored_creator(simulation_t &sim,
                                   emul_driver_thread_t::task_t prepare);
  // Called only inside preparation. The bundle lives until continuation exit
  // or cancellation and is destroyed on its creator. One bundle per worker.
  void retain_on_restored_creator(simulation_t &sim,
                                  std::unique_ptr<emul_driver_thread_t::resource_t> resource);
  bool restored_driver_pending() const { return restored_driver != nullptr || loading_driver != nullptr; }
  bool resume_restored_driver(uint32_t bits, const uint32_t *banks);

  /**
   * Transfers control to the simulator on a tick.
   *
   * Helper to be called solely from the target thread.
   */
  bool to_sim();

  // Return through DPI while the driver is parked at a completed operation.
  // The harness invokes this after its rising-edge NBA updates, then resumes
  // the same driver/DPI tick. Neither leg consumes a host timing tick.
  bool checkpoint_pending() const { return settled_pending.load(); }
  bool checkpoint_lowclock() const { return checkpoint_polling || driver_diagnostic.lowclock(); }
  // Read-only native/VPI collection is allowed only on VCS's main thread
  // while the completed-operation worker is parked at a low-clock cut.
  void require_settled_checkpoint_capture() const;
  void schedule_checkpoint_operation(uint64_t operation) {
    if (driver_started || checkpoint_polling)
      throw std::runtime_error("checkpoint schedule after worker startup");
    driver_diagnostic.arm_checkpoint_operation(operation);
    public_capture_required = true;
  }
  // Poll the audited target counter at every completed driver operation. The
  // caller must acknowledge each settled callback, including a not-due cut.
  // This scheduling state is host policy and is freshly configured on restore.
  void schedule_checkpoint_polling() {
    if (driver_started || checkpoint_polling || driver_diagnostic.requested())
      throw std::runtime_error("invalid repeated checkpoint polling lifecycle");
    checkpoint_polling = public_capture_required = true;
  }
  void acknowledge_checkpoint_publication();
  bool checkpoint_settled(uint32_t bits, const uint32_t *banks);

  /**
   * Transfers control to the target and lets the simulator wait for a tick.
   *
   * To be called solely from the target.
   */
  void do_tick();

  void write(size_t addr, uint32_t data) override;
  uint32_t read(size_t addr) override;

  // void sync_sockets() override;

  /**
   * Returns a pointer to the CPU-managed AXI4 IF at the metasimulation
   * boundary.
   *
   * This is used by the harness to bridge the driver and the simulation.
   */
  mmio_t *get_cpu_managed_axi4() {
    if (cpu_managed_stream_io)
      return &cpu_managed_stream_io->cpu_managed_axi4;
    return nullptr;
  }

  /**
   * Returns a pointer to a mm_t instance written to by FPGA-managed AXI4
   * transactions during metasimulation.
   *
   * This is used by the harness to bridge the driver and the simulation.
   */
  mm_t *get_fpga_managed_axi4() {
    if (fpga_managed_stream_io)
      return &fpga_managed_stream_io->cpu_mem;
    return nullptr;
  }

  /**
   * @brief Pointers to inter-context (i.e., between VCS/verilator and driver)
   * AXI4 transaction channels
   */
  std::unique_ptr<mmio_t> master;

  /**
   * @brief Host DRAM models shared across the RTL simulator and driver
   * contexts.
   *
   * The driver only needs access to these to implement a faster form of
   * loadmem, which instead of using mmio (~10 cycles per MMIO transaction),
   * writes directly into the backing memory (0 cycles). See
   * simif_emul_t::load_mems.
   */
  std::vector<std::unique_ptr<mm_t>> slave;

  // Pointers to inter-context QSFP communication channels
  std::vector<std::unique_ptr<qsfp_t>> qsfp;

  // Enumerate the audited AXI component owners with stable configuration-order
  // identities. This is a subset of runtime state: harness/PRNG/thread/driver/
  // scheduler/stream-engine/QSFP owners and a coherent freeze remain required.
  std::vector<std::pair<std::string, checkpoint_owner_t *>> checkpoint_axi_owners();

  // A component subset, with no thread freeze or driver continuation claim.
  // Hooks reject while advance_target is suspended, even if RTL is parked.
  std::pair<std::string, checkpoint_owner_t *> checkpoint_host_timing_owner() {
    return {"runtime/host-timing", &host_timing};
  }

  /**
   * Return the wrapper around the CPU-managed stream AXI interface.
   */
  CPUManagedStreamIO &get_cpu_managed_stream_io() override {
    return *cpu_managed_stream_io;
  }

  /**
   * Return the wrapper around the FPGA-managed stream AXI interface.
   */
  FPGAManagedStreamIO &get_fpga_managed_stream_io() override {
    return *fpga_managed_stream_io;
  }

protected:
  // Full PRNG state and explicit pending host-delay progress. Configured by
  // +fuzz-host-timing and +fuzz-seed; synchronous stack capture is unsupported.
  host_timing_t host_timing;

  uint64_t memsize;

  /**
   * If +fastloadmem and +loadmem are set, memories are loaded from this file.
   */
  std::string load_mem_path;

  /**
   * Advances the simulation by a random number of ticks.
   *
   * The number of ticks is bounded by the configured maximum host delay.
   */
  inline void advance_target() {
    host_timing.run_delay([this] { do_tick(); });
  }

  void wait_read(mmio_t &mmio, void *data);
  void wait_write(mmio_t &mmio);

  // Writes directly into the host DRAM models to initialize them.
  void load_mems(const char *fname);

private:
  simulation_t *restored_driver = nullptr;
  simulation_t *loading_driver = nullptr;
  bool restore_consumed = false;
  bool driver_started = false;
  bool public_capture_required = false;
  bool public_capture_published = false;
  bool checkpoint_polling = false;
  void require_restored_destination(simulation_t &sim);
  bool start_driver_impl(simulation_t &sim, bool restored);
  driver_boundary_diagnostic_t driver_diagnostic;
  // Published only while to_sim has parked VCS in DPI. This does not mean
  // RTL/NBA settlement: it certifies software ownership access on the driver.
  std::atomic<bool> driver_context_active{false};
  bool driver_capture_active = false;
  std::atomic<bool> settled_pending{false};
  std::mutex settled_mutex;
  std::condition_variable settled_cond;
  driver_boundary_diagnostic_t::snapshot_t settled_owners;
  void require_no_driver_capture() const;
  void capture_driver_boundary(simulation_t &sim, bool done);

  class CPUManagedStreamIOImpl final : public CPUManagedStreamIO {
  public:
    CPUManagedStreamIOImpl(simif_emul_t &simif, const AXI4Config &config)
        : simif(simif), cpu_managed_axi4(config) {}

    uint32_t mmio_read(size_t addr) override { return simif.read(addr); }

    size_t
    cpu_managed_axi4_write(size_t addr, const char *data, size_t size) override;

    size_t cpu_managed_axi4_read(size_t addr, char *data, size_t size) override;

    uint64_t get_beat_bytes() const override {
      return cpu_managed_axi4.get_config().beat_bytes();
    }

    simif_emul_t &simif;
    mmio_t cpu_managed_axi4;
  };

  std::unique_ptr<CPUManagedStreamIOImpl> cpu_managed_stream_io;

private:
  class FPGAManagedStreamIOImpl final : public FPGAManagedStreamIO {
  public:
    FPGAManagedStreamIOImpl(simif_emul_t &simif, const AXI4Config &config);

    uint32_t mmio_read(size_t addr) override { return simif.read(addr); }

    void mmio_write(size_t addr, uint32_t value) override {
      return simif.write(addr, value);
    }

    char *get_memory_base() override { return ((char *)cpu_mem.get_data()); }

    simif_emul_t &simif;

    /**
     * @brief A model of FPGA-addressable CPU-host memory.
     *
     * In metasimulations, FPGA-managed AXI4 transactions read and write to this
     * AXI4 memory subordinate as a proxy for writing into actual host-CPU DRAM.
     * The driver-side of FPGAManagedStreams inspect circular buffers hosted
     * here.
     */
    mm_magic_t cpu_mem;
  };

  std::unique_ptr<FPGAManagedStreamIOImpl> fpga_managed_stream_io;

private:
  /**
   * The flag is set when the simulation thread returns.
   *
   * If finished is set, the simulation thread can be joined into the target
   * thread to gracefully finalize the simulation.
   */
  std::atomic<bool> finished;

  /**
   * Exit code returned by the simulation, if it finished.
   */
  std::atomic<int> exit_code;

  /**
   * Identifier of the simulation thread.
   *
   * The main thread is the target thread.
   */
  emul_driver_thread_t thread;

  // Synchronisation primitives blocking the driver.  The driver thread
  // waits on the condition variable until the simulator allows it to proceed.
  std::mutex driver_mutex;
  std::condition_variable driver_cond;
  bool driver_flag;

  // Synchronisation primitives blocking the simulator.  The simulator thread
  // is the main thread, invoking the DPI tick function.  After information is
  // passed on, the simulator thread yields to the driver and waits on these
  // variables until the driver performs a tick.
  std::mutex rtlsim_mutex;
  std::condition_variable rtlsim_cond;
  bool rtlsim_flag;
};

#endif // __SIMIF_EMUL_H
