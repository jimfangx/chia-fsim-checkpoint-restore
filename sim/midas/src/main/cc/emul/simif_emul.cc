// See LICENSE for license details.

#include "simif_emul.h"

#include "bridges/cpu_managed_stream.h"
#include "bridges/fpga_managed_stream.h"
#include "core/simulation.h"
#include "emul/mm.h"
#include "emul/mmio.h"
#include "emul/harness_observation.h"
#include <cassert>
#include <climits>
#include <cinttypes>
#include <limits>
#include <memory>
#include <string.h>
#include <unistd.h>

simif_emul_t::simif_emul_t(const TargetConfig &config,
                           const std::vector<std::string> &args)
    : simif_t(config), master(std::make_unique<mmio_t>(config.ctrl)),
      driver_diagnostic(args) {
#ifndef VCS
  if (driver_diagnostic.settled())
    throw std::runtime_error("settled driver diagnostics require the VCS harness");
#endif

  // Parse arguments.
  memsize = 1L << config.mem.addr_bits;
  bool fastloadmem = false;
  int fpga_cnt = 1;
  int fpga_idx = 0;
  bool noc_part = false;
  bool comb_logic = false;
  uint64_t maximum_host_delay = 1, fuzz_seed = 0;
  for (auto arg : args) {
    if (arg.find("+fastloadmem") == 0) {
      fastloadmem = true;
    }
    if (arg.find("+memsize=") == 0) {
      memsize = strtoll(arg.c_str() + 9, NULL, 10);
    }
    if (arg.find("+fuzz-host-timing=") == 0) {
      maximum_host_delay = host_timing_t::parse_unsigned(arg.substr(18), INT_MAX);
    }
    if (arg.find("+fuzz-seed=") == 0) {
      fuzz_seed = host_timing_t::parse_unsigned(arg.substr(11), std::numeric_limits<uint64_t>::max());
      fprintf(stderr, "Using custom fuzzer seed: %" PRIu64 "\n", fuzz_seed);
    }
    if (arg.find("+loadmem=") == 0) {
      load_mem_path = arg.c_str() + 9;
    }
    if (arg.find("+partition-fpga-cnt=") == 0) {
      fpga_cnt = atoi(arg.c_str() + 20);
    }
    if (arg.find("+partition-fpga-idx=") == 0) {
      fpga_idx = atoi(arg.c_str() + 20);
    }
    if (arg.find("+partition-fpga-topo=") == 0) {
      int fpga_topo_str = atoi(arg.c_str() + 21);
      if (fpga_topo_str == 1)
        comb_logic = true;
      else if (fpga_topo_str == 2)
        noc_part = true;
    }
  }

  // Validate delay configuration before any external partition setup.
  host_timing = host_timing_t(maximum_host_delay, fuzz_seed);

  for (int i = 0; i < config.qsfp.num_channels; i++) {
    qsfp.emplace_back(std::make_unique<qsfp_t>(config.qsfp));
  }

  printf("qsfp_num_chans: %d\n", config.qsfp.num_channels);

  for (int idx = 0; idx < config.qsfp.num_channels; idx++) {
    char qsfp_owned_name[257];
    char qsfp_other_name[257];
    int next_fpga_idx = (fpga_idx + 1) % fpga_cnt;
    int prev_fpga_idx = (fpga_idx + fpga_cnt - 1) % fpga_cnt;

    printf("noc_part: %d comb_logic: %d\n", noc_part, comb_logic);

    // TODO : add argument in firesim manager
    if (!noc_part)
      assert(fpga_cnt <= 3);

    // If this is FPGA-0, reverse the qsfp channel shmem setup order to
    // avoid deadlocks
    int qsfp_chan =
        (fpga_idx == 0) ? idx : (config.qsfp.num_channels - 1 - idx);

    if (comb_logic) {
      qsfp_chan = idx;
    }

    if (fpga_cnt <= 2) {
      sprintf(qsfp_owned_name,
              "/qsfp%03d_%03d_to_%03d",
              qsfp_chan,
              fpga_idx,
              next_fpga_idx);
      sprintf(qsfp_other_name,
              "/qsfp%03d_%03d_to_%03d",
              qsfp_chan,
              next_fpga_idx,
              fpga_idx);
    } else if (noc_part) {
      if (qsfp_chan == 0) {
        sprintf(qsfp_owned_name, "/qsfp_%02d_to_%02d", fpga_idx, prev_fpga_idx);
        sprintf(qsfp_other_name, "/qsfp_%02d_to_%02d", prev_fpga_idx, fpga_idx);
      } else {
        sprintf(qsfp_owned_name, "/qsfp_%02d_to_%02d", fpga_idx, next_fpga_idx);
        sprintf(qsfp_other_name, "/qsfp_%02d_to_%02d", next_fpga_idx, fpga_idx);
      }
    } else {
      if (qsfp_chan == 0 && fpga_idx != 0) {
        sprintf(qsfp_owned_name, "/qsfp_%02d_to_%02d", fpga_idx, next_fpga_idx);
        sprintf(qsfp_other_name, "/qsfp_%02d_to_%02d", next_fpga_idx, fpga_idx);
      }
      if (qsfp_chan == 0 && fpga_idx == 0) {
        sprintf(qsfp_owned_name, "/qsfp_%02d_to_%02d", fpga_idx, prev_fpga_idx);
        sprintf(qsfp_other_name, "/qsfp_%02d_to_%02d", prev_fpga_idx, fpga_idx);
      }
      if (qsfp_chan == 1 && fpga_idx != 0) {
        sprintf(qsfp_owned_name, "/qsfp_%02d_to_%02d", fpga_idx, prev_fpga_idx);
        sprintf(qsfp_other_name, "/qsfp_%02d_to_%02d", prev_fpga_idx, fpga_idx);
      }
      if (qsfp_chan == 1 && fpga_idx == 0) {
        sprintf(qsfp_owned_name, "/qsfp_%02d_to_%02d", fpga_idx, next_fpga_idx);
        sprintf(qsfp_other_name, "/qsfp_%02d_to_%02d", next_fpga_idx, fpga_idx);
      }
    }
    qsfp[qsfp_chan]->setup_shmem(qsfp_owned_name, qsfp_other_name);
  }

  if (!fastloadmem)
    load_mem_path.clear();

  // Initialise memories.
  for (unsigned i = 0; i < config.mem_num_channels; ++i) {
    slave.emplace_back(std::make_unique<mm_magic_t>(config.mem));
    (*slave.rbegin())->init(memsize, 64);
  }

  // Set up the managed stream.
  if (std::optional<AXI4Config> conf = config.cpu_managed) {
    assert(!config.fpga_managed && "stream should be CPU or FPGA managed");
    cpu_managed_stream_io.reset(new CPUManagedStreamIOImpl(*this, *conf));
  }

  if (std::optional<AXI4Config> conf = config.fpga_managed) {
    assert(!config.cpu_managed && "stream should be CPU or FPGA managed");
    fpga_managed_stream_io.reset(new FPGAManagedStreamIOImpl(*this, *conf));
  }

  std::cout << "simif_emul_t: done" << std::endl;
}

simif_emul_t::~simif_emul_t() { thread.cancel_reserved(); }

std::vector<std::pair<std::string, checkpoint_owner_t *>>
simif_emul_t::checkpoint_axi_owners() {
  std::vector<std::pair<std::string, checkpoint_owner_t *>> owners{
      {"runtime/mmio", master.get()}};
  for (size_t i = 0; i < slave.size(); ++i)
    owners.emplace_back("runtime/functional-memory/" + std::to_string(i), slave[i].get());
  if (cpu_managed_stream_io)
    owners.emplace_back("runtime/cpu-managed-axi4", &cpu_managed_stream_io->cpu_managed_axi4);
  if (fpga_managed_stream_io)
    owners.emplace_back("runtime/fpga-managed-memory", &fpga_managed_stream_io->cpu_mem);
  return owners;
}

std::vector<std::pair<std::string, checkpoint_owner_t *>>
simif_emul_t::checkpoint_stream_io_owners() {
  std::vector<std::pair<std::string, checkpoint_owner_t *>> owners;
  if (cpu_managed_stream_io)
    owners.emplace_back("runtime/cpu-managed-stream-io", cpu_managed_stream_io.get());
  if (fpga_managed_stream_io)
    owners.emplace_back("runtime/fpga-managed-stream-io", nullptr);
  return owners;
}

void simif_emul_t::start_driver(simulation_t &sim) {
  thread.require_controller_if_started();
  if (loading_driver)
    throw std::runtime_error("reserved restore destination cannot start ordinarily");
  if (restored_driver || restore_consumed)
    throw std::runtime_error("ordinary startup of an armed restored dispatcher");
  start_driver_impl(sim, false);
}

void simif_emul_t::require_restored_destination(simulation_t &sim) {
  thread.require_controller_if_started();
  if (driver_started || driver_diagnostic.requested() ||
      cpu_managed_stream_io || fpga_managed_stream_io || !qsfp.empty() || slave.size() > 4)
    throw std::runtime_error("unsupported/nonfresh restored dispatcher destination");
  sim.require_restored_dispatcher();
  host_timing.validate_checkpoint(host_timing.save_checkpoint());
}

void simif_emul_t::arm_restored_driver(simulation_t &sim) {
  thread.require_controller_if_started();
  if (restored_driver || restore_consumed)
    throw std::runtime_error("restored dispatcher already armed/consumed");
  if (loading_driver && loading_driver != &sim)
    throw std::runtime_error("reserved restore driver mismatch");
  require_restored_destination(sim);
  loading_driver = nullptr;
  restored_driver = &sim;
}

void simif_emul_t::begin_restored_load(simulation_t &sim) {
  thread.require_controller_if_started();
  if (loading_driver || restored_driver || restore_consumed || driver_started ||
      driver_diagnostic.requested() || cpu_managed_stream_io || fpga_managed_stream_io ||
      !qsfp.empty() || slave.size()>4)
    throw std::runtime_error("unsupported/nonfresh restore loading destination");
  sim.require_fresh_dispatcher();
  if (!sim.simulation_supports_steps())
    throw std::runtime_error("restore requires incremental simulation driver");
  for (const auto &owners : {sim.checkpoint_driver_owners(), sim.checkpoint_widget_owners()})
    for (const auto &[name, owner] : owners)
      if (!owner || owner->checkpoint_schema().empty())
        throw std::runtime_error("unsupported restore loading owner: " + name);
  loading_driver = &sim;
}

void simif_emul_t::reserve_restored_creator(simulation_t &sim) {
  if (loading_driver != &sim || restored_driver || restore_consumed || driver_started)
    throw std::runtime_error("creator reservation requires fresh restore loading hold");
  sim.require_fresh_dispatcher();
  thread.reserve();
}

void simif_emul_t::prepare_on_restored_creator(
    simulation_t &sim, emul_driver_thread_t::task_t prepare) {
  if (loading_driver != &sim || restored_driver || restore_consumed || driver_started)
    throw std::runtime_error("creator preparation requires restore loading hold");
  thread.invoke(std::move(prepare));
}

void simif_emul_t::retain_on_restored_creator(
    simulation_t &sim, std::unique_ptr<emul_driver_thread_t::resource_t> resource) {
  if (loading_driver != &sim || restored_driver || restore_consumed || driver_started)
    throw std::runtime_error("creator lifetime requires restore loading hold");
  thread.retain(std::move(resource));
}

void simif_emul_t::prepare_restored_launch(simulation_t &sim) {
  thread.require_controller_if_started();
  if (loading_driver != &sim || restored_driver || restore_consumed || driver_started ||
      prepared_launch_driver || !thread.reserved())
    throw std::runtime_error("launch preparation requires idle restore loading creator");
  sim.require_fresh_dispatcher();
  thread.prepare_launch(make_driver_task(sim, true));
  prepared_launch_driver = &sim;
}

void simif_emul_t::prepare_restored_publication(
    simulation_t &sim, emul_driver_thread_t::task_t publish) {
  thread.require_controller_if_started();
  if (loading_driver != &sim || restored_driver || restore_consumed || driver_started ||
      prepared_launch_driver != &sim)
    throw std::runtime_error("owner publication preparation requires prepared loading creator");
  sim.require_fresh_dispatcher();
  thread.prepare_publication(std::move(publish));
}

void simif_emul_t::publish_restored_owners(simulation_t &sim) {
  thread.require_controller_if_started();
  if (loading_driver != &sim || restored_driver || restore_consumed || driver_started ||
      prepared_launch_driver != &sim)
    throw std::runtime_error("owner publication requires sealed loading creator");
  sim.require_fresh_dispatcher();
  thread.publish_prepared();
}

void simif_emul_t::seal_restored_launch(
    simulation_t &sim, emul_driver_thread_t::publication_factory_t prepare) {
  thread.require_controller_if_started();
  if (loading_driver != &sim || restored_driver || restore_consumed || driver_started ||
      prepared_launch_driver != &sim)
    throw std::runtime_error("publication seal requires prepared restore loading creator");
  sim.require_fresh_dispatcher();
  thread.seal_launch(std::move(prepare));
}

bool simif_emul_t::resume_restored_driver(uint32_t bits, const uint32_t *banks) {
  thread.require_controller_if_started();
  if (!restored_driver || restore_consumed)
    throw std::runtime_error("missing/consumed restored dispatcher");
  auto &sim = *restored_driver;
  // Any failure poisons this fresh process; it cannot use ordinary startup.
  restored_driver = nullptr;
  try {
    bool result = start_restored_driver(sim, bits, banks);
    restore_consumed = true;
    return result;
  } catch (...) {
    restore_consumed = true;
    throw;
  }
}

bool simif_emul_t::start_restored_driver(simulation_t &sim, uint32_t bits,
                                         const uint32_t *banks) {
  if (restored_driver || loading_driver || restore_consumed)
    throw std::runtime_error("armed dispatcher must resume through the harness");
  require_restored_destination(sim);
  harness_observation_t::require_clock_phase(bits, banks, true);
  for (const uint32_t flag_bit : {bits - 66, bits - 67})
    if ((banks[flag_bit / 32] >> (flag_bit % 32)) & 1)
      throw std::runtime_error("restored dispatcher requires reset/finish deasserted");
  return start_driver_impl(sim, true);
}

bool simif_emul_t::start_driver_impl(simulation_t &sim, bool restored) {
  if (driver_started)
    throw std::runtime_error("driver dispatcher already started");
  if (!restored)
    sim.require_fresh_dispatcher();
  if ((driver_diagnostic.requested() || checkpoint_polling) && !sim.simulation_supports_steps()) {
    fprintf(stderr, "FS_DRIVER_BOUNDARY_ERROR: unsupported incremental simulation driver\n");
    std::exit(EXIT_FAILURE);
  }
  if (prepared_launch_driver && (!restored || prepared_launch_driver != &sim))
    throw std::runtime_error("prepared driver continuation destination mismatch");
  // Build every std::function capture before changing dispatcher state. The
  // optional restore coordinator has already staged these before publication.
  auto task = prepared_launch_driver ? emul_driver_thread_t::task_t{} : make_driver_task(sim, restored);
  // Set up the simulation thread.
  driver_started = true;
  finished = false;
  exit_code = EXIT_FAILURE;
  {
    std::unique_lock<std::mutex> lock(rtlsim_mutex);
    driver_flag = false;
    rtlsim_flag = false;

    if (prepared_launch_driver) {
      thread.launch_prepared();
      prepared_launch_driver = nullptr;
    } else {
      thread.launch(std::move(task));
    }

    // Wait for the target thread to yield and enter the RTL simulator.
    // A restored terminal cut can finish without yielding.
    rtlsim_cond.wait(lock, [&] { return rtlsim_flag || finished || settled_pending; });
  }
  return finished;
}

emul_driver_thread_t::task_t simif_emul_t::make_driver_task(simulation_t &sim, bool restored) {
  simulation_t::run_boundary_t boundary;
  if (driver_diagnostic.requested() || checkpoint_polling)
    boundary = [this](simulation_t &driver, bool done) { capture_driver_boundary(driver, done); };
  return [this, &sim, restored, boundary = std::move(boundary)] {
      if (!restored)
        do_tick();

      // Load memories before initialising the simulation.
      if (!restored && !load_mem_path.empty()) {
        fprintf(stdout,
                "[fast loadmem] loadmem path exists, load simulation memories "
                "without loadmem_t widget, path = %s\n",
                load_mem_path.c_str());
        load_mems(load_mem_path.c_str());
      }

      // Run the simulation flow.
      try {
        exit_code = sim.execute_simulation_flow(boundary);
        driver_diagnostic.finish();
      } catch (const std::exception &error) {
        fprintf(stderr, "FS_DRIVER_BOUNDARY_ERROR: %s\n", error.what());
        // VCS is waiting in DPI. Its exit handlers can wait for that thread,
        // deadlocking a worker-thread std::exit(). Fail without exit handlers;
        // the diagnostic is unpublished/closed before reaching this catch.
        std::fflush(nullptr);
        std::_Exit(EXIT_FAILURE);
      }

      // Wake the target thread before returning from the simulation thread.
      {
        std::unique_lock<std::mutex> lock(rtlsim_mutex);
        finished = true;
        rtlsim_cond.notify_one();
      }
    };
}

void simif_emul_t::wait_write(mmio_t &mmio) {
  while (!mmio.write_resp())
    advance_target();
}

void simif_emul_t::wait_read(mmio_t &mmio, void *data) {
  while (!mmio.read_resp(data))
    advance_target();
}

void simif_emul_t::write(size_t addr, uint32_t data) {
  require_no_driver_capture();
  uint64_t size = master->get_config().get_size();
  assert(size == 2 && "AXI4-lite control interface has unexpected size");
  uint64_t strb = (1 << master->get_config().strb_bits()) - 1;
  master->write_req(addr, size, 0, &data, &strb);
  wait_write(*master);
}

uint32_t simif_emul_t::read(size_t addr) {
  require_no_driver_capture();
  uint64_t size = master->get_config().get_size();
  assert(size == 2 && "AXI4-lite control interface has unexpected size");
  uint32_t data;
  master->read_req(addr, size, 0);
  wait_read(*master, &data);
  return data;
}

#define MAX_LEN 255

size_t simif_emul_t::CPUManagedStreamIOImpl::cpu_managed_axi4_read(
    size_t addr, char *data, size_t size) {
  simif.require_no_driver_capture();
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  const uint64_t beat_bytes = cpu_managed_axi4.get_config().beat_bytes();
  if (!size) return 0;
  if (size % beat_bytes || size > size_t(SSIZE_MAX))
    throw std::runtime_error("unsupported CPU-managed DMA read size");
  ssize_t len = (size - 1) / beat_bytes;

  while (len >= 0) {
    size_t part_len = len % (MAX_LEN + 1);

    cpu_managed_axi4.read_req(addr, log2(beat_bytes), part_len);
    simif.wait_read(cpu_managed_axi4, data);

    len -= (part_len + 1);
    addr += (part_len + 1) * beat_bytes;
    data += (part_len + 1) * beat_bytes;
  }
  return size;
}

size_t simif_emul_t::CPUManagedStreamIOImpl::cpu_managed_axi4_write(
    size_t addr, const char *data, size_t size) {
  simif.require_no_driver_capture();
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  const uint64_t beat_bytes = cpu_managed_axi4.get_config().beat_bytes();
  if (!size) return 0;
  if (size % beat_bytes || size > size_t(SSIZE_MAX))
    throw std::runtime_error("unsupported CPU-managed DMA write size");
  ssize_t len = (size - 1) / beat_bytes;
  const size_t remaining = size - len * beat_bytes;
  size_t strb[len + 1];
  size_t *strb_ptr = &strb[0];

  for (int i = 0; i < len; i++) {
    strb[i] = beat_bytes > 63 ? -1 : ((1LL << beat_bytes) - 1);
  }

  if (remaining == beat_bytes && len > 0)
    strb[len] = strb[0];
  else if (remaining == beat_bytes)
    strb[len] = -1;
  else
    strb[len] = (1LL << remaining) - 1;

  while (len >= 0) {
    const size_t part_len = len % (MAX_LEN + 1);

    cpu_managed_axi4.write_req(
        addr, log2(beat_bytes), part_len, data, strb_ptr);
    simif.wait_write(cpu_managed_axi4);

    len -= (part_len + 1);
    addr += (part_len + 1) * beat_bytes;
    data += (part_len + 1) * beat_bytes;
    strb_ptr += (part_len + 1);
  }

  return size;
}

simif_emul_t::FPGAManagedStreamIOImpl::FPGAManagedStreamIOImpl(
    simif_emul_t &simif, const AXI4Config &config)
    : simif(simif), cpu_mem(config) {
  // The final parameter, line size, is not used under mm_magic_t
  cpu_mem.init((1ULL << cpu_mem.get_config().addr_bits), 512);
}

void simif_emul_t::load_mems(const char *fname) {
  // TODO: allow file to be split across slaves
  if (!slave.empty()) {
    slave[0]->load_mem(0, fname);
  }
}

int simif_emul_t::end() {
  assert(finished && "simulation not yet finished");
  thread.join();
  return exit_code;
}

void simif_emul_t::do_tick() {
  require_no_driver_capture();
  {
    std::unique_lock<std::mutex> lock(driver_mutex);
    driver_flag = false;
  }
  {
    std::unique_lock<std::mutex> lock(rtlsim_mutex);
    rtlsim_flag = true;
    rtlsim_cond.notify_one();
  }
  {
    std::unique_lock<std::mutex> lock(driver_mutex);
    driver_cond.wait(lock, [&] { return driver_flag; });
  }
}

bool simif_emul_t::to_sim() {
  if (!driver_started)
    throw std::runtime_error("driver dispatcher has not started");
  {
    std::unique_lock<std::mutex> lock(rtlsim_mutex);
    rtlsim_flag = false;
  }
  {
    std::unique_lock<std::mutex> lock(driver_mutex);
    driver_flag = true;
    driver_context_active = true;
    driver_cond.notify_one();
  }
  {
    std::unique_lock<std::mutex> lock(rtlsim_mutex);
    rtlsim_cond.wait(lock, [&] { return rtlsim_flag || finished || settled_pending; });
  }
  driver_context_active = false;
  return finished;
}

void simif_emul_t::capture_driver_boundary(simulation_t &sim, bool done) {
  if (std::this_thread::get_id() != thread.get_id() || !driver_context_active)
    throw std::runtime_error(
        "driver boundary capture outside parked DPI driver context");
  require_no_driver_capture();
  driver_capture_active = true;
  try {
    driver_boundary_diagnostic_t::owners_t owners;
    if (driver_diagnostic.due()) {
      owners = checkpoint_axi_owners();
      owners.push_back(checkpoint_host_timing_owner());
      const auto widgets = sim.checkpoint_widget_owners();
      owners.insert(owners.end(), widgets.begin(), widgets.end());
      const auto drivers = sim.checkpoint_driver_owners();
      owners.insert(owners.end(), drivers.begin(), drivers.end());
      // Every missing category remains explicit. In particular, parked DPI
      // is not settled RTL; harness AXI latches/NBA/clock phase are not
      // captured.
      owners.emplace_back("generated/rtl", nullptr);
      owners.emplace_back("runtime/harness", nullptr);
      owners.emplace_back("runtime/execution-context", nullptr);
      const auto streams = checkpoint_stream_io_owners();
      owners.insert(owners.end(), streams.begin(), streams.end());
      for (size_t i = 0; i < qsfp.size(); ++i)
        owners.emplace_back("runtime/qsfp/" + std::to_string(i), nullptr);
    }
    if (checkpoint_polling || (driver_diagnostic.settled() && driver_diagnostic.due())) {
      // Freeze on the driver creator, while VCS is still parked in DPI. The
      // settled callback must never invoke mutable or creator-bound owners.
      // Empty polling inventories have no diagnostic owner work to transfer.
      auto snapshot = owners.empty() ? driver_boundary_diagnostic_t::snapshot_t{}
                                    : driver_boundary_diagnostic_t::freeze(owners);
      std::unique_lock<std::mutex> lock(settled_mutex);
      public_capture_published = false;
      settled_owners = std::move(snapshot);
      {
        std::unique_lock<std::mutex> rtl_lock(rtlsim_mutex);
        settled_pending = true;
        rtlsim_cond.notify_one();
      }
      settled_cond.wait(lock, [&] { return !settled_pending; });
      settled_owners = driver_boundary_diagnostic_t::snapshot_t{};
    } else {
      driver_diagnostic.observe(owners);
    }
    if (done)
      driver_diagnostic.finish();
    driver_capture_active = false;
  } catch (...) {
    driver_capture_active = false;
    throw;
  }
}

void simif_emul_t::require_settled_checkpoint_capture() const {
  if (std::this_thread::get_id() == thread.get_id() || driver_context_active ||
      !settled_pending || !driver_capture_active || !checkpoint_lowclock() ||
      cpu_managed_stream_io || fpga_managed_stream_io || !qsfp.empty() || slave.size() > 4)
    throw std::runtime_error("live capture outside parked low-clock driver boundary");
}

void simif_emul_t::acknowledge_checkpoint_publication() {
  require_settled_checkpoint_capture();
  if (!public_capture_required || public_capture_published)
    throw std::runtime_error("unexpected/duplicate public checkpoint publication");
  public_capture_published = true;
}

bool simif_emul_t::checkpoint_settled(uint32_t bits, const uint32_t *banks) {
  if (std::this_thread::get_id() == thread.get_id() || driver_context_active ||
      !settled_pending || !driver_capture_active)
    throw std::runtime_error("settled capture outside parked driver boundary");
  harness_observation_t::require_clock_phase(bits, banks, checkpoint_lowclock());
  if (public_capture_required && !public_capture_published)
    throw std::runtime_error("public checkpoint callback did not complete publication");
  {
    std::unique_lock<std::mutex> lock(settled_mutex);
    harness_observation_t observation(bits, banks);
    auto owners = settled_owners.owners();
    owners.emplace_back("diagnostic/harness-banks", &observation);
    driver_diagnostic.observe(owners);
    // Resume the existing operation dispatcher until its next normal yield.
    // The DPI caller will refresh the output banks before the next RTL edge.
    driver_context_active = true;
    settled_pending = false;
    settled_cond.notify_one();
  }
  {
    std::unique_lock<std::mutex> lock(rtlsim_mutex);
    // A resumed operation can finish entirely in software and immediately
    // park again, without another MMIO yield or target edge.
    rtlsim_cond.wait(lock, [&] { return rtlsim_flag || finished || settled_pending; });
  }
  driver_context_active = false;
  return finished;
}

void simif_emul_t::require_no_driver_capture() const {
  if (!driver_started && (loading_driver || restored_driver || restore_consumed))
    throw std::runtime_error("MMIO/RTL advancement during restore loading hold");
  if (driver_capture_active)
    throw std::runtime_error(
        "MMIO/RTL advancement during driver boundary capture");
}
