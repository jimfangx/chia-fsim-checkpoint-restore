// See LICENSE for license details.

#include "simulation.h"
#include "bridges/clock.h"
#include "bridges/loadmem.h"
#include "bridges/master.h"
#include "core/bridge_driver.h"
#include "core/simif.h"
#include "core/stream_engine.h"
#include "core/timing.h"

#include <cassert>
#include <cinttypes>
#include <cstdio>

simulation_t::simulation_t(widget_registry_t &registry,
                           const std::vector<std::string> &args)
    : registry(registry), clock(registry.get_widget<clockmodule_t>()) {
  bool fastloadmem = false;
  for (auto &arg : args) {
    if (arg.find("+fastloadmem") == 0) {
      fastloadmem = true;
    }
    if (arg.find("+loadmem=") == 0) {
      load_mem_path = arg.c_str() + 9;
    }
    if (arg.find("+zero-out-dram") == 0) {
      do_zero_out_dram = true;
    }
    if (arg.find("+check-fingerprint") == 0) {
      check_fingerprint_only = true;
    }
    if (arg.find("+write-fingerprint=") == 0) {
      write_fingerprint_only = atoi(arg.c_str() + 19);
    }
  }

  if (fastloadmem)
    load_mem_path.clear();
}

void simulation_t::record_start_times() {
  start_hcycle = clock.hcycle();
  start_time = timestamp();
}

void simulation_t::record_end_times() {
  end_time = timestamp();
  end_tcycle = clock.tcycle();
  end_hcycle = clock.hcycle();
}

void simulation_t::print_simulation_performance_summary() {
  // Must call record_start_times and record_end_times before invoking this
  // function
  assert(start_hcycle.has_value() && end_hcycle.has_value() &&
         "simulation not executed");

  const uint64_t hcycles = *end_hcycle - *start_hcycle;
  const double sim_time = diff_secs(end_time, start_time);
  const double sim_speed = ((double)end_tcycle) / (sim_time * 1000.0);
  const double measured_host_frequency =
      ((double)hcycles) / (sim_time * 1000.0);
  const double fmr = ((double)hcycles / end_tcycle);

  fprintf(stderr, "\nEmulation Performance Summary\n");
  fprintf(stderr, "------------------------------\n");
  fprintf(stderr, "Wallclock Time Elapsed: %.1f s\n", sim_time);
  // Provide enough sig-figs to let the report be useful in RTL sim
  fprintf(stderr, "Host Frequency: ");
  if (measured_host_frequency > 1000.0) {
    fprintf(stderr, "%.3f MHz\n", measured_host_frequency / 1000.0);
  } else {
    fprintf(stderr, "%.3f KHz\n", measured_host_frequency);
  }

  fprintf(stderr, "Target Cycles Emulated: %" PRIu64 "\n", end_tcycle);
  fprintf(stderr, "Effective Target Frequency: ");
  if (sim_speed > 1000.0) {
    fprintf(stderr, "%.3f MHz\n", sim_speed / 1000.0);
  } else {
    fprintf(stderr, "%.3f KHz\n", sim_speed);
  }
  fprintf(stderr, "FMR: %.2f\n", fmr);
  fprintf(stderr,
          "Note: The latter three figures are based on the fastest "
          "target clock.\n");
}

void simulation_t::simulation_init() {
  for (auto *bridge : registry.get_all_bridges()) {
    bridge->init();
  }
}

void simulation_t::simulation_finish() {
  for (auto *bridge : registry.get_all_bridges()) {
    bridge->finish();
  }
}

std::vector<std::pair<std::string, checkpoint_owner_t *>>
simulation_t::checkpoint_widget_owners() {
  return registry.checkpoint_owners();
}

int simulation_t::execute_simulation_flow(const run_boundary_t &boundary) {
  if (flow_on_stack || flow_poisoned ||
      (flow_phase != flow_phase_t::constructed &&
       flow_phase != flow_phase_t::running))
    throw std::runtime_error("reentrant/interrupted/completed simulation flow");
  flow_on_stack = true;
  try {
    const int result = execute_simulation_flow_impl(boundary);
    flow_on_stack = false;
    return result;
  } catch (...) {
    flow_poisoned = true;
    flow_on_stack = false;
    throw;
  }
}

void simulation_t::require_restored_dispatcher() {
  require_checkpoint_boundary();
  if (flow_on_stack || !base_checkpoint_loaded)
    throw std::runtime_error("dispatcher bootstrap requires a freshly loaded flow");
  // Fail before starting a worker or submitting MMIO for an unknown concrete
  // driver, callback, or widget. Each component still has its own load hook.
  auto owners = checkpoint_driver_owners();
  const auto widgets = checkpoint_widget_owners();
  owners.insert(owners.end(), widgets.begin(), widgets.end());
  for (const auto &[id, owner] : owners) {
    if (!owner || owner->checkpoint_schema().empty())
      throw std::runtime_error("unsupported dispatcher owner: " + id);
    const auto payload = owner->save_checkpoint();
    owner->validate_checkpoint(payload);
  }
}

void simulation_t::require_fresh_dispatcher() const {
  if (flow_on_stack || flow_poisoned || base_checkpoint_loaded ||
      flow_phase != flow_phase_t::constructed)
    throw std::runtime_error("ordinary dispatcher start requires a fresh flow");
}

int simulation_t::execute_simulation_flow_impl(const run_boundary_t &boundary) {
  // Reject before initialization/MMIO if a coordinator requests boundaries
  // from a driver that cannot return at completed host operations.
  if (boundary && !simulation_supports_steps())
    throw std::runtime_error("unsupported incremental simulation driver");
  if (flow_phase == flow_phase_t::constructed) {
    widget_checkpoint_state_t::call_t call(flow_call);
    flow_phase = flow_phase_t::initializing;
    fprintf(stderr, "entered simulation flow execution\n");
    wait_for_init();
    fprintf(stderr, "finished waiting\n");

    // following fingerprint logic uses 'exit' instead of 'return' to avoid
    // issues w/ deconstructors not having initialized values
    auto &master = registry.get_widget<master_t>();
    if (check_fingerprint_only || write_fingerprint_only.has_value()) {
      if (check_fingerprint_only && write_fingerprint_only.has_value()) {
        fprintf(stderr, "Unable to both check/write FireSim fingerprint\n");
        exit(EXIT_FAILURE);
      }

      if (check_fingerprint_only && master.check_fingerprint()) {
        fprintf(stderr, "Invalid FireSim fingerprint\n");
        exit(EXIT_FAILURE);
      }
      if (write_fingerprint_only.has_value()) {
        master.write_fingerprint(write_fingerprint_only.value());
      }
      exit(EXIT_SUCCESS);
    } else {
      if (master.check_fingerprint()) {
        fprintf(stderr, "Invalid FireSim fingerprint\n");
        exit(EXIT_FAILURE);
      }
    }

    if (auto *fpga_stream = registry.get_fpga_stream_engine()) {
      fpga_stream->init();
    }

    if (auto *stream = registry.get_stream_engine()) {
      stream->init();
    }

    init_dram();

    simulation_init();

    record_start_times();
    fprintf(stderr, "Commencing simulation.\n");
    flow_phase = flow_phase_t::running;
  }
  int exit_code;
  if (simulation_supports_steps()) {
    while (!run_done) {
      {
        widget_checkpoint_state_t::call_t call(flow_call);
        run_done = simulation_run_step();
      }
      if (boundary)
        boundary(*this, run_done);
    }
    widget_checkpoint_state_t::call_t call(flow_call);
    flow_phase = flow_phase_t::finalizing;
    exit_code = simulation_run_finalize();
  } else {
    widget_checkpoint_state_t::call_t call(flow_call);
    exit_code = simulation_run();
    flow_phase = flow_phase_t::finalizing;
  }
  widget_checkpoint_state_t::call_t call(flow_call);
  fprintf(stderr, "\nSimulation complete.\n");
  record_end_times();

  simulation_finish();

  const bool timeout = simulation_timed_out();

  if (exit_code != 0) {
    fprintf(stderr,
            "*** FAILED *** (code = %d) after %" PRIu64 " cycles\n",
            exit_code,
            end_tcycle);
  } else if (timeout) {
    fprintf(stderr,
            "*** FAILED *** simulation timed out after %" PRIu64 " cycles\n",
            end_tcycle);
  } else {
    fprintf(stderr, "*** PASSED *** after %" PRIu64 " cycles\n", end_tcycle);
  }

  print_simulation_performance_summary();

  flow_phase = flow_phase_t::completed;
  return timeout ? EXIT_FAILURE : exit_code;
}

widget_checkpoint_config_t simulation_t::checkpoint_configuration() const {
  widget_checkpoint_config_t config(8,
                                    {do_zero_out_dram,
                                     check_fingerprint_only,
                                     write_fingerprint_only.has_value(),
                                     write_fingerprint_only.value_or(0),
                                     simulation_supports_steps()});
  config.text(target_name);
  config.text(load_mem_path);
  const auto owners = registry.checkpoint_owners();
  config.word(owners.size());
  for (const auto &[id, owner] : owners) {
    config.text(id);
    config.text(owner->checkpoint_schema());
  }
  return config;
}

void simulation_t::require_checkpoint_boundary() const {
  flow_call.require_idle();
  if (flow_poisoned || flow_phase != flow_phase_t::running ||
      !simulation_supports_steps() || check_fingerprint_only ||
      write_fingerprint_only || !start_hcycle || end_hcycle || end_time ||
      end_tcycle)
    throw std::runtime_error(
        "unsupported simulation lifecycle checkpoint boundary");
}

std::vector<uint8_t> simulation_t::base_checkpoint_t::save_checkpoint() const {
  sim.require_checkpoint_boundary();
  auto config = sim.checkpoint_configuration();
  config.word(1); // initialized, running; terminal output has not started
  config.word(sim.run_done);
  config.word(sim.start_time);
  config.word(*sim.start_hcycle);
  return std::move(config.bytes);
}

void simulation_t::base_checkpoint_t::validate_checkpoint(
    const std::vector<uint8_t> &payload) const {
  sim.flow_call.require_idle();
  if (sim.flow_poisoned || !sim.simulation_supports_steps() ||
      sim.check_fingerprint_only || sim.write_fingerprint_only ||
      (sim.flow_phase != flow_phase_t::constructed &&
       sim.flow_phase != flow_phase_t::running))
    throw std::runtime_error(
        "unsupported simulation lifecycle restore destination");
  auto config = sim.checkpoint_configuration();
  if (payload.size() != config.bytes.size() + 32 ||
      !std::equal(config.bytes.begin(), config.bytes.end(), payload.begin()) ||
      payload[config.bytes.size()] != 1 ||
      std::any_of(payload.begin() + config.bytes.size() + 1,
                  payload.begin() + config.bytes.size() + 8,
                  [](uint8_t value) { return value != 0; }) ||
      payload[config.bytes.size() + 8] > 1 ||
      std::any_of(payload.begin() + config.bytes.size() + 9,
                  payload.begin() + config.bytes.size() + 16,
                  [](uint8_t value) { return value != 0; }))
    throw std::runtime_error(
        "simulation lifecycle configuration/payload mismatch");
}

void simulation_t::base_checkpoint_t::load_checkpoint(
    const std::vector<uint8_t> &payload) {
  validate_checkpoint(payload);
  if (sim.flow_on_stack || sim.flow_phase != flow_phase_t::constructed)
    throw std::runtime_error("simulation lifecycle load requires a fresh flow");
  const size_t offset = payload.size() - 16;
  uint64_t start = 0, hcycle = 0;
  for (unsigned i = 0; i < 8; ++i) {
    start |= uint64_t(payload[offset + i]) << (8 * i);
    hcycle |= uint64_t(payload[offset + 8 + i]) << (8 * i);
  }
  // No timestamp, MMIO, initialization, file output or target edge on load.
  // The historical wall-clock baseline is reporting state, never target time.
  publication.changed();
  sim.start_time = start;
  sim.start_hcycle = hcycle;
  sim.run_done = payload[payload.size() - 24];
  sim.flow_phase = flow_phase_t::running;
  sim.base_checkpoint_loaded = true;
}

std::vector<uint8_t>
simulation_t::base_checkpoint_t::destination_stamp() const {
  sim.flow_call.require_idle();
  sim.require_fresh_dispatcher();
  // A fresh flow cannot use source save: that requires a running lifecycle.
  // Bind every base-flow field as well as the complete effective configuration
  // and ordered widget registry. The call-state guard separately rejects an
  // active or poisoned synchronous operation.
  auto stamp = sim.checkpoint_configuration();
  stamp.word(static_cast<uint64_t>(sim.flow_phase));
  stamp.word(sim.flow_on_stack);
  stamp.word(sim.flow_poisoned);
  stamp.word(sim.base_checkpoint_loaded);
  stamp.word(sim.run_done);
  stamp.word(sim.start_time);
  stamp.word(sim.end_time);
  stamp.word(sim.start_hcycle.has_value());
  stamp.word(sim.start_hcycle.value_or(0));
  stamp.word(sim.end_hcycle.has_value());
  stamp.word(sim.end_hcycle.value_or(0));
  stamp.word(sim.end_tcycle);
  return std::move(stamp.bytes);
}

class simulation_t::base_checkpoint_t::restore_t final
    : public prepared_checkpoint_t {
  base_checkpoint_t &owner;
  checkpoint_publication_t::guard_t guard;
  const std::vector<uint8_t> expected;
  uint64_t start = 0, hcycle = 0;
  bool done = false;
public:
  restore_t(base_checkpoint_t &owner, const std::vector<uint8_t> &payload)
      : owner(owner), guard(owner.publication),
        expected(owner.destination_stamp()) {
    owner.validate_checkpoint(payload);
    const size_t offset = payload.size() - 16;
    for (unsigned i = 0; i < 8; ++i) {
      start |= uint64_t(payload[offset + i]) << (8 * i);
      hcycle |= uint64_t(payload[offset + 8 + i]) << (8 * i);
    }
    done = payload[payload.size() - 24];
  }
  void seal() override {
    guard.require_unsealed(); // Check lifetime/thread/generation before owner access.
    if (owner.destination_stamp() != expected)
      throw std::runtime_error("simulation lifecycle checkpoint destination changed");
    guard.seal();
  }
  void publish() noexcept override {
    guard.publish();
    // Scalars and a trivial optional only. No init/reset, timestamp, MMIO,
    // output, concrete-driver service or potentially allocating legacy load.
    owner.sim.start_time = start;
    owner.sim.start_hcycle = hcycle;
    owner.sim.run_done = done;
    owner.sim.flow_phase = flow_phase_t::running;
    owner.sim.base_checkpoint_loaded = true;
  }
};

std::unique_ptr<prepared_checkpoint_t>
simulation_t::base_checkpoint_t::prepare_checkpoint_publication(
    const std::vector<uint8_t> &payload) {
  return std::make_unique<restore_t>(*this, payload);
}

void simulation_t::wait_for_init() {
  auto &master = registry.get_widget<master_t>();
  while (!master.is_init_done())
    ;
}

void simulation_t::init_dram() {
  if (auto *loadmem = registry.get_widget_opt<loadmem_t>()) {
    if (do_zero_out_dram) {
      fprintf(stderr,
              "Zeroing out FPGA DRAM. This will take a few seconds...\n");
      loadmem->zero_out_dram();
    }

    if (!load_mem_path.empty()) {
      loadmem->load_mem_from_file(load_mem_path);
    }
  } else {
    if (do_zero_out_dram || !load_mem_path.empty()) {
      fprintf(stderr,
              "Skipping memory initialization: target does not use DRAM\n");
    }
  }
}
