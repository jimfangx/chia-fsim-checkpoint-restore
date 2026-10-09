// See LICENSE for license details.

#include "bridges/clock.h"
#include "bridges/fased_memory_timing_model.h"
#include "bridges/peek_poke.h"
#include "bridges/reset_pulse.h"
#include "core/bridge_driver.h"
#include "core/simulation.h"
#include "core/systematic_scheduler.h"
#include "core/stepped_run_loop.h"

class fasedtests_top_t : public systematic_scheduler_t, public simulation_t,
                         private stepped_run_loop_t::services_t {
public:
  fasedtests_top_t(simif_t &simif,
                  widget_registry_t &registry,
                  const std::vector<std::string> &args);

  int simulation_run() override;
  bool simulation_supports_steps() const override { return true; }
  bool simulation_run_step() override {
    widget_checkpoint_state_t::call_t call(checkpoint_state);
    if (finalized)
      throw std::runtime_error("FCFS operation after finalization");
    started = true;
    run_loop.advance(*this);
    return run_loop.finished();
  }
  int simulation_run_finalize() override;

  bool simulation_timed_out() override { return !run_loop.target_done(); }

  std::vector<std::pair<std::string, checkpoint_owner_t *>>
  checkpoint_driver_owners() override {
    auto owners = simulation_t::checkpoint_driver_owners();
    owners.front().second = &driver_checkpoint;
    auto scheduler_owners = systematic_scheduler_t::checkpoint_owners();
    owners.insert(owners.end(), scheduler_owners.begin(), scheduler_owners.end());
    owners.emplace_back("runtime/simulation/fased-run-loop", &run_loop);
    return owners;
  }

private:
  // Use a separate adapter: overriding checkpoint_owner_t on this multiply
  // inherited driver would also override the scheduler's virtual methods.
  class concrete_checkpoint_t final : public checkpoint_owner_t {
  public:
    explicit concrete_checkpoint_t(fasedtests_top_t &driver) : driver(driver) {}
    std::string checkpoint_schema() const override {
      return "firesim.simulation.fcfs.v1";
    }
    std::vector<uint8_t> save_checkpoint() const override {
      return driver.save_driver_checkpoint();
    }
    void validate_checkpoint(const std::vector<uint8_t> &payload) const override {
      driver.validate_driver_checkpoint(payload);
    }
    void load_checkpoint(const std::vector<uint8_t> &payload) override {
      driver.load_driver_checkpoint(payload);
    }
  private:
    fasedtests_top_t &driver;
  } driver_checkpoint{*this};
  // Concrete fields only. Base flow, scheduler, loop, widgets, hardware and
  // transport are separate owners; this adapter cannot certify their cut.
  std::vector<uint8_t> save_driver_checkpoint() const;
  void validate_driver_checkpoint(const std::vector<uint8_t> &) const;
  void load_driver_checkpoint(const std::vector<uint8_t> &);
  /// Reference to the simulation interface.
  simif_t &simif;
  /// Reference to the peek-poke bridge.
  peek_poke_t &peek_poke;
  /// List of models in the design.
  std::vector<FASEDMemoryTimingModel *> models;
  /// Records all uarch events we want to validate.
  std::vector<std::pair<std::string, uint32_t>> expected_uarchevent_values;
  std::optional<uint64_t> profile_interval;
  // Bind the bridge traversal once; registry construction is complete before
  // this driver is created. Identity/order must also be bound by the outer
  // manifest, not inferred from pointer values or bridge count alone.
  const std::vector<bridge_driver_t *> bridges;
  stepped_run_loop_t run_loop;
  widget_checkpoint_state_t checkpoint_state;
  bool started = false;
  bool finalized = false;
  widget_checkpoint_config_t checkpoint_configuration() const;
  bool horizon_reached() override { return finished_scheduled_tasks(); }
  void run_tasks() override { run_scheduled_tasks(); }
  void issue_step() override { peek_poke.step(get_largest_stepsize(), false); }
  bool step_done() override { return peek_poke.is_done(); }
  void tick_bridge(uint64_t index) override { bridges.at(index)->tick(); }
  bool sample_done() override { return peek_poke.sample_value("done"); }
};

fasedtests_top_t::fasedtests_top_t(simif_t &simif,
                                 widget_registry_t &registry,
                                 const std::vector<std::string> &args)
    : systematic_scheduler_t(args), simulation_t(registry, args), simif(simif),
      peek_poke(registry.get_widget<peek_poke_t>()),
      models(registry.get_bridges<FASEDMemoryTimingModel>()),
      bridges(registry.get_all_bridges()), run_loop(bridges.size()) {

  // Cycles to advance before profiling instrumentation registers in models.
  std::unordered_map<std::string, uint32_t> expectations;
  for (auto &arg : args) {
    if (arg.find("+profile-interval=") == 0) {
      profile_interval = atoi(arg.c_str() + 18);
      continue;
    }
    if (arg.find("+expect_") == 0) {
      auto sub_arg = std::string(arg.c_str() + 8);
      size_t delimit_idx = sub_arg.find_first_of("=");
      std::string key = sub_arg.substr(0, delimit_idx).c_str();
      int value = std::stoi(sub_arg.substr(delimit_idx + 1).c_str());
      expectations[key] = value;
      continue;
    }
  }
  // Retain the original hash-map traversal order for ordinary finalization;
  // bind that order explicitly so a fresh driver cannot silently change the
  // MMIO/output sequence. Duplicate arguments retain last-value semantics.
  expected_uarchevent_values.assign(expectations.begin(), expectations.end());

  // Add functions you'd like to periodically invoke on a paused simulator here.
  if (profile_interval) {
    // The former reference capture retained a destroyed constructor-local
    // optional. Capture the interval by value; profiling remains unaudited.
    register_task(0, [this, interval = *profile_interval] {
      for (auto *mod : models) {
        mod->profile();
      }
      return interval;
    });
  }
}

int fasedtests_top_t::simulation_run() {
  // Run the simulation until the target reports it is finished by setting
  // the done output flag or the desired number of cycles completes.
  while (!simulation_run_step())
    ;
  return simulation_run_finalize();
}

int fasedtests_top_t::simulation_run_finalize() {
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  if (finalized)
    throw std::runtime_error("FCFS repeated finalization");
  if (!run_loop.finished())
    throw std::runtime_error("FCFS finalization before run-loop completion");

  // Iterate through all uarch values we want to validate.
  bool failed = false;
  for (auto &model : models) {
    for (auto &[key, value] : expected_uarchevent_values) {
      auto actual_value = simif.read(model->get_addr_map().r_addr(key));
      if (actual_value != value) {
        fprintf(stderr,
                "FASED Test Harness -- %s did not match: Measured %d, Expected "
                "%d\n",
                key.c_str(),
                actual_value,
                value);
        failed = true;
      }
    }
  }

  finalized = true;
  return failed ? 1 : 0;
}

widget_checkpoint_config_t fasedtests_top_t::checkpoint_configuration() const {
  widget_checkpoint_config_t config(
      9, {profile_interval.has_value(), profile_interval.value_or(0)});
  config.word(expected_uarchevent_values.size());
  for (const auto &[key, value] : expected_uarchevent_values) {
    config.text(key);
    config.word(value);
  }
  const auto owners = registry.checkpoint_owners();
  const auto identity = [&](checkpoint_owner_t *owner) {
    const auto it = std::find_if(owners.begin(), owners.end(),
                                [&](const auto &entry) {
                                  return entry.second == owner;
                                });
    if (it == owners.end())
      throw std::runtime_error("FCFS owner missing from registry");
    config.text(it->first);
    config.text(owner->checkpoint_schema());
  };
  identity(&peek_poke);
  if (bridges != registry.get_all_bridges() ||
      models != registry.get_bridges<FASEDMemoryTimingModel>())
    throw std::runtime_error("FCFS registry changed after construction");
  config.word(bridges.size());
  for (auto *bridge : bridges)
    identity(bridge);
  config.word(models.size());
  for (auto *model : models) {
    identity(model);
    const auto &map = model->get_addr_map();
    for (const auto *registers : {&map.r_registers, &map.w_registers}) {
      config.word(registers->size());
      for (const auto &[name, address] : *registers) {
        config.text(name);
        config.word(address);
      }
    }
    for (const auto &[key, value] : expected_uarchevent_values) {
      (void)value;
      if (!map.r_reg_exists(key))
        throw std::runtime_error("FCFS expectation has no readable counter: " + key);
    }
  }
  return config;
}

std::vector<uint8_t> fasedtests_top_t::save_driver_checkpoint() const {
  checkpoint_state.require_idle();
  if (!started || finalized)
    throw std::runtime_error("FCFS checkpoint requires an unfinished running driver");
  return checkpoint_state.save(checkpoint_configuration(), 1);
}

void fasedtests_top_t::validate_driver_checkpoint(
    const std::vector<uint8_t> &payload) const {
  checkpoint_state.require_idle();
  if (finalized)
    throw std::runtime_error("FCFS restore after finalization");
  if (checkpoint_state.validate(payload, checkpoint_configuration(), 1) != 1)
    throw std::runtime_error("FCFS checkpoint is not a running driver");
}

void fasedtests_top_t::load_driver_checkpoint(const std::vector<uint8_t> &payload) {
  validate_driver_checkpoint(payload);
  if (started)
    throw std::runtime_error("FCFS restore requires a fresh driver");
  // No MMIO, profiling, file output, task execution or clock advancement.
  started = true;
}

std::unique_ptr<simulation_t>
create_simulation(simif_t &simif,
                  widget_registry_t &registry,
                  const std::vector<std::string> &args) {
  return std::make_unique<fasedtests_top_t>(simif, registry, args);
}
