#ifndef __HEARTBEAT_H
#define __HEARTBEAT_H

#include "core/bridge_driver.h"
#include "core/widget_checkpoint.h"
#include "core/checkpoint_publication.h"

#include <fstream>

class clockmodule_t;

// Periodically checks that the target is advancing by polling an FPGA-hosted
// cycle count, which it writes out to a file.  The causes of an apparently
// hung simulator can be coarsely deduced from the behavior of this class and
// whether the simulation terminates.
//
// See
// docs/Advanced-Usage/Debugging-and-Profiling-on-FPGA/Debugging-Hanging-Simulators.rst
// for expanded discussion.

class heartbeat_t final : public bridge_driver_t {
public:
  /// The identifier for the bridge type used for casts.
  static char KIND;

  heartbeat_t(simif_t &sim,
              clockmodule_t &clock,
              const std::vector<std::string> &args);

  void tick() override;
  void finish() override;
  bool terminate() override { return has_timed_out; };
  int exit_code() override { return (has_timed_out) ? 1 : 0; };

  // Completed-call host ownership, opt-in buffered log only. The clock's
  // generated counters/latches and MMIO transport remain separate owners.
  std::string checkpoint_schema() const override { return "firesim.widget.heartbeat.v1"; }
  std::vector<uint8_t> save_checkpoint() const override;
  void validate_checkpoint(const std::vector<uint8_t> &) const override;
  void load_checkpoint(const std::vector<uint8_t> &) override;
  std::unique_ptr<prepared_checkpoint_t>
  prepare_checkpoint_publication(const std::vector<uint8_t> &) override;

private:
  class restore_t;
  checkpoint_publication_t publication;
  clockmodule_t &clock;
  std::ofstream log;
  time_t start_time;
  widget_checkpoint_state_t checkpoint_state;
  bool buffered = false;
  bool finished = false;
  std::string output_path = "heartbeat.csv";
  struct sample_t {
    uint64_t cycle;
    int64_t elapsed;
  };
  std::vector<sample_t> samples;
  struct decoded_checkpoint_t {
    time_t start_time;
    bool timed_out;
    uint64_t trip_count, last_cycle;
    std::vector<sample_t> samples;
  };
  void require_checkpointable() const;
  widget_checkpoint_config_t checkpoint_configuration() const;
  decoded_checkpoint_t decode_checkpoint(const std::vector<uint8_t> &) const;

  bool has_timed_out = false;
  // Arbitrary selection; O(10) wallclock seconds for default targets during
  // linux boot
  uint64_t polling_interval = 10e5;
  uint64_t trip_count = 0;
  uint64_t last_cycle = 0;
  bool ignore_heartbeat = false;
};

#endif //__HEARTBEAT_H
