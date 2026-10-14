// See LICENSE for license details.
#ifndef __BRIDGE_RUN_LOOP_H
#define __BRIDGE_RUN_LOOP_H

#include "core/checkpoint.h"

// FireSim's nonblocking PeekPoke/bridge-termination loop. Unlike the FCFS
// loop, termination is checked immediately after EACH bridge tick and stops
// traversal before touching any later bridge. No service call is replayed.
// Scheduler, widgets, generated RTL and transport are separate owners; this
// component does not certify settlement or completed target-cycle time.
class bridge_run_loop_t final : public checkpoint_owner_t {
public:
  class services_t {
  public:
    virtual ~services_t() = default;
    virtual bool horizon_reached() = 0;
    virtual void run_tasks() = 0;
    virtual void issue_step() = 0;
    virtual bool step_done() = 0;
    virtual void tick_bridge(uint64_t index) = 0;
    virtual bool bridge_terminated(uint64_t index) = 0;
    virtual int bridge_exit_code(uint64_t index) = 0;
  };

  explicit bridge_run_loop_t(uint64_t bridge_count);
  void advance(services_t &services);
  bool finished() const;
  bool terminated() const;
  int exit_code() const;
  std::string checkpoint_schema() const override;
  std::vector<uint8_t> save_checkpoint() const override;
  void validate_checkpoint(const std::vector<uint8_t> &) const override;
  void load_checkpoint(const std::vector<uint8_t> &) override;

private:
  enum class phase_t : uint64_t {
    ready, tasks, issue, poll, tick, terminate, status, finished
  };
  struct state_t {
    phase_t phase = phase_t::ready;
    uint64_t bridge_index = 0;
    bool terminated = false;
    int exit_code = 0;
  } state;
  const uint64_t bridge_count;
  bool active = false;
  bool poisoned = false;
  void require_boundary() const;
  state_t parse(const std::vector<uint8_t> &) const;
};
#endif
