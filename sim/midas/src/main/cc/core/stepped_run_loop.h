// See LICENSE for license details.
#ifndef __STEPPED_RUN_LOOP_H
#define __STEPPED_RUN_LOOP_H

#include "core/checkpoint.h"

// The FCFS driver's nonblocking PeekPoke loop. Each advance performs at most
// one synchronous host operation. It returns only after that operation commits;
// a suspended MMIO/bridge/callback stack is never a checkpoint boundary.
// Services, scheduler, bridges, RTL and the harness are SEPARATE owners. This
// component neither freezes them nor establishes completed target-cycle time.
class stepped_run_loop_t final : public checkpoint_owner_t {
public:
  class services_t {
  public:
    virtual ~services_t() = default;
    virtual bool horizon_reached() = 0;
    virtual void run_tasks() = 0;
    // Credit issuance and the nonblocking step write are one guarded operation.
    virtual void issue_step() = 0;
    virtual bool step_done() = 0;
    virtual void tick_bridge(uint64_t index) = 0;
    virtual bool sample_done() = 0;
  };

  explicit stepped_run_loop_t(uint64_t bridge_count);
  void advance(services_t &services);
  bool finished() const;
  bool target_done() const;
  std::string checkpoint_schema() const override;
  std::vector<uint8_t> save_checkpoint() const override;
  void validate_checkpoint(const std::vector<uint8_t> &) const override;
  void load_checkpoint(const std::vector<uint8_t> &) override;

private:
  enum class phase_t : uint64_t {
    ready, tasks, issue, poll, tick, sample, finished
  };
  struct state_t {
    phase_t phase = phase_t::ready;
    uint64_t bridge_index = 0;
    bool done = false;
  } state;
  const uint64_t bridge_count;
  bool active = false;
  bool poisoned = false;
  void require_boundary() const;
  state_t parse(const std::vector<uint8_t> &) const;
};
#endif
