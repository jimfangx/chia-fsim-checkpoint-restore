// See LICENSE for license details
#ifndef __SYSTEMATIC_SCHEDULER_H
#define __SYSTEMATIC_SCHEDULER_H

#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "core/checkpoint.h"
#include "core/checkpoint_publication.h"

// Maximum step size in MIDAS's master is capped to width of the simulation bus
constexpr uint64_t MAX_MIDAS_STEP = (1LL << sizeof(uint32_t) * 8) - 1;

// Schedules a series of tasks that must run a specific cycles on the FPGA, but
// may be associated with multiple bridges / touch non-bridge simulator
// collateral (e.g, reservoir sampling for strober.)
//
// This class does that by providing cycle deltas (i.e., a credit) that can be
// fed into some mechanism (e.g., peek/poke or a bridge) to advance the
// simulator to the time of the desired task. The onus is on the parent class /
// instantiator to ensure the simulator has advanced to the desired cycle
// before running the scheduled task.
class systematic_scheduler_t : public checkpoint_owner_t {
  using task_t = std::function<uint64_t()>;
  using task_tuple_t = struct {
    task_t task;
    uint64_t next_cycle;
    uint64_t first_cycle;
    std::string checkpoint_id;
    checkpoint_owner_t *checkpoint_owner;
  };

public:
  systematic_scheduler_t(const std::vector<std::string> &args);

  // Adds a new task to scheduler.
  void register_task(uint64_t first_cycle, task_t &&task);
  // The adapter must cover ALL callback/closure state and outlive the scheduler.
  // Its payload is a separate owner, never a function or object address.
  void register_checkpoint_task(const std::string &id, uint64_t first_cycle,
                                task_t &&task, checkpoint_owner_t &owner);
  // Component inventory only: the runtime coordinator must capture every
  // returned owner at a coherent cut. current_cycle records issued credits,
  // not completed target execution; pending step/driver state lives elsewhere.
  std::vector<std::pair<std::string, checkpoint_owner_t *>> checkpoint_owners();
  std::string checkpoint_schema() const override;
  std::vector<uint8_t> save_checkpoint() const override;
  void validate_checkpoint(const std::vector<uint8_t> &) const override;
  void load_checkpoint(const std::vector<uint8_t> &) override;
  std::unique_ptr<prepared_checkpoint_t>
  prepare_checkpoint_publication(const std::vector<uint8_t> &) override;
  // Calculates the next simulation step by taking the min of all
  // tasks.next_cycle
  uint32_t get_largest_stepsize();
  // Assumption: The simulator is idle. (simif::done() == true)
  // Invokes all tasks that wish to be executed on our current target cycle
  void run_scheduled_tasks();
  // Returns true if no further tasks are scheduled before specified horizon
  // (max_cycles).
  bool finished_scheduled_tasks() { return current_cycle == max_cycles; };

private:
  class restore_t;
  checkpoint_publication_t publication;
  // Unless overriden, assume the simulator will run indefinitely.
  std::optional<uint64_t> max_cycles = std::nullopt;

  uint64_t default_step_size = MAX_MIDAS_STEP;

  uint64_t current_cycle = 0;

  std::vector<task_tuple_t> tasks;
  // Callbacks may yield to RTL through MMIO. Such suspended calls are not a
  // restorable boundary. A throwing callback also makes the scheduler unsafe.
  bool executing_tasks = false;
  bool poisoned = false;
  void require_checkpoint_boundary() const;
  std::pair<uint64_t, std::vector<uint64_t>>
  parse_checkpoint(const std::vector<uint8_t> &) const;
};
#endif // __SYSTEMATIC_SCHEDULER_H
