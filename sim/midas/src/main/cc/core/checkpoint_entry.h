// See LICENSE for license details.
#ifndef __CHECKPOINT_ENTRY_H
#define __CHECKPOINT_ENTRY_H

#include <memory>
#include <string>
#include <vector>

class simif_t;
class simulation_t;

// Explicitly linked checkpoint capability, enabled only with
// FS_CHECKPOINT_ENTRY. The ordinary entry owns the platform, generated registry
// and simulation; a capability must never reconstruct them independently.
// Its factory runs before platform construction so configuration/library
// rejection cannot start a worker or consume a native/target tick.
class checkpoint_entry_t {
public:
  virtual ~checkpoint_entry_t() = default;
  enum class action_t { run, restore_reserved };

  // Called after generated construction and simulation creation, before run.
  // restore_reserved promises the fresh harness clock hold is already armed;
  // the DPI/VPI coordinator must validate/load/read back all owners and
  // bootstrap before releasing that hold. Exceptions must fail the process.
  virtual action_t prepare(simif_t &platform, simulation_t &simulation) = 0;
};

// Enabling FS_CHECKPOINT_ENTRY without a concrete factory fails at link time.
// This hook alone does not enable the public durable checkpoint plusargs.
std::unique_ptr<checkpoint_entry_t>
create_checkpoint_entry(const std::vector<std::string> &args);

#endif
