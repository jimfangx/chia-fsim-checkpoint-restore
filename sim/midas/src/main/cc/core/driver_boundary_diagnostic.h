// See LICENSE for license details.
#ifndef __DRIVER_BOUNDARY_DIAGNOSTIC_H
#define __DRIVER_BOUNDARY_DIAGNOSTIC_H
#include "core/checkpoint.h"
#include <memory>
#include <utility>

// Read-only software diagnostic at a completed host operation. The caller
// parks RTL throughout observe(). No target/MMIO/component edges are issued.
// This is not .fsckpt: unaudited owners are explicitly unsupported; there is
// no restore path. The RTL/harness cut remains a separate requirement.
class driver_boundary_diagnostic_t {
public:
  using owners_t = std::vector<std::pair<std::string, checkpoint_owner_t *>>;
  // Read-only bytes for the VCS side of a parked-driver handoff. Capture on
  // the owners' creator thread before publishing through the settled mutex.
  // No live owner or validator reference survives capture. Loading a snapshot
  // always rejects; these observations do not grant restore admission.
  class snapshot_t {
  public:
    snapshot_t() = default;
    snapshot_t(snapshot_t &&) noexcept = default;
    snapshot_t &operator=(snapshot_t &&) noexcept = default;
    snapshot_t(const snapshot_t &) = delete;
    snapshot_t &operator=(const snapshot_t &) = delete;
    owners_t owners() const;
  private:
    friend class driver_boundary_diagnostic_t;
    std::vector<std::pair<std::string, std::unique_ptr<checkpoint_owner_t>>> records;
  };
  static snapshot_t freeze(const owners_t &owners);
  explicit driver_boundary_diagnostic_t(const std::vector<std::string> &args);
  bool requested() const { return !path.empty() || public_capture; }
  void arm_checkpoint_operation(uint64_t operation);
  bool settled() const { return settled_capture; }
  bool lowclock() const { return lowclock_capture; }
  bool due() const { return requested() && completed + 1 == cut; }
  void observe(const owners_t &owners);
  void finish() const;

private:
  std::string path;
  uint64_t cut = 0, completed = 0;
  bool captured = false, active = false, failed = false;
  bool settled_capture = false;
  bool lowclock_capture = false;
  bool public_capture = false;
};
#endif
