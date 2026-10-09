// See LICENSE for license details.
#ifndef __DRIVER_BOUNDARY_DIAGNOSTIC_H
#define __DRIVER_BOUNDARY_DIAGNOSTIC_H
#include "core/checkpoint.h"
#include <utility>

// Read-only software diagnostic at a completed host operation. The caller
// parks RTL throughout observe(). No target/MMIO/component edges are issued.
// This is not .fsckpt: unaudited owners are explicitly unsupported; there is
// no restore path. The RTL/harness cut remains a separate requirement.
class driver_boundary_diagnostic_t {
public:
  using owners_t = std::vector<std::pair<std::string, checkpoint_owner_t *>>;
  explicit driver_boundary_diagnostic_t(const std::vector<std::string> &args);
  bool requested() const { return !path.empty(); }
  bool due() const { return requested() && completed + 1 == cut; }
  void observe(const owners_t &owners);
  void finish() const;

private:
  std::string path;
  uint64_t cut = 0, completed = 0;
  bool captured = false, active = false, failed = false;
};
#endif
