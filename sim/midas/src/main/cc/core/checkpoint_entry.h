// See LICENSE for license details.
#ifndef __CHECKPOINT_ENTRY_H
#define __CHECKPOINT_ENTRY_H

#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

class simif_t;
class simulation_t;

// Public Level-1 requests are parsed even in builds without a capability.
// A path is opaque here: only the durable implementation may open it, after
// authorizing the request and binding the runtime inventory/build identity.
struct checkpoint_request_t {
  enum class kind_t { none, preflight, capture, restore };
  kind_t kind = kind_t::none;
  std::string path;

  bool durable() const {
    return kind == kind_t::capture || kind == kind_t::restore;
  }

  static checkpoint_request_t parse(const std::vector<std::string> &args) {
    checkpoint_request_t request;
    for (const auto &arg : args) {
      if (arg.rfind("+fs-checkpoint", 0) != 0 &&
          arg.rfind("+fs-restore", 0) != 0)
        continue;
      if (request.kind != kind_t::none)
        throw std::runtime_error("duplicate or conflicting Level-1 checkpoint requests");
      if (arg == "+fs-checkpoint-preflight") {
        request.kind = kind_t::preflight;
      } else if (arg.rfind("+fs-checkpoint=", 0) == 0) {
        request.kind = kind_t::capture;
        request.path = arg.substr(15);
      } else if (arg.rfind("+fs-restore=", 0) == 0) {
        request.kind = kind_t::restore;
        request.path = arg.substr(12);
      } else {
        throw std::runtime_error("malformed Level-1 checkpoint request: " + arg);
      }
      if (request.durable() && request.path.empty())
        throw std::runtime_error("empty Level-1 checkpoint path");
    }
    return request;
  }
};

// Explicitly linked checkpoint capability, enabled only with
// FS_CHECKPOINT_ENTRY. The ordinary entry owns the platform, generated registry
// and simulation; a capability must never reconstruct them independently.
// Its factory runs before platform construction so configuration/library
// rejection cannot start a worker or consume a native/target tick.
class checkpoint_entry_t {
public:
  virtual ~checkpoint_entry_t() = default;
  enum class action_t { run, restore_reserved };

  // Explicit opt-in, before platform/registry/driver construction. Test-only
  // capabilities inherit rejection and cannot enable public durable requests
  // merely by defining FS_CHECKPOINT_ENTRY. A durable capability must validate
  // its supported mode/configuration without MMIO, workers or file publication.
  virtual void authorize_request(const checkpoint_request_t &request) const {
    if (request.durable())
      throw std::runtime_error("unsupported public durable checkpoint capability");
  }

  // Called after generated construction and simulation creation, before run.
  // restore_reserved promises the fresh harness clock hold is already armed;
  // the DPI/VPI coordinator must validate/load/read back all owners and
  // bootstrap before releasing that hold. Exceptions must fail the process.
  virtual action_t prepare(simif_t &platform,
                           simulation_t &simulation,
                           const checkpoint_request_t &request) = 0;
};

// Enabling FS_CHECKPOINT_ENTRY without a concrete factory fails at link time.
// Public durable requests also require explicit authorize_request support.
std::unique_ptr<checkpoint_entry_t>
create_checkpoint_entry(const std::vector<std::string> &args);

#endif
