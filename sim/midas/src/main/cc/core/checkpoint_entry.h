// See LICENSE for license details.
#ifndef __CHECKPOINT_ENTRY_H
#define __CHECKPOINT_ENTRY_H

#include <memory>
#include <charconv>
#include <cstdint>
#include <limits>
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
  uint64_t operation = 0; // Completed host driver operations, never target cycles.
  std::string directory;
  uint64_t interval = 0, retention = 0;
  uint64_t host_timeout_ms = 0, cycle_timeout_ms = 0, capture_timeout_ms = 0;

  bool periodic() const { return !directory.empty(); }
  bool watchdog() const { return host_timeout_ms || cycle_timeout_ms; }

  bool durable() const {
    return watchdog() || periodic() || kind == kind_t::capture || kind == kind_t::restore;
  }

  static checkpoint_request_t parse(const std::vector<std::string> &args) {
    checkpoint_request_t request;
    bool have_operation = false;
    bool have_directory = false, have_interval = false, have_retention = false, have_level = false;
    bool have_host_timeout = false, have_cycle_timeout = false, have_capture_timeout = false;
    for (const auto &arg : args) {
      auto positive = [&](const std::string &prefix, bool &seen, uint64_t &number) {
        if (seen || arg.rfind(prefix, 0) != 0)
          throw std::runtime_error("malformed/duplicate periodic checkpoint option");
        seen = true;
        const auto value = arg.substr(prefix.size());
        const auto parsed = std::from_chars(value.data(), value.data()+value.size(), number);
        if (parsed.ec != std::errc{} || parsed.ptr != value.data()+value.size() || !number)
          throw std::runtime_error("invalid periodic checkpoint value");
      };
      auto timeout = [&](const std::string &prefix, bool &seen, uint64_t &number) {
        positive(prefix,seen,number);
        if(number>uint64_t(std::numeric_limits<int64_t>::max())/1000000)
          throw std::runtime_error("checkpoint watchdog timeout exceeds steady-clock range");
      };
      if(arg.rfind("+fs-checkpoint-host-timeout",0)==0) {
        timeout("+fs-checkpoint-host-timeout-ms=",have_host_timeout,request.host_timeout_ms); continue;
      }
      if(arg.rfind("+fs-checkpoint-cycle-timeout",0)==0) {
        timeout("+fs-checkpoint-cycle-timeout-ms=",have_cycle_timeout,request.cycle_timeout_ms); continue;
      }
      if(arg.rfind("+fs-checkpoint-capture-timeout",0)==0) {
        timeout("+fs-checkpoint-capture-timeout-ms=",have_capture_timeout,request.capture_timeout_ms); continue;
      }
      if (arg.rfind("+fs-checkpoint-interval", 0) == 0) {
        positive("+fs-checkpoint-interval=", have_interval, request.interval); continue;
      }
      if (arg.rfind("+fs-checkpoint-retention", 0) == 0) {
        positive("+fs-checkpoint-retention=", have_retention, request.retention); continue;
      }
      if (arg.rfind("+fs-checkpoint-directory", 0) == 0) {
        const std::string prefix = "+fs-checkpoint-directory=";
        if (have_directory || arg.rfind(prefix, 0) != 0 || arg.size() == prefix.size())
          throw std::runtime_error("invalid/duplicate checkpoint directory");
        have_directory = true; request.directory = arg.substr(prefix.size()); continue;
      }
      if (arg.rfind("+fs-checkpoint-level", 0) == 0) {
        if (have_level || arg != "+fs-checkpoint-level=1")
          throw std::runtime_error("periodic checkpoint requires independent Level-1 state");
        have_level = true; continue;
      }
      if (arg.rfind("+fs-checkpoint-operation", 0) == 0) {
        const std::string prefix = "+fs-checkpoint-operation=";
        if (have_operation || arg.rfind(prefix, 0) != 0)
          throw std::runtime_error("malformed/duplicate checkpoint operation schedule");
        have_operation = true;
        const auto value = arg.substr(prefix.size());
        const auto parsed = std::from_chars(value.data(), value.data()+value.size(), request.operation);
        if (parsed.ec != std::errc{} || parsed.ptr != value.data()+value.size() || !request.operation)
          throw std::runtime_error("invalid checkpoint operation schedule");
        continue;
      }
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
      if ((request.kind == kind_t::capture || request.kind == kind_t::restore) && request.path.empty())
        throw std::runtime_error("empty Level-1 checkpoint path");
    }
    if (have_operation && request.kind != kind_t::capture)
      throw std::runtime_error("checkpoint operation schedule requires capture");
    if (have_directory || have_interval || have_retention || have_level) {
      if (!have_directory || !have_interval || !have_retention || !have_level ||
          (request.kind != kind_t::none && request.kind != kind_t::restore))
        throw std::runtime_error("periodic checkpoint requires directory, target interval, retention and level=1; no single capture/preflight");
    }
    if(have_host_timeout || have_cycle_timeout || have_capture_timeout) {
      if(!request.watchdog() || !have_capture_timeout ||
         (!request.periodic() && request.kind!=kind_t::restore))
        throw std::runtime_error("checkpoint watchdog requires progress and bounded capture timeouts with periodic history or restore");
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
