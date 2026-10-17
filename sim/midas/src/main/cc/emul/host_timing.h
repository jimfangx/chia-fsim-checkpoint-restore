// See LICENSE for license details.
#ifndef __HOST_TIMING_H
#define __HOST_TIMING_H

#include "core/checkpoint.h"
#include <functional>
#include <random>

// The metasim host-delay generator is a component owner, not a freeze protocol.
// Explicit begin/complete boundaries retain a pending delay without a C++ stack.
// Production synchronous calls reject checkpoint access while suspended in RTL.
class host_timing_t final : public checkpoint_owner_t {
public:
  explicit host_timing_t(uint64_t maximum_delay = 1, uint64_t seed = 0);
  static uint64_t parse_unsigned(const std::string &value, uint64_t maximum);

  void begin_delay();
  void complete_tick();
  uint64_t pending_ticks() const { return remaining; }
  // Resume a previously begun delay, or draw exactly once for a new delay.
  void run_delay(const std::function<void()> &tick);

  std::string checkpoint_schema() const override;
  std::vector<uint8_t> save_checkpoint() const override;
  void validate_checkpoint(const std::vector<uint8_t> &) const override;
  void load_checkpoint(const std::vector<uint8_t> &) override;

private:
  friend class simif_emul_t;
  void swap_checkpoint_state(host_timing_t &other) noexcept {
    static_assert(std::is_nothrow_swappable_v<std::mt19937_64>);
    std::swap(engine, other.engine);
    std::swap(total, other.total);
    std::swap(remaining, other.remaining);
  }
  struct state_t {
    uint64_t total, remaining;
    std::mt19937_64 engine;
  };
  state_t parse_checkpoint(const std::vector<uint8_t> &) const;
  void require_boundary() const;
  uint64_t maximum_delay, seed;
  std::mt19937_64 engine;
  uint64_t total = 0, remaining = 0;
  bool active = false, failed = false;
};

#endif
