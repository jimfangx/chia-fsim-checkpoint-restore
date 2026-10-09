// See LICENSE for license details.
#ifndef __HARNESS_OBSERVATION_H
#define __HARNESS_OBSERVATION_H
#include "core/checkpoint.h"

// Immutable, read-only observation of the packed harness banks after the
// rising-edge NBA updates. This is deliberately a diagnostic schema, with no
// load adapter: clock/reset scheduling and fresh context reconstruction still
// need an audit before runtime/harness can be declared restorable.
class harness_observation_t final : public checkpoint_owner_t {
public:
  harness_observation_t(uint32_t bits, const uint32_t *banks) {
    if (!banks || bits < 3 || bits > (1U << 20))
      throw std::runtime_error("invalid settled harness observation geometry");
    append(0x3153424F534E5248ULL); // HRNSOBS1
    append(bits);
    for (uint32_t i = 0; i < (bits + 31) / 32; ++i) {
      uint32_t value = banks[i];
      if (i == bits / 32 && bits % 32)
        value &= (1U << (bits % 32)) - 1;
      for (unsigned j = 0; j < 4; ++j)
        payload.push_back(value >> (8 * j));
    }
  }
  std::string checkpoint_schema() const override {
    return "firesim.diagnostic.harness-banks.v1";
  }
  std::vector<uint8_t> save_checkpoint() const override { return payload; }
  void validate_checkpoint(const std::vector<uint8_t> &value) const override {
    if (value != payload)
      throw std::runtime_error("changed settled harness observation");
  }
private:
  void append(uint64_t value) {
    for (unsigned i = 0; i < 8; ++i)
      payload.push_back(value >> (8 * i));
  }
  std::vector<uint8_t> payload;
};
#endif
