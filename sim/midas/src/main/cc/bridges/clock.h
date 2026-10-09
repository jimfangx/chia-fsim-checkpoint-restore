// See LICENSE for license details.

#ifndef __CLOCK_H
#define __CLOCK_H

#include "core/widget.h"
#include "core/widget_checkpoint.h"

#include <cstdint>
#include <string>
#include <vector>

class simif_t;

struct CLOCKBRIDGEMODULE_struct {
  uint64_t hCycle_0;
  uint64_t hCycle_1;
  uint64_t hCycle_latch;
  uint64_t tCycle_0;
  uint64_t tCycle_1;
  uint64_t tCycle_latch;
};

class clockmodule_t final : public widget_t {
public:
  // Audited host state only; generated RTL/MMIO remain separate owners.
  std::string checkpoint_schema() const override { return "firesim.widget.clock.v1"; }
  std::vector<uint8_t> save_checkpoint() const override {
    return checkpoint_state.save(checkpoint_configuration());
  }
  void validate_checkpoint(const std::vector<uint8_t> &payload) const override {
    checkpoint_state.validate(payload, checkpoint_configuration());
  }
  void load_checkpoint(const std::vector<uint8_t> &payload) override {
    validate_checkpoint(payload);
  }

  /// The identifier for the bridge type.
  static char KIND;

  clockmodule_t(simif_t &simif,
                const CLOCKBRIDGEMODULE_struct &mmio_addrs,
                unsigned index,
                const std::vector<std::string> &args);

  /**
   * Provides the current target cycle of the fastest clock.
   *
   * The target cycle is based on the number of clock tokens enqueued
   * (will report a larger number).
   */
  uint64_t tcycle();

  /**
   * Returns the current host cycle as measured by a hardware counter
   */
  uint64_t hcycle();

private:
  widget_checkpoint_state_t checkpoint_state;
  widget_checkpoint_config_t checkpoint_configuration() const;
  const CLOCKBRIDGEMODULE_struct mmio_addrs;
};

#endif // __CLOCK_H
