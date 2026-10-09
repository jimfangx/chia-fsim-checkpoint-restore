// See LICENSE for license details.

#include "clock.h"
#include "core/simif.h"

char clockmodule_t::KIND;

clockmodule_t::clockmodule_t(simif_t &simif,
                             const CLOCKBRIDGEMODULE_struct &mmio_addrs,
                             unsigned index,
                             const std::vector<std::string> &args)
    : widget_t(simif, &KIND), mmio_addrs(mmio_addrs) {
  assert(index == 0 && "only one clock bridge is allowed");
}

uint64_t clockmodule_t::tcycle() {
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  simif.write(mmio_addrs.tCycle_latch, 1);
  uint32_t cycle_l = simif.read(mmio_addrs.tCycle_0);
  uint32_t cycle_h = simif.read(mmio_addrs.tCycle_1);
  return (((uint64_t)cycle_h) << 32) | cycle_l;
}

uint64_t clockmodule_t::hcycle() {
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  simif.write(mmio_addrs.hCycle_latch, 1);
  uint32_t cycle_l = simif.read(mmio_addrs.hCycle_0);
  uint32_t cycle_h = simif.read(mmio_addrs.hCycle_1);
  return (((uint64_t)cycle_h) << 32) | cycle_l;
}

widget_checkpoint_config_t clockmodule_t::checkpoint_configuration() const {
  return {2, {mmio_addrs.hCycle_0, mmio_addrs.hCycle_1, mmio_addrs.hCycle_latch,
              mmio_addrs.tCycle_0, mmio_addrs.tCycle_1, mmio_addrs.tCycle_latch}};
}
