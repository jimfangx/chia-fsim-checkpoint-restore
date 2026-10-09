// See LICENSE for license details.

#include "master.h"
#include "core/simif.h"
#include <stdio.h>

char master_t::KIND;

master_t::master_t(simif_t &simif,
                   const SIMULATIONMASTER_struct &mmio_addrs,
                   unsigned index,
                   const std::vector<std::string> &args)
    : widget_t(simif, &KIND), mmio_addrs(mmio_addrs) {
  assert(index == 0 && "only one simulation master is allowed");
}

bool master_t::is_init_done() {
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  return simif.read(mmio_addrs.INIT_DONE) == 1;
}

bool master_t::check_fingerprint() {
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  uint32_t presence = simif.read(mmio_addrs.PRESENCE_READ);
  printf("FireSim fingerprint: 0x%x\n", presence);
  return presence != 0x46697265;
}

void master_t::write_fingerprint(uint32_t data) {
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  simif.write(mmio_addrs.PRESENCE_WRITE, data);
}

widget_checkpoint_config_t master_t::checkpoint_configuration() const {
  return {1, {mmio_addrs.INIT_DONE, mmio_addrs.PRESENCE_READ, mmio_addrs.PRESENCE_WRITE}};
}
