// See LICENSE for license details.

#ifndef __MASTER_H
#define __MASTER_H

#include "core/widget.h"
#include "core/widget_checkpoint.h"
#include "core/configuration_checkpoint.h"

#include <cstdint>
#include <string>
#include <vector>

class simif_t;

struct SIMULATIONMASTER_struct {
  uint64_t INIT_DONE;
  uint64_t PRESENCE_READ;
  uint64_t PRESENCE_WRITE;
};

class master_t final : public widget_t {
public:
  // Audited host state only; generated RTL/MMIO remain separate owners.
  std::string checkpoint_schema() const override { return "firesim.widget.master.v1"; }
  std::vector<uint8_t> save_checkpoint() const override {
    return checkpoint_state.save(checkpoint_configuration());
  }
  void validate_checkpoint(const std::vector<uint8_t> &payload) const override {
    checkpoint_state.validate(payload, checkpoint_configuration());
  }
  void load_checkpoint(const std::vector<uint8_t> &payload) override {
    validate_checkpoint(payload);
    checkpoint_publication.changed();
  }
  std::unique_ptr<prepared_checkpoint_t> prepare_checkpoint_publication(
      const std::vector<uint8_t> &payload) override {
    return std::make_unique<configuration_checkpoint_t>(*this, checkpoint_publication, payload);
  }

  /// The identifier for the bridge type.
  static char KIND;

  master_t(simif_t &simif,
           const SIMULATIONMASTER_struct &mmio_addrs,
           unsigned index,
           const std::vector<std::string> &args);

  /**
   * Check whether the device has FireSim fingerprint string.
   */
  bool check_fingerprint();

  /**
   * Write new value to FireSim fingerprint string.
   */
  void write_fingerprint(uint32_t data);

  /**
   * Check whether the device is initialised.
   */
  bool is_init_done();

private:
  checkpoint_publication_t checkpoint_publication;
  widget_checkpoint_state_t checkpoint_state;
  widget_checkpoint_config_t checkpoint_configuration() const;
  const SIMULATIONMASTER_struct mmio_addrs;
};

#endif // __MASTER_H
