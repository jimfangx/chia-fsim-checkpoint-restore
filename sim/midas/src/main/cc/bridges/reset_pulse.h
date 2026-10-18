#ifndef __RESET_PULSE_H
#define __RESET_PULSE_H

struct RESETPULSEBRIDGEMODULE_struct {
  unsigned long pulseLength;
  unsigned long doneInit;
};

#include <vector>

#include "core/bridge_driver.h"
#include "core/widget_checkpoint.h"
#include "core/configuration_checkpoint.h"

class reset_pulse_t final : public bridge_driver_t {
public:
  // Audited host state only; generated RTL/MMIO remain separate owners.
  std::string checkpoint_schema() const override { return "firesim.widget.reset-pulse.v1"; }
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

  /// The identifier for the bridge type used for casts.
  static char KIND;

  reset_pulse_t(simif_t &sim,
                const RESETPULSEBRIDGEMODULE_struct &mmio_addrs,
                unsigned index,
                const std::vector<std::string> &args,
                unsigned int max_pulse_length,
                unsigned int default_pulse_length);

  // Bridge interface
  void init() override;

  unsigned get_max_pulse_length() const { return max_pulse_length; }

private:
  checkpoint_publication_t checkpoint_publication;
  widget_checkpoint_state_t checkpoint_state;
  widget_checkpoint_config_t checkpoint_configuration() const;
  const RESETPULSEBRIDGEMODULE_struct mmio_addrs;
  const unsigned int max_pulse_length;
  const unsigned int default_pulse_length;

  unsigned int pulse_length = default_pulse_length;
};

#endif //__RESET_PULSE_H
