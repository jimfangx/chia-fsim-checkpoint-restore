// See LICENSE for license details.

#ifndef __PEEK_POKE
#define __PEEK_POKE

#include <gmp.h>
#include <string_view>

#include "core/simif.h"
#include "core/widget_checkpoint.h"
#include "core/timing.h"

struct PEEKPOKEBRIDGEMODULE_struct {
  uint64_t STEP;
  uint64_t DONE;
  uint64_t PRECISE_PEEKABLE;
};

/**
 * Bridge interfacing with the top-level ports of the DUT.
 *
 * The peek-poke bridge allows the driver to control the input pins of the
 * design and inspect the output bits. It should only be used in tests.
 *
 * Care must be taken to avoid deadlock, since calls to step. must be blocking
 * for peeks and pokes to capture and drive values at the correct cycles.
 */
class peek_poke_t final : public widget_t {
public:
  // Audited host state only; generated RTL/MMIO remain separate owners.
  std::string checkpoint_schema() const override { return "firesim.widget.peek-poke.v1"; }
  std::vector<uint8_t> save_checkpoint() const override {
    return checkpoint_state.save(checkpoint_configuration(), uint64_t(req_unstable) | (uint64_t(req_timeout) << 1));
  }
  void validate_checkpoint(const std::vector<uint8_t> &payload) const override {
    checkpoint_state.validate(payload, checkpoint_configuration(), 3);
  }
  void load_checkpoint(const std::vector<uint8_t> &payload) override {
    const auto flags = checkpoint_state.validate(payload, checkpoint_configuration(), 3);
    req_unstable = flags & 1;
    req_timeout = flags & 2;
  }

  /// The identifier for the bridge type.
  static char KIND;

  struct Port {
    uint64_t address;
    uint32_t chunks;
  };

  using PortMap = std::map<std::string, Port, std::less<>>;

  peek_poke_t(simif_t &simif,
              const PEEKPOKEBRIDGEMODULE_struct &mmio_addrs,
              unsigned index,
              const std::vector<std::string> &args,
              PortMap &&inputs,
              PortMap &&outputs);
  ~peek_poke_t() override = default;

  void poke(std::string_view id, uint32_t value, bool blocking);
  void poke(std::string_view id, mpz_t &value);

  uint32_t peek(std::string_view id, bool blocking);
  void peek(std::string_view id, mpz_t &value);
  uint32_t sample_value(std::string_view id) { return peek(id, false); }

  /**
   * Returns true if the last request times out.
   */
  bool timeout() const { return req_timeout; }

  /**
   * Returns true if the last peek was unstable.
   */
  bool unstable() const { return req_unstable; }

  /**
   * Check whether the cycle horizon has been reached.
   */
  bool is_done();

  /**
   * Advance the cycle horizon a given number of steps.
   */
  void step(size_t n, bool blocking);

private:
  widget_checkpoint_state_t checkpoint_state;
  widget_checkpoint_config_t checkpoint_configuration() const;
  /// Base MMIO port mapping.
  const PEEKPOKEBRIDGEMODULE_struct mmio_addrs;
  /// Flag to indicate whether the last request was on an unstable value.
  bool req_unstable = false;
  /// Flag to indicate whether the last request timed out.
  bool req_timeout = false;
  /// Addresses of input ports.
  const PortMap inputs;
  /// Addresses of output ports.
  const PortMap outputs;

  bool wait_on(size_t flag_addr, double timeout) {
    midas_time_t start = timestamp();
    while (!simif.read(flag_addr))
      if (diff_secs(timestamp(), start) > timeout)
        return false;
    return true;
  }

  bool wait_on_done(double timeout) {
    return wait_on(mmio_addrs.DONE, timeout);
  }

  bool wait_on_stable_peeks(double timeout) {
    return wait_on(mmio_addrs.PRECISE_PEEKABLE, timeout);
  }
};

#endif // __PEEK_POKE
