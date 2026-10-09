// See LICENSE for license details.
#ifndef __WIDGET_CHECKPOINT_H
#define __WIDGET_CHECKPOINT_H

#include <algorithm>
#include <exception>
#include <initializer_list>

#include "core/checkpoint.h"

// Used only by explicitly audited concrete widgets. The generated RTL and
// MMIO queue owners retain hardware counters, latches and in-flight requests;
// these payloads bind host configuration and any completed-call result flags.
class widget_checkpoint_config_t {
public:
  widget_checkpoint_config_t(uint64_t kind,
                             std::initializer_list<uint64_t> values) {
    word(0x3154504B43474457ULL); // WDGCKPT1, little endian
    word(1);
    word(kind);
    word(values.size());
    for (auto value : values)
      word(value);
  }

  void word(uint64_t value) {
    for (unsigned i = 0; i < 8; ++i)
      bytes.push_back(value >> (8 * i));
  }
  void text(const std::string &value) {
    word(value.size());
    bytes.insert(bytes.end(), value.begin(), value.end());
  }
  std::vector<uint8_t> bytes;
};

class widget_checkpoint_state_t {
public:
  // The call must finish before the coordinator can own this widget. A
  // suspended stack may retain read halves, GMP buffers, file positions or
  // wall-clock deadlines; none can be reconstructed by these adapters.
  class call_t {
  public:
    explicit call_t(widget_checkpoint_state_t &state)
        : state(state), exceptions(std::uncaught_exceptions()) {
      state.require_idle();
      state.active = true;
    }
    ~call_t() {
      if (std::uncaught_exceptions() > exceptions)
        state.poisoned = true;
      state.active = false;
    }
    call_t(const call_t &) = delete;
    call_t &operator=(const call_t &) = delete;

  private:
    widget_checkpoint_state_t &state;
    int exceptions;
  };

  std::vector<uint8_t> save(widget_checkpoint_config_t config,
                            uint64_t flags = 0) const {
    require_idle();
    config.word(flags);
    return std::move(config.bytes);
  }

  uint64_t validate(const std::vector<uint8_t> &payload,
                    const widget_checkpoint_config_t &config,
                    uint64_t allowed_flags = 0) const {
    require_idle();
    if (payload.size() != config.bytes.size() + 8 ||
        !std::equal(config.bytes.begin(), config.bytes.end(), payload.begin()))
      throw std::runtime_error("widget checkpoint configuration/payload mismatch");
    uint64_t flags = 0;
    for (unsigned i = 0; i < 8; ++i)
      flags |= uint64_t(payload[config.bytes.size() + i]) << (8 * i);
    if (flags & ~allowed_flags)
      throw std::runtime_error("invalid widget checkpoint result flags");
    return flags;
  }

public:
  void require_idle() const {
    if (active || poisoned)
      throw std::runtime_error("suspended/reentrant/interrupted widget operation");
  }
private:
  bool active = false;
  bool poisoned = false;
};

#endif
