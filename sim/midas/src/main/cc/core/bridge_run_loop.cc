// See LICENSE for license details.
#include "bridge_run_loop.h"
#include <limits>

namespace {
constexpr uint64_t magic = 0x3154504b434c5242ULL; // BRLCKPT1
void require(bool ok, const char *message) {
  if (!ok)
    throw std::runtime_error(std::string("bridge run loop checkpoint: ") + message);
}
void append(std::vector<uint8_t> &out, uint64_t value) {
  for (unsigned i = 0; i < 8; ++i)
    out.push_back(static_cast<uint8_t>(value >> (8 * i)));
}
uint64_t word(const std::vector<uint8_t> &bytes, size_t offset) {
  uint64_t value = 0;
  for (unsigned i = 0; i < 8; ++i)
    value |= uint64_t(bytes[offset + i]) << (8 * i);
  return value;
}
} // namespace

bridge_run_loop_t::bridge_run_loop_t(uint64_t count) : bridge_count(count) {
  static_assert(sizeof(int) == 4, "bridge exit status requires a 32-bit int");
}

void bridge_run_loop_t::require_boundary() const {
  require(!active && !poisoned, "suspended or failed host operation");
}
bool bridge_run_loop_t::finished() const {
  require_boundary();
  return state.phase == phase_t::finished;
}
bool bridge_run_loop_t::terminated() const {
  require_boundary();
  return state.terminated;
}
int bridge_run_loop_t::exit_code() const {
  require_boundary();
  return state.exit_code;
}

void bridge_run_loop_t::advance(services_t &services) {
  require_boundary();
  active = true;
  try {
    switch (state.phase) {
    case phase_t::ready:
      state.phase = services.horizon_reached() ? phase_t::finished : phase_t::tasks;
      break;
    case phase_t::tasks:
      services.run_tasks();
      state.phase = phase_t::issue;
      break;
    case phase_t::issue:
      services.issue_step();
      state.phase = phase_t::poll;
      break;
    case phase_t::poll:
      state.phase = services.step_done() ? phase_t::ready :
                    (bridge_count ? phase_t::tick : phase_t::poll);
      break;
    case phase_t::tick:
      services.tick_bridge(state.bridge_index);
      state.phase = phase_t::terminate;
      break;
    case phase_t::terminate:
      if (services.bridge_terminated(state.bridge_index)) {
        state.phase = phase_t::status;
      } else if (++state.bridge_index == bridge_count) {
        state.bridge_index = 0;
        state.phase = phase_t::poll;
      } else {
        state.phase = phase_t::tick;
      }
      break;
    case phase_t::status:
      state.exit_code = services.bridge_exit_code(state.bridge_index);
      state.terminated = true;
      state.bridge_index = 0;
      state.phase = phase_t::finished;
      break;
    case phase_t::finished:
      break;
    }
    active = false;
  } catch (...) {
    // A service may have changed another owner before throwing. It is unsafe
    // to retry or load this component without reconstructing the whole cut.
    active = false;
    poisoned = true;
    throw;
  }
}

std::string bridge_run_loop_t::checkpoint_schema() const {
  return "firesim.bridge-run-loop.v1";
}
std::vector<uint8_t> bridge_run_loop_t::save_checkpoint() const {
  require_boundary();
  std::vector<uint8_t> out;
  for (uint64_t value : {magic, uint64_t(1), bridge_count, uint64_t(state.phase),
                         state.bridge_index, uint64_t(state.terminated),
                         uint64_t(uint32_t(state.exit_code))})
    append(out, value);
  return out;
}
bridge_run_loop_t::state_t
bridge_run_loop_t::parse(const std::vector<uint8_t> &bytes) const {
  require_boundary();
  require(bytes.size() == 56, "invalid payload size");
  require(word(bytes, 0) == magic && word(bytes, 8) == 1, "invalid magic/version");
  require(word(bytes, 16) == bridge_count, "incompatible bridge count");
  const auto phase = word(bytes, 24), index = word(bytes, 32),
             stopped = word(bytes, 40), code = word(bytes, 48);
  require(phase <= uint64_t(phase_t::finished) && stopped <= 1 &&
          code <= std::numeric_limits<uint32_t>::max(), "invalid phase/boolean/status");
  const bool at_bridge = phase >= uint64_t(phase_t::tick) &&
                         phase <= uint64_t(phase_t::status);
  require(at_bridge ? index < bridge_count : index == 0, "invalid next bridge index");
  require(!stopped || phase == uint64_t(phase_t::finished), "terminated before completion");
  require(stopped || code == 0, "exit status without termination");
  // Decode two's-complement without implementation-defined unsigned narrowing.
  const int decoded = code <= uint64_t(std::numeric_limits<int>::max()) ? int(code) :
                      -1 - int(std::numeric_limits<uint32_t>::max() - code);
  return {phase_t(phase), index, bool(stopped), decoded};
}
void bridge_run_loop_t::validate_checkpoint(const std::vector<uint8_t> &bytes) const {
  (void)parse(bytes);
}
void bridge_run_loop_t::load_checkpoint(const std::vector<uint8_t> &bytes) {
  const auto replacement = parse(bytes);
  state = replacement;
}
