// See LICENSE for license details.
#include "stepped_run_loop.h"

namespace {
constexpr uint64_t magic = 0x3154504b434c5253ULL; // SRLCKPT1
void require(bool ok, const char *message) {
  if (!ok)
    throw std::runtime_error(std::string("stepped run loop checkpoint: ") + message);
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

stepped_run_loop_t::stepped_run_loop_t(uint64_t count) : bridge_count(count) {}

void stepped_run_loop_t::require_boundary() const {
  require(!active && !poisoned, "suspended or failed host operation");
}

bool stepped_run_loop_t::finished() const {
  require_boundary();
  return state.phase == phase_t::finished;
}

bool stepped_run_loop_t::target_done() const {
  require_boundary();
  return state.done;
}

void stepped_run_loop_t::advance(services_t &services) {
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
                    (bridge_count ? phase_t::tick : phase_t::sample);
      break;
    case phase_t::tick:
      services.tick_bridge(state.bridge_index);
      if (++state.bridge_index == bridge_count) {
        state.bridge_index = 0;
        state.phase = phase_t::sample;
      }
      break;
    case phase_t::sample:
      state.done = services.sample_done();
      state.phase = state.done ? phase_t::finished : phase_t::poll;
      break;
    case phase_t::finished:
      break;
    }
    active = false;
  } catch (...) {
    // The operation may have partly changed an external owner. Do not guess
    // whether its MMIO/credit/bridge effect committed or allow a retry/load.
    active = false;
    poisoned = true;
    throw;
  }
}

std::string stepped_run_loop_t::checkpoint_schema() const {
  return "firesim.stepped-run-loop.v1";
}

std::vector<uint8_t> stepped_run_loop_t::save_checkpoint() const {
  require_boundary();
  std::vector<uint8_t> out;
  for (uint64_t value : {magic, uint64_t(1), bridge_count,
                         uint64_t(state.phase), state.bridge_index,
                         uint64_t(state.done)})
    append(out, value);
  return out;
}

stepped_run_loop_t::state_t
stepped_run_loop_t::parse(const std::vector<uint8_t> &bytes) const {
  require_boundary();
  require(bytes.size() == 48, "invalid payload size");
  require(word(bytes, 0) == magic && word(bytes, 8) == 1, "invalid magic/version");
  require(word(bytes, 16) == bridge_count, "incompatible bridge count");
  const auto phase = word(bytes, 24), index = word(bytes, 32), done = word(bytes, 40);
  require(phase <= uint64_t(phase_t::finished) && done <= 1, "invalid phase/boolean");
  require(phase == uint64_t(phase_t::tick) ? index < bridge_count : index == 0,
          "invalid next bridge index");
  require(!done || phase == uint64_t(phase_t::finished), "done before completion");
  return {phase_t(phase), index, bool(done)};
}

void stepped_run_loop_t::validate_checkpoint(const std::vector<uint8_t> &bytes) const {
  (void)parse(bytes);
}

void stepped_run_loop_t::load_checkpoint(const std::vector<uint8_t> &bytes) {
  const auto replacement = parse(bytes);
  state = replacement;
}
