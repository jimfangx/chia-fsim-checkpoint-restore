// See LICENSE for license details

#include "systematic_scheduler.h"

#include <algorithm>
#include <charconv>
#include <limits>

namespace {
constexpr uint64_t checkpoint_magic = 0x3154504b43484353ULL; // SCHCKPT1
void require(bool condition, const char *message) {
  if (!condition)
    throw std::runtime_error(std::string("scheduler checkpoint: ") + message);
}
void append(std::vector<uint8_t> &out, uint64_t value) {
  for (unsigned i = 0; i < 8; ++i)
    out.push_back(static_cast<uint8_t>(value >> (8 * i)));
}
void append(std::vector<uint8_t> &out, const std::string &value) {
  append(out, value.size());
  out.insert(out.end(), value.begin(), value.end());
}
class reader {
public:
  explicit reader(const std::vector<uint8_t> &bytes) : bytes(bytes) {}
  uint64_t u64() {
    require(bytes.size() - pos >= 8, "truncated payload");
    uint64_t value = 0;
    for (unsigned i = 0; i < 8; ++i)
      value |= uint64_t(bytes[pos++]) << (8 * i);
    return value;
  }
  void match(const std::string &expected) {
    require(u64() == expected.size(), "incompatible callback identity/schema");
    require(bytes.size() - pos >= expected.size(), "truncated identity/schema");
    for (unsigned char c : expected)
      require(bytes[pos++] == c, "incompatible callback identity/schema");
  }
  bool done() const { return pos == bytes.size(); }
private:
  const std::vector<uint8_t> &bytes;
  size_t pos = 0;
};
} // namespace

systematic_scheduler_t::systematic_scheduler_t(
    const std::vector<std::string> &args) {
  for (const auto &arg : args) {
    if (arg.find("+max-cycles=") == 0) {
      uint64_t value = 0;
      const char *first = arg.data() + 12, *last = arg.data() + arg.size();
      auto result = std::from_chars(first, last, value);
      require(result.ec == std::errc{} && result.ptr == last,
              "invalid max-cycles");
      max_cycles = value;
    }
  }
}

void systematic_scheduler_t::register_task(uint64_t first_cycle, task_t &&task) {
  require(!executing_tasks && !poisoned && first_cycle >= current_cycle && bool(task),
          "invalid task registration boundary");
  tasks.push_back(task_tuple_t{std::move(task), first_cycle, first_cycle, {}, nullptr});
}

void systematic_scheduler_t::register_checkpoint_task(
    const std::string &id, uint64_t first_cycle, task_t &&task,
    checkpoint_owner_t &owner) {
  require(!id.empty() && std::all_of(id.begin(), id.end(), [](unsigned char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') || c == '_' || c == '-' || c == '.';
  }), "invalid callback identity");
  require(!owner.checkpoint_schema().empty() && &owner != this,
          "unsupported callback owner");
  for (const auto &t : tasks)
    require(t.checkpoint_id != id && t.checkpoint_owner != &owner,
            "duplicate callback identity/owner");
  register_task(first_cycle, std::move(task));
  tasks.back().checkpoint_id = id;
  tasks.back().checkpoint_owner = &owner;
}

uint32_t systematic_scheduler_t::get_largest_stepsize() {
  require(!executing_tasks && !poisoned, "unsafe step boundary");
  uint64_t next_cycle = current_cycle + std::min(
      default_step_size, std::numeric_limits<uint64_t>::max() - current_cycle);
  if (max_cycles && *max_cycles < next_cycle)
    next_cycle = *max_cycles;
  for (const auto &t : tasks)
    next_cycle = std::min(next_cycle, t.next_cycle);
  require(next_cycle > current_cycle && next_cycle - current_cycle <= MAX_MIDAS_STEP,
          "step cannot make forward progress");
  uint32_t step = next_cycle - current_cycle;
  current_cycle = next_cycle;
  return step;
}

void systematic_scheduler_t::run_scheduled_tasks() {
  require(!executing_tasks && !poisoned, "unsafe callback boundary");
  executing_tasks = true;
  try {
    for (auto &t : tasks) {
      if (t.next_cycle == current_cycle) {
        auto delta = t.task();
        require(delta > 0 && delta <= std::numeric_limits<uint64_t>::max() - t.next_cycle,
                "callback deadline overflow/no progress");
        t.next_cycle += delta;
      }
    }
    executing_tasks = false;
  } catch (...) {
    executing_tasks = false;
    poisoned = true;
    throw;
  }
}

std::string systematic_scheduler_t::checkpoint_schema() const {
  for (const auto &t : tasks)
    if (!t.checkpoint_owner || t.checkpoint_owner->checkpoint_schema().empty())
      return {};
  return "firesim.scheduler.deadlines.v1";
}

void systematic_scheduler_t::require_checkpoint_boundary() const {
  require(!executing_tasks && !poisoned, "callback running/failed; no resumable boundary");
  require(!checkpoint_schema().empty(), "unsupported anonymous/unaudited callback");
}

std::vector<std::pair<std::string, checkpoint_owner_t *>>
systematic_scheduler_t::checkpoint_owners() {
  require_checkpoint_boundary();
  std::vector<std::pair<std::string, checkpoint_owner_t *>> owners{
      {"runtime/scheduler", this}};
  for (const auto &t : tasks)
    owners.emplace_back("runtime/scheduler/callback/" + t.checkpoint_id,
                        t.checkpoint_owner);
  return owners;
}

std::vector<uint8_t> systematic_scheduler_t::save_checkpoint() const {
  require_checkpoint_boundary();
  std::vector<uint8_t> out;
  for (auto value : {checkpoint_magic, uint64_t{1}, uint64_t(max_cycles.has_value()),
                     max_cycles.value_or(0), default_step_size, current_cycle,
                     uint64_t(tasks.size())})
    append(out, value);
  for (const auto &t : tasks) {
    append(out, t.checkpoint_id);
    append(out, t.checkpoint_owner->checkpoint_schema());
    append(out, t.first_cycle);
    append(out, t.next_cycle);
  }
  validate_checkpoint(out);
  return out;
}

std::pair<uint64_t, std::vector<uint64_t>>
systematic_scheduler_t::parse_checkpoint(const std::vector<uint8_t> &bytes) const {
  require_checkpoint_boundary();
  reader in(bytes);
  require(in.u64() == checkpoint_magic && in.u64() == 1, "wrong schema/version");
  require(in.u64() == uint64_t(max_cycles.has_value()) &&
              in.u64() == max_cycles.value_or(0) && in.u64() == default_step_size,
          "incompatible scheduler configuration");
  auto cycle = in.u64();
  require(!max_cycles || cycle <= *max_cycles, "cycle beyond horizon");
  require(in.u64() == tasks.size(), "incompatible callback count");
  std::vector<uint64_t> deadlines;
  deadlines.reserve(tasks.size());
  for (const auto &t : tasks) {
    in.match(t.checkpoint_id);
    in.match(t.checkpoint_owner->checkpoint_schema());
    require(in.u64() == t.first_cycle, "incompatible first deadline");
    auto next = in.u64();
    require(next >= cycle && next >= t.first_cycle, "deadline precedes scheduler/first cycle");
    deadlines.push_back(next);
  }
  require(in.done(), "trailing bytes");
  return {cycle, std::move(deadlines)};
}

void systematic_scheduler_t::validate_checkpoint(const std::vector<uint8_t> &bytes) const {
  (void)parse_checkpoint(bytes);
}

void systematic_scheduler_t::load_checkpoint(const std::vector<uint8_t> &bytes) {
  auto state = parse_checkpoint(bytes); // All parsing/allocation precedes mutation.
  publication.changed();
  current_cycle = state.first;
  for (size_t i = 0; i < tasks.size(); ++i)
    tasks[i].next_cycle = state.second[i];
}

class systematic_scheduler_t::restore_t final : public prepared_checkpoint_t {
  systematic_scheduler_t &owner;
  checkpoint_publication_t::guard_t guard;
  std::vector<uint8_t> expected;
  std::pair<uint64_t, std::vector<uint64_t>> replacement;
public:
  restore_t(systematic_scheduler_t &owner, const std::vector<uint8_t> &bytes)
      : owner(owner), guard(owner.publication), expected(owner.save_checkpoint()),
        replacement(owner.parse_checkpoint(bytes)) {}
  void seal() override {
    guard.require_unsealed(); // Check lifetime before dereferencing owner.
    if (owner.save_checkpoint() != expected)
      throw std::runtime_error("scheduler destination changed after preparation");
    guard.seal();
  }
  void publish() noexcept override {
    guard.publish();
    owner.current_cycle = replacement.first;
    for (size_t i = 0; i < replacement.second.size(); ++i)
      owner.tasks[i].next_cycle = replacement.second[i];
  }
};

std::unique_ptr<prepared_checkpoint_t>
systematic_scheduler_t::prepare_checkpoint_publication(const std::vector<uint8_t> &bytes) {
  return std::make_unique<restore_t>(*this, bytes);
}
