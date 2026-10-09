// See LICENSE for license details.
#include "host_timing.h"

#include <climits>
#include <locale>
#include <sstream>

namespace {
constexpr uint64_t magic = 0x3154504b434d5448ULL; // HTMCKPT1
constexpr size_t state_words = 312;
static_assert(std::mt19937_64::state_size == state_words &&
              std::mt19937_64::word_size == 64);
void require(bool condition, const char *message) {
  if (!condition)
    throw std::runtime_error(std::string("host timing checkpoint: ") + message);
}
void append(std::vector<uint8_t> &out, uint64_t value) {
  for (unsigned i = 0; i < 8; ++i) out.push_back(value >> (8 * i));
}
uint64_t word(const std::vector<uint8_t> &bytes, size_t index) {
  uint64_t value = 0;
  for (unsigned i = 0; i < 8; ++i) value |= uint64_t(bytes.at(8 * index + i)) << (8 * i);
  return value;
}
} // namespace

host_timing_t::host_timing_t(uint64_t maximum_delay, uint64_t seed)
    : maximum_delay(maximum_delay), seed(seed), engine(seed) {
  require(maximum_delay > 0 && maximum_delay <= INT_MAX, "invalid maximum delay");
}

uint64_t host_timing_t::parse_unsigned(const std::string &value, uint64_t maximum) {
  require(!value.empty(), "empty unsigned argument");
  uint64_t result = 0;
  for (char ch : value) {
    require(ch >= '0' && ch <= '9', "invalid unsigned argument");
    auto digit = uint64_t(ch - '0');
    require(digit <= maximum && result <= (maximum - digit) / 10,
            "unsigned argument overflow");
    result = result * 10 + digit;
  }
  return result;
}

void host_timing_t::require_boundary() const {
  require(!active && !failed, "suspended or failed synchronous host delay");
}

void host_timing_t::begin_delay() {
  require_boundary();
  require(remaining == 0, "delay already pending");
  total = engine() % maximum_delay + 1;
  remaining = total;
}

void host_timing_t::complete_tick() {
  require_boundary();
  require(remaining > 0, "no pending tick");
  --remaining;
}

void host_timing_t::run_delay(const std::function<void()> &tick) {
  require_boundary();
  require(bool(tick), "empty tick callback");
  if (!remaining) begin_delay();
  active = true;
  try {
    while (remaining) {
      tick();
      --remaining; // Commit only after the RTL handoff returns.
    }
    active = false;
  } catch (...) {
    active = false;
    failed = true; // An interrupted tick has no certified commit boundary.
    throw;
  }
}

std::string host_timing_t::checkpoint_schema() const {
  return "firesim.host-timing.mt19937-64.libstdcxx.v1";
}

std::vector<uint8_t> host_timing_t::save_checkpoint() const {
  require_boundary();
  std::vector<uint8_t> out;
  for (auto value : {magic, uint64_t{1}, maximum_delay, seed, total, remaining,
                     uint64_t{state_words}}) append(out, value);
  // Use the engine's stream adapter, never its object layout. This audited
  // libstdc++ adapter expects 312 state words and the next-word index; the
  // future outer container must also bind the executable/library build.
  std::ostringstream encoded;
  encoded.imbue(std::locale::classic());
  encoded << engine;
  std::istringstream in(encoded.str());
  in.imbue(std::locale::classic());
  for (size_t i = 0; i <= state_words; ++i) {
    uint64_t value;
    require(bool(in >> value), "unsupported engine stream shape");
    append(out, value);
  }
  in >> std::ws;
  require(in.eof(), "unsupported trailing engine stream state");
  validate_checkpoint(out);
  return out;
}

host_timing_t::state_t
host_timing_t::parse_checkpoint(const std::vector<uint8_t> &bytes) const {
  require_boundary();
  require(bytes.size() == (7 + state_words + 1) * 8, "wrong payload size");
  require(word(bytes, 0) == magic && word(bytes, 1) == 1, "wrong schema/version");
  require(word(bytes, 2) == maximum_delay && word(bytes, 3) == seed,
          "incompatible timing configuration");
  state_t state{word(bytes, 4), word(bytes, 5), {}};
  require(state.total <= maximum_delay && state.remaining <= state.total,
          "invalid pending delay");
  require(word(bytes, 6) == state_words && word(bytes, 7 + state_words) <= state_words,
          "invalid engine geometry/index");
  bool nonzero = false;
  std::ostringstream encoded;
  encoded.imbue(std::locale::classic());
  for (size_t i = 0; i <= state_words; ++i) {
    uint64_t value = word(bytes, 7 + i);
    if (i < state_words) nonzero |= value != 0;
    encoded << value << ' ';
  }
  require(nonzero, "zero engine state");
  std::istringstream in(encoded.str());
  in.imbue(std::locale::classic());
  require(bool(in >> state.engine), "unsupported engine stream load");
  if (state.total == 0)
    require(state.engine == std::mt19937_64(seed), "invalid initial engine state");
  return state;
}

void host_timing_t::validate_checkpoint(const std::vector<uint8_t> &bytes) const {
  (void)parse_checkpoint(bytes);
}

void host_timing_t::load_checkpoint(const std::vector<uint8_t> &bytes) {
  auto state = parse_checkpoint(bytes); // All validation/allocation precedes mutation.
  engine = state.engine;
  total = state.total;
  remaining = state.remaining;
}
