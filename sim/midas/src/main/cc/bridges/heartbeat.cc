
#include "heartbeat.h"
#include "bridges/clock.h"
#include "core/simif.h"

#include <cinttypes>
#include <cstring>
#include <limits>

namespace {
constexpr const char *log_header = "Target Cycle (fastest), Seconds Since Start";
}

char heartbeat_t::KIND;

heartbeat_t::heartbeat_t(simif_t &sim,
                         clockmodule_t &clock,
                         const std::vector<std::string> &args)
    : bridge_driver_t(sim, &KIND), clock(clock) {
  auto interval_arg = std::string("+heartbeat-polling-interval=");
  auto partitioned_arg = std::string("+partitioned=");
  for (const auto &arg : args) {
    if (arg.find(interval_arg) == 0) {
      char *str = const_cast<char *>(arg.c_str()) + interval_arg.length();
      polling_interval = atol(str);
    }
    if (arg.find(partitioned_arg) == 0) {
      char *str = const_cast<char *>(arg.c_str()) + partitioned_arg.length();
      ignore_heartbeat = atoi(str);
    }
    if (arg == "+heartbeat-checkpoint-buffered")
      buffered = true;
    const std::string output_arg = "+heartbeat-output=";
    if (arg.find(output_arg) == 0)
      output_path = arg.substr(output_arg.size());
  }

  if (!buffered) {
    log.open(output_path, std::ios_base::out);
    if (!log.is_open()) {
      fprintf(stderr, "Could not open heartbeat output file.\n");
      abort();
    }
    log << log_header << std::endl;
  }
  time(&start_time);
}

void heartbeat_t::tick() {
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  if (finished)
    throw std::runtime_error("heartbeat tick after finish");
  if (trip_count == polling_interval) {
    trip_count = 0;
    uint64_t current_cycle = clock.tcycle();
    if (!ignore_heartbeat) {
      has_timed_out |= current_cycle == last_cycle;
    }

    time_t current_time;
    time(&current_time);
    (void)localtime(&current_time);
    if (buffered) {
      if (start_time < 0 || current_time < 0)
        throw std::runtime_error("unsupported heartbeat wall-clock failure");
      samples.push_back({current_cycle, current_time - start_time});
    } else {
      log << current_cycle << ", " << current_time - start_time << std::endl;
    }
    last_cycle = current_cycle;

    if (has_timed_out) {
      fprintf(stderr,
              "Simulator deadlock detected at target cycle %" PRId64
              ". Terminating.\n",
              current_cycle);
    }
  } else {
    trip_count++;
  }
}

void heartbeat_t::finish() {
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  if (finished)
    throw std::runtime_error("heartbeat already finished");
  if (buffered) {
    std::ofstream output(output_path, std::ios_base::out);
    output << log_header << '\n';
    for (const auto &sample : samples)
      output << sample.cycle << ", " << sample.elapsed << '\n';
    output.flush();
    if (!output)
      throw std::runtime_error("heartbeat output publish failed");
  }
  finished = true;
}

void heartbeat_t::require_checkpointable() const {
  checkpoint_state.require_idle();
  if (!buffered || finished)
    throw std::runtime_error("unsupported heartbeat live-log/finished lifecycle");
}

widget_checkpoint_config_t heartbeat_t::checkpoint_configuration() const {
  auto binding = clock.save_checkpoint(); // Configuration only; no MMIO.
  widget_checkpoint_config_t c(11, {polling_interval, uint64_t(ignore_heartbeat), binding.size()});
  c.bytes.insert(c.bytes.end(), binding.begin(), binding.end());
  return c;
}

std::vector<uint8_t> heartbeat_t::save_checkpoint() const {
  require_checkpointable();
  auto c = checkpoint_configuration();
  c.word(start_time);
  c.word(has_timed_out);
  c.word(trip_count);
  c.word(last_cycle);
  c.word(samples.size());
  for (const auto &sample : samples) {
    c.word(sample.cycle);
    c.word(sample.elapsed);
  }
  (void)decode_checkpoint(c.bytes);
  return std::move(c.bytes);
}

heartbeat_t::decoded_checkpoint_t heartbeat_t::decode_checkpoint(const std::vector<uint8_t> &b) const {
  static_assert(sizeof(time_t) == 8 && std::numeric_limits<time_t>::is_signed,
                "heartbeat adapter requires signed 64-bit time_t");
  require_checkpointable();
  const auto c = checkpoint_configuration();
  if (b.size() < c.bytes.size() + 40 ||
      !std::equal(c.bytes.begin(), c.bytes.end(), b.begin()))
    throw std::runtime_error("heartbeat checkpoint configuration/incomplete state mismatch");
  size_t pos = c.bytes.size();
  auto word = [&]() {
    uint64_t v = 0;
    for (unsigned i = 0; i < 8; ++i) v |= uint64_t(b[pos++]) << (8 * i);
    return v;
  };
  const auto epoch = word(), timed_out = word(), trips = word(), last = word(), count = word();
  if (epoch > uint64_t(INT64_MAX) || timed_out > 1 || trips > polling_interval ||
      count != (b.size() - pos) / 16 || (b.size() - pos) % 16)
    throw std::runtime_error("invalid heartbeat epoch/flags/polling/sample length");
  decoded_checkpoint_t d{time_t(epoch), bool(timed_out), trips, last, {}};
  d.samples.reserve(count);
  uint64_t previous = 0;
  bool expected_timeout = false;
  for (uint64_t i = 0; i < count; ++i) {
    const auto cycle = word(), elapsed_bits = word();
    int64_t elapsed;
    std::memcpy(&elapsed, &elapsed_bits, sizeof(elapsed));
    // Signed elapsed time may decrease when the host wall clock changes.
    // Preserve it as diagnostic data; timeout depends only on cycle progress.
    d.samples.push_back({cycle, elapsed});
    expected_timeout |= !ignore_heartbeat && cycle == previous;
    previous = cycle;
  }
  if (previous != last || expected_timeout != bool(timed_out))
    throw std::runtime_error("heartbeat sample/cycle/timeout mismatch");
  return d;
}

void heartbeat_t::validate_checkpoint(const std::vector<uint8_t> &b) const {
  (void)decode_checkpoint(b);
}

void heartbeat_t::load_checkpoint(const std::vector<uint8_t> &b) {
  auto d = decode_checkpoint(b); // Parse/allocate everything before mutation.
  samples.swap(d.samples);
  start_time = d.start_time;
  has_timed_out = d.timed_out;
  trip_count = d.trip_count;
  last_cycle = d.last_cycle;
}
