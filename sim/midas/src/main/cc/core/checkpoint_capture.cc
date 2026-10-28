// See LICENSE for license details.
#include "core/checkpoint_capture.h"

#include <algorithm>
#include <cstring>
#include <limits>
#include <set>
#include <stdexcept>
#include <utility>

namespace {
void require_capture(bool value, const char *message) {
  if (!value)
    throw std::runtime_error(message);
}

uint64_t load_le(const uint8_t *bytes, size_t count) {
  uint64_t value = 0;
  for (size_t i = 0; i < count; ++i)
    value |= uint64_t(bytes[i]) << (8 * i);
  return value;
}

uint64_t manifest_words(const checkpoint_capture_manifest_t &manifest) {
  require_capture(!manifest.entries.empty() && manifest.entries.size() <= 65535,
                  "invalid capture entry count");
  require_capture(std::any_of(manifest.wire_hash.begin(), manifest.wire_hash.end(),
                              [](uint8_t b) { return b != 0; }),
                  "empty capture manifest hash");
  uint64_t words = 0;
  std::set<std::pair<uint8_t, uint16_t>> selectors;
  for (const auto &entry : manifest.entries) {
    require_capture(entry.width > 0 && entry.width <= 64 && entry.depth > 0,
                    "unsupported capture descriptor");
    require_capture(selectors.emplace(entry.bank, entry.slot).second,
                    "duplicate capture selector");
    require_capture(words <= std::numeric_limits<uint64_t>::max() - entry.depth,
                    "capture word count overflow");
    words += entry.depth;
  }
  return words;
}

size_t frame_bytes(uint64_t words) {
  require_capture(words <= std::numeric_limits<size_t>::max() / 64 - 2,
                  "capture byte count overflow");
  return static_cast<size_t>(words + 2) * 64;
}

uint32_t crc32c(const uint8_t *bytes, size_t length) {
  uint32_t value = 0xffffffff;
  for (size_t i = 0; i < length; ++i) {
    value ^= bytes[i];
    for (unsigned bit = 0; bit < 8; ++bit)
      value = (value >> 1) ^ ((value & 1) ? 0x82f63b78 : 0);
  }
  return ~value;
}
} // namespace

checkpoint_capture_session_t::checkpoint_capture_session_t(
    checkpoint_capture_io_t &io, checkpoint_capture_manifest_t manifest)
    : io(io), manifest(std::move(manifest)), words(manifest_words(this->manifest)),
      total_bytes(frame_bytes(words)) {}

void checkpoint_capture_session_t::validate_frames(
    const std::vector<uint8_t> &frames,
    const checkpoint_capture_manifest_t &manifest, uint64_t expected_cycle) {
  const uint64_t words = manifest_words(manifest);
  require_capture(frames.size() == frame_bytes(words),
                  "truncated or extra capture frames");
  for (size_t offset = 0; offset < frames.size(); offset += 64) {
    const auto *frame = frames.data() + offset;
    require_capture(std::memcmp(frame, "FSMCKPT1", 8) == 0 &&
                        load_le(frame + 8, 2) == 1 &&
                        load_le(frame + 12, 8) == expected_cycle,
                    "capture magic, version or target cut mismatch");
  }
  const auto *header = frames.data();
  const auto *trailer = frames.data() + frames.size() - 64;
  require_capture(load_le(header + 10, 2) == 1 &&
                      load_le(header + 20, 8) == 0 &&
                      load_le(header + 28, 4) == manifest.entries.size() &&
                      std::equal(manifest.wire_hash.begin(), manifest.wire_hash.end(),
                                 header + 32),
                  "incompatible capture manifest/header");
  require_capture(load_le(trailer + 10, 2) == 3 &&
                      load_le(trailer + 20, 8) == words &&
                      load_le(trailer + 28, 4) ==
                          crc32c(frames.data(), frames.size() - 64) &&
                      std::all_of(trailer + 32, trailer + 64,
                                  [](uint8_t b) { return b == 0; }),
                  "capture trailer/checksum mismatch");
  uint64_t ordinal = 0;
  for (size_t index = 0; index < manifest.entries.size(); ++index) {
    const auto &entry = manifest.entries[index];
    for (uint32_t address = 0; address < entry.depth; ++address, ++ordinal) {
      const auto *frame = frames.data() + (ordinal + 1) * 64;
      require_capture(load_le(frame + 10, 2) == 2 &&
                          load_le(frame + 20, 8) == ordinal &&
                          load_le(frame + 28, 4) == index &&
                          load_le(frame + 32, 4) == address &&
                          load_le(frame + 36, 2) == entry.width &&
                          frame[38] == entry.bank &&
                          load_le(frame + 39, 2) == entry.slot,
                      "capture state order/selector mismatch");
      require_capture(frame[41] == 0 &&
                          std::all_of(frame + 50, frame + 64,
                                      [](uint8_t b) { return b == 0; }),
                      "capture error/reserved bits");
      require_capture(entry.width == 64 ||
                          (load_le(frame + 42, 8) >> entry.width) == 0,
                      "noncanonical capture padding");
    }
  }
}

void checkpoint_capture_session_t::begin() {
  require_capture(state == phase_t::idle, "capture session is not idle");
  // All allocations precede the start command. No partial image is exposed.
  frames.assign(total_bytes, 0);
  received = 0;
  abort_issued = false;
  try {
    require_capture(io.read_capture_mmio(0x60) == 0 &&
                        io.read_capture_mmio(0x04) == 1,
                    "capture hardware not ready or stale stream invalid");
    require_capture(stream_accounting_valid && read_frozen_u64(0x68) == consumed_frames,
                    "unconsumed capture history or coordinated stream reset required");
    require_capture(io.read_capture_mmio(0x20) == manifest.entries.size(),
                    "hardware capture entry count mismatch");
    for (unsigned i = 0; i < 8; ++i)
      require_capture(io.read_capture_mmio(0x40 + i * 4) ==
                          load_le(manifest.wire_hash.data() + i * 4, 4),
                      "hardware capture manifest mismatch");
    state = phase_t::receiving; // an exception during MMIO may have accepted start
    io.write_capture_mmio(0, 1);
  } catch (...) {
    state = phase_t::failed;
    throw;
  }
}

uint64_t checkpoint_capture_session_t::read_frozen_u64(uint32_t offset) {
  const uint32_t low = io.read_capture_mmio(offset);
  const uint32_t high = io.read_capture_mmio(offset + 4);
  return uint64_t(low) | (uint64_t(high) << 32);
}

size_t checkpoint_capture_session_t::pull_counted(void *buffer, size_t capacity) {
  size_t count;
  try {
    count = io.pull_capture_stream(buffer, capacity);
  } catch (...) {
    // An interrupted platform transfer may already have consumed bytes. No
    // guessed count can safely acknowledge that history after cancellation.
    stream_accounting_valid = false;
    throw;
  }
  if (count > capacity || count % 64 != 0 ||
      consumed_frames > std::numeric_limits<uint64_t>::max() - count / 64) {
    stream_accounting_valid = false;
    throw std::runtime_error("invalid stream transfer size/accounting; coordinated reset required");
  }
  consumed_frames += count / 64;
  return count;
}

void checkpoint_capture_session_t::check_frozen() {
  const uint32_t status = io.read_capture_mmio(0x04);
  require_capture((status & 2) != 0 && (status & 0x30) == 0 &&
                      io.read_capture_mmio(0x08) == 0 &&
                      io.read_capture_mmio(0x60) == 0,
                  "capture lost freeze, failed or invalidated shared stream");
}

bool checkpoint_capture_session_t::poll() {
  require_capture(state == phase_t::receiving || state == phase_t::validated,
                  "capture session cannot poll");
  try {
    check_frozen();
    if (state == phase_t::validated) {
      require_capture(read_frozen_u64(0x10) == cycle,
                      "validated capture cycle changed");
      require_capture(read_frozen_u64(0x68) == consumed_frames,
                      "validated capture stream count changed");
      return true;
    }
    alignas(4096) std::array<uint8_t, 4096> buffer{};
    io.flush_capture_stream();
    const size_t capacity = std::min(buffer.size(), total_bytes - received);
    if (capacity) {
      const size_t count = pull_counted(buffer.data(), capacity);
      std::copy_n(buffer.begin(), count, frames.begin() + received);
      received += count;
    }
    check_frozen();
    if (received != total_bytes || !(io.read_capture_mmio(0x04) & 8))
      return false;
    cycle = read_frozen_u64(0x10);
    require_capture(read_frozen_u64(0x18) == words,
                    "hardware capture word count mismatch");
    validate_frames(frames, manifest, cycle);
    require_capture(io.read_capture_mmio(0x24) ==
                        load_le(frames.data() + frames.size() - 36, 4),
                    "hardware capture CRC readback mismatch");
    // No extra frame may remain queued at this completed cut.
    require_capture(pull_counted(buffer.data(), 64) == 0,
                    "extra hardware capture stream frame");
    require_capture(read_frozen_u64(0x68) == consumed_frames,
                    "completed capture stream count mismatch");
    check_frozen();
    require_capture(read_frozen_u64(0x10) == cycle,
                    "capture cycle drift during validation");
    state = phase_t::validated;
    return true;
  } catch (...) {
    state = phase_t::failed;
    throw;
  }
}

uint64_t checkpoint_capture_session_t::cut_cycle() const {
  require_capture(state == phase_t::validated, "capture has not been validated");
  return cycle;
}

const std::vector<uint8_t> &checkpoint_capture_session_t::validated_frames() const {
  require_capture(state == phase_t::validated, "capture has not been validated");
  return frames;
}

checkpoint_live_widget_shadow_t
checkpoint_capture_session_t::capture_live_widget_shadow(uint32_t bits) {
  require_capture(state == phase_t::validated && !shadow_may_be_occupied,
                  "live widget shadow requires a validated, unoccupied capture");
  // Layout 2: 411 nonphysical or 481+GATES physical control bits, plus 228
  // local bits and 530 canonical NASTI adapter bits. Layout 1 is incompatible.
  // Bound allocation before any MMIO mutation; the generated header is the
  // caller's exact shape authority, not an arbitrary runtime word count.
  require_capture(bits == 1169 || (bits >= 1240 && bits <= 66774),
                  "unsupported live widget shadow shape");
  checkpoint_live_widget_shadow_t result{bits, cycle,
                                        std::vector<uint64_t>((bits + 63) / 64)};
  try {
    poll();
    require_capture(io.read_capture_mmio(0x98) == 2 && io.read_capture_mmio(0x9c) == bits,
                    "live widget shadow layout differs from generated header");
    require_capture(io.read_capture_mmio(0x88) == 0,
                    "existing live widget shadow must not be overwritten");
    require_capture(io.read_capture_mmio(0x94) == result.words.size(),
                    "live widget shadow word count differs from generated header");
    // A failing MMIO call might already have accepted its write. Recovery must
    // discard this uncertain epoch explicitly, without publishing any result.
    shadow_may_be_occupied = true;
    io.write_capture_mmio(0x78, 1);
    require_capture(io.read_capture_mmio(0x88) == 1 &&
                        read_frozen_u64(0x8c) == cycle,
                    "live widget shadow cut was not acknowledged");
    for (size_t i = 0; i < result.words.size(); ++i) {
      io.write_capture_mmio(0x7c, static_cast<uint32_t>(i));
      require_capture(io.read_capture_mmio(0x7c) == i,
                      "live widget shadow selector readback differs");
      result.words[i] = read_frozen_u64(0x80);
    }
    require_capture(bits % 64 == 0 || (result.words.back() >> (bits % 64)) == 0,
                    "noncanonical live widget shadow padding");
    check_frozen();
    require_capture(io.read_capture_mmio(0x88) == 1 &&
                        read_frozen_u64(0x8c) == cycle &&
                        read_frozen_u64(0x10) == cycle,
                    "live widget shadow changed during readout");
    io.write_capture_mmio(0x78, 2);
    require_capture(io.read_capture_mmio(0x88) == 0,
                    "live widget shadow release was not acknowledged");
    shadow_may_be_occupied = false;
    return result;
  } catch (...) {
    state = phase_t::failed;
    throw;
  }
}

void checkpoint_capture_session_t::release_after_publication() {
  require_capture(state == phase_t::validated,
                  "cannot release an unpublished/unvalidated capture");
  try {
    poll(); // recheck freeze, manifest-bound cycle and owner fault state
    io.write_capture_mmio(0, 2);
    require_capture(io.read_capture_mmio(0x04) == 1 &&
                        io.read_capture_mmio(0x60) == 0,
                    "capture publication was not acknowledged");
    state = phase_t::idle;
  } catch (...) {
    state = phase_t::failed;
    throw;
  }
}

bool checkpoint_capture_session_t::abort_and_drain() {
  require_capture(state != phase_t::idle, "no capture to abort");
  state = phase_t::failed;
  require_capture(stream_accounting_valid,
                  "unknown consumed stream bytes require coordinated reset");
  if (shadow_may_be_occupied) {
    io.write_capture_mmio(0x78, 4);
    require_capture(io.read_capture_mmio(0x88) == 0,
                    "live widget shadow discard was not acknowledged");
    shadow_may_be_occupied = false;
  }
  if (!abort_issued) {
    io.write_capture_mmio(0, 4);
    abort_issued = true;
  }
  alignas(4096) std::array<uint8_t, 4096> buffer{};
  io.flush_capture_stream();
  const size_t count = pull_counted(buffer.data(), buffer.size());
  // Ready is asserted only after accepted bank responses drain. Producer
  // quiescence plus an empty stream must precede the explicit invalid ack.
  if (count || io.read_capture_mmio(0x04) != 1)
    return false;
  // An empty nonblocking pull is not a drain barrier: old beats can still be
  // in upstream queues. Compare against the widget's lifetime accepted count.
  if (read_frozen_u64(0x68) != consumed_frames)
    return false;
  io.write_capture_mmio(0x70, uint32_t(consumed_frames));
  io.write_capture_mmio(0x74, uint32_t(consumed_frames >> 32));
  io.write_capture_mmio(0x64, 1);
  require_capture(io.read_capture_mmio(0x60) == 0,
                  "shared stream drain acknowledgement failed");
  frames.clear();
  received = 0;
  state = phase_t::idle;
  return true;
}
