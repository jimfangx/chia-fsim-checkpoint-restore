// See LICENSE for license details.
#ifndef FIRESIM_CHECKPOINT_CAPTURE_H
#define FIRESIM_CHECKPOINT_CAPTURE_H

#include <array>
#include <cstddef>
#include <cstdint>
#include <vector>

// Generated immutable manifest metadata, in on-wire descriptor order. This is
// independent of both canonical checkpoint containers and host-owner state.
struct checkpoint_capture_entry_t {
  uint8_t bank;
  uint16_t slot;
  uint16_t width;
  uint32_t depth;
};

struct checkpoint_capture_manifest_t {
  // SHA256 interpreted as an integer, least-significant byte first, matching
  // the transport's 256-bit parameter and low-word-first MMIO registers.
  std::array<uint8_t, 32> wire_hash;
  std::vector<checkpoint_capture_entry_t> entries;
};

// Observation of the live transport at a held target cut, layout version 2.
// This is not a canonical Level-1 image or a complete/restore-capable owner.
struct checkpoint_live_widget_shadow_t {
  uint32_t bits;
  uint64_t cut_cycle;
  std::vector<uint64_t> words;
};

class checkpoint_capture_io_t {
public:
  virtual ~checkpoint_capture_io_t() = default;
  virtual uint32_t read_capture_mmio(uint32_t byte_offset) = 0;
  virtual void write_capture_mmio(uint32_t byte_offset, uint32_t value) = 0;
  // Nonblocking, frame-aligned transfer. The supplied buffer is page aligned.
  virtual size_t pull_capture_stream(void *buffer, size_t capacity) = 0;
  virtual void flush_capture_stream() = 0;
};

// Host side of the synthesized transport. No target edges or ordinary bridge
// tick calls are made here. A global coordinator must freeze/snapshot all host
// owners too. This component deliberately does not grant a complete CPU schema
// or publish .fsckpt/.rtlckpt: callers must independently validate both levels,
// memory, host owners, provenance, and durable publication before release().
class checkpoint_capture_session_t {
public:
  enum class phase_t { idle, receiving, validated, failed };

  checkpoint_capture_session_t(checkpoint_capture_io_t &io,
                               checkpoint_capture_manifest_t manifest);
  void begin();
  // Bounded, nonblocking service. False means more hardware/stream work needed.
  // All faults retain freeze until explicit abort_and_drain().
  bool poll();
  uint64_t cut_cycle() const;
  const std::vector<uint8_t> &validated_frames() const;
  // Pass SHADOW_BITS from the exact generated image/driver header. Requires a
  // validated, still-frozen capture. Failed reads retain freeze and any staging
  // epoch until explicit abort_and_drain(); no ordinary bridge tick is issued.
  checkpoint_live_widget_shadow_t capture_live_widget_shadow(uint32_t bits);
  void release_after_publication();
  // May need repeated calls. Invalid shared-stream history is discarded, never
  // interpreted as a later capture. No automatic timeout silently releases it.
  bool abort_and_drain();
  phase_t phase() const { return state; }

  static void validate_frames(const std::vector<uint8_t> &frames,
                              const checkpoint_capture_manifest_t &manifest,
                              uint64_t expected_cycle);

private:
  checkpoint_capture_io_t &io;
  const checkpoint_capture_manifest_t manifest;
  const uint64_t words;
  const size_t total_bytes;
  phase_t state = phase_t::idle;
  std::vector<uint8_t> frames;
  size_t received = 0;
  uint64_t cycle = 0;
  bool abort_issued = false;
  uint64_t consumed_frames = 0;
  bool stream_accounting_valid = true;
  bool shadow_may_be_occupied = false;
  uint64_t read_frozen_u64(uint32_t offset);
  size_t pull_counted(void *buffer, size_t capacity);
  void check_frozen();
};

#endif
