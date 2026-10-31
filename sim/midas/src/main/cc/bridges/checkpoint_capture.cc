// See LICENSE for license details.
#include "bridges/checkpoint_capture.h"

#include <limits>
#include <stdexcept>
#include <utility>

char checkpoint_capture_t::KIND;

namespace {
StreamEngine &require_capture_stream(StreamEngine *stream) {
  if (!stream)
    throw std::invalid_argument("capture stream engine missing from registry");
  return *stream;
}

void validate_capture_location(StreamEngine &stream, size_t base, unsigned index) {
  if ((base & 0xff) || base > std::numeric_limits<size_t>::max() - 0x9c)
    throw std::invalid_argument("capture widget base must be 256-byte aligned");
  if (index >= static_cast<unsigned>(stream.fpga_to_cpu_cnt()))
    throw std::invalid_argument("capture stream index missing from engine");
}
} // namespace

checkpoint_capture_t::checkpoint_capture_t(
    simif_t &sim, StreamEngine &stream, size_t mmio_base, unsigned stream_index,
    checkpoint_capture_manifest_t manifest)
    : streaming_bridge_driver_t(sim, stream, &KIND), base(mmio_base),
      stream_index(stream_index), session(*this, std::move(manifest)) {
  validate_capture_location(stream, base, stream_index);
}

checkpoint_capture_t::checkpoint_capture_t(
    simif_t &sim, StreamEngine *stream, size_t mmio_base, unsigned stream_index,
    checkpoint_capture_manifest_t manifest, checkpoint_native_capture_binding_t binding)
    : streaming_bridge_driver_t(sim, require_capture_stream(stream), &KIND), base(mmio_base),
      stream_index(stream_index), session(*this, std::move(manifest), binding) {
  validate_capture_location(*stream, base, stream_index);
}

uint32_t checkpoint_capture_t::read_capture_mmio(uint32_t offset) {
  return read(base + offset);
}

void checkpoint_capture_t::write_capture_mmio(uint32_t offset, uint32_t value) {
  write(base + offset, value);
}

size_t checkpoint_capture_t::pull_capture_stream(void *buffer, size_t capacity) {
  return pull(stream_index, buffer, capacity, 0);
}

void checkpoint_capture_t::flush_capture_stream() {
  pull_flush(stream_index);
}
