// See LICENSE for license details.
#include "bridges/checkpoint_capture.h"

#include <limits>
#include <stdexcept>
#include <utility>

char checkpoint_capture_t::KIND;

checkpoint_capture_t::checkpoint_capture_t(
    simif_t &sim, StreamEngine &stream, size_t mmio_base, unsigned stream_index,
    checkpoint_capture_manifest_t manifest)
    : streaming_bridge_driver_t(sim, stream, &KIND), base(mmio_base),
      stream_index(stream_index), session(*this, std::move(manifest)) {
  if ((base & 0xff) || base > std::numeric_limits<size_t>::max() - 0x74)
    throw std::invalid_argument("capture widget base must be 256-byte aligned");
  if (stream_index >= static_cast<unsigned>(stream.fpga_to_cpu_cnt()))
    throw std::invalid_argument("capture stream index missing from engine");
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
