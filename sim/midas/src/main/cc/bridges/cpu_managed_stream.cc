#include "cpu_managed_stream.h"
#include "core/simif.h"

#include <cassert>

using namespace CPUManagedStreams;

namespace {
widget_checkpoint_config_t stream_config(
    unsigned index, uint64_t beat_bytes,
    const std::vector<StreamParameters> &from_cpu,
    const std::vector<StreamParameters> &to_cpu) {
  widget_checkpoint_config_t config(7, {index, beat_bytes});
  // Direction and generated order are significant: runtime calls use indices.
  for (const auto *streams : {&from_cpu, &to_cpu}) {
    config.word(streams->size());
    for (const auto &params : *streams) {
      config.text(params.stream_name);
      config.word(params.dma_addr);
      config.word(params.count_addr);
      config.word(params.fpga_buffer_size);
      config.word(params.fpga_buffer_width_bytes);
    }
  }
  return config;
}
} // namespace

std::vector<uint8_t> CPUManagedStreamWidget::save_checkpoint() const {
  return checkpoint_state.save(checkpoint_config, initialized);
}

void CPUManagedStreamWidget::validate_checkpoint(
    const std::vector<uint8_t> &payload) const {
  checkpoint_state.validate(payload, checkpoint_config, 1);
}

void CPUManagedStreamWidget::load_checkpoint(const std::vector<uint8_t> &payload) {
  // Full validation precedes mutation; hooks perform no MMIO/DMA/init calls.
  const auto flags = checkpoint_state.validate(payload, checkpoint_config, 1);
  publication.changed();
  initialized = flags;
}

/**
 * @brief Enqueues as much as num_bytes of data into the associated stream
 *
 * @param src Source from which to copy data to enqueue
 * @param num_bytes Desired number of bytes to enqueue
 * @param required_bytes Minimum number of bytes to enqueue. If fewer bytes
 *        would be enqueued, this method enqueues none and returns 0.
 * @return size_t
 */
size_t CPUManagedStreams::CPUToFPGADriver::push(void *src,
                                                size_t num_bytes,
                                                size_t required_bytes) {

  assert(num_bytes >= required_bytes);

  // Similarly to above, the legacy implementation of DMA does not correctly
  // implement non-multiples of 512b. The FPGA-side queue will take on the
  // high-order bytes of the final beat in the transaction, and the strobe is
  // not respected. So put the assertion here and discuss what to do next.
  assert((num_bytes % fpga_buffer_width_bytes()) == 0);

  auto num_beats = num_bytes / fpga_buffer_width_bytes();
  auto threshold_beats = required_bytes / fpga_buffer_width_bytes();

  assert(threshold_beats <= fpga_buffer_size());
  auto space_available = fpga_buffer_size() - mmio_read(count_addr());

  if ((space_available == 0) || (space_available < threshold_beats)) {
    return 0;
  }

  auto push_beats = std::min(space_available, num_beats);
  auto push_bytes = push_beats * fpga_buffer_width_bytes();
  auto bytes_written =
      cpu_managed_axi4_write(dma_addr(), (char *)src, push_bytes);
  assert(bytes_written == push_bytes);

  return bytes_written;
}

/**
 * @brief Dequeues as much as num_bytes of data from the associated bridge
 * stream.
 *
 * @param dest  Buffer into which to copy dequeued stream data
 * @param num_bytes  Bytes of data to dequeue
 * @param required_bytes  Minimum number of bytes to dequeue. If fewer bytes
 * would be dequeued, dequeue none and return 0.
 * @return size_t Number of bytes successfully dequeued
 */
size_t CPUManagedStreams::FPGAToCPUDriver::pull(void *dest,
                                                size_t num_bytes,
                                                size_t required_bytes) {
  assert(num_bytes >= required_bytes);

  // The legacy code is clearly broken for requests that aren't a
  // multiple of 512b since CPU_MANAGED_AXI4_SIZE is fixed to the full width of
  // the AXI4 IF. The high-order bytes of the final word will be copied into the
  // destination buffer (potentially an overflow, bug 1), and since reads are
  // destructive, will not be visible to future pulls (bug 2). So i've put this
  // assertion here for now...

  // Due to the destructive nature of reads, if we wish to support reads that
  // aren't a multiple of 512b, we'll need to keep a little buffer around for
  // the remainder, and prepend this to the destination buffer.
  assert((num_bytes % fpga_buffer_width_bytes()) == 0);

  auto num_beats = num_bytes / fpga_buffer_width_bytes();
  auto threshold_beats = required_bytes / fpga_buffer_width_bytes();

  assert(threshold_beats <= fpga_buffer_size());
  auto count = mmio_read(count_addr());

  if ((count == 0) || (count < threshold_beats)) {
    return 0;
  }

  auto pull_beats = std::min(count, num_beats);
  auto pull_bytes = pull_beats * fpga_buffer_width_bytes();
  auto bytes_read = cpu_managed_axi4_read(dma_addr(), (char *)dest, pull_bytes);
  assert(bytes_read == pull_bytes);
  return bytes_read;
}

CPUManagedStreamWidget::CPUManagedStreamWidget(
    simif_t &simif,
    unsigned index,
    const std::vector<std::string> &args,
    std::vector<CPUManagedStreams::StreamParameters> &&from_cpu,
    std::vector<CPUManagedStreams::StreamParameters> &&to_cpu)
    : checkpoint_config(stream_config(index,
                                     simif.get_cpu_managed_stream_io().get_beat_bytes(),
                                     from_cpu, to_cpu)) {
  assert(index == 0 && "only one managed stream engine is allowed");

  auto &io = simif.get_cpu_managed_stream_io();
  for (auto &&params : from_cpu) {
    cpu_to_fpga_streams.push_back(
        std::make_unique<CPUManagedStreams::CPUToFPGADriver>(std::move(params),
                                                             io));
  }

  for (auto &&params : to_cpu) {
    fpga_to_cpu_streams.push_back(
        std::make_unique<CPUManagedStreams::FPGAToCPUDriver>(std::move(params),
                                                             io));
  }
}

// Returned-call host state only. Generated queues, MMIO/DMA and caller buffers
// remain separate owners. The coordinator retains exclusion through readback.
class CPUManagedStreamWidget::restore_t final : public prepared_checkpoint_t {
  CPUManagedStreamWidget &owner;
  checkpoint_publication_t::guard_t guard;
  const std::vector<uint8_t> expected;
  const uint64_t replacement;
public:
  restore_t(CPUManagedStreamWidget &owner, const std::vector<uint8_t> &payload)
      : owner(owner), guard(owner.publication), expected(owner.save_checkpoint()),
        replacement(owner.checkpoint_state.validate(payload, owner.checkpoint_config, 1)) {}
  void seal() override {
    guard.require_unsealed(); // Check lifetime/thread/generation before owner access.
    if (owner.save_checkpoint() != expected)
      throw std::runtime_error("cpu_managed_stream checkpoint destination changed");
    guard.seal();
  }
  void publish() noexcept override {
    guard.publish();
    owner.initialized = replacement;
  }
};

std::unique_ptr<prepared_checkpoint_t>
CPUManagedStreamWidget::prepare_checkpoint_publication(const std::vector<uint8_t> &payload) {
  return std::make_unique<restore_t>(*this, payload);
}
