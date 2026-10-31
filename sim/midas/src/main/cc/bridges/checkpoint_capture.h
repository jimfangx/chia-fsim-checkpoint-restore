// See LICENSE for license details.
#ifndef FIRESIM_CHECKPOINT_CAPTURE_DRIVER_H
#define FIRESIM_CHECKPOINT_CAPTURE_DRIVER_H

#include "core/bridge_driver.h"
#include "core/checkpoint_capture.h"

// Concrete MMIO/StreamEngine binding. Construction and coordinated invocation
// are opt-in until the entire generated simulator and host owners are wired.
// This driver cannot confer aggregate checkpoint support on the runtime.
class checkpoint_capture_t : public streaming_bridge_driver_t,
                             private checkpoint_capture_io_t {
public:
  static char KIND;
  checkpoint_capture_t(simif_t &sim, StreamEngine &stream, size_t mmio_base,
                       unsigned stream_index,
                       checkpoint_capture_manifest_t manifest);
  // Generated native installation checks the nullable registry engine before
  // dereferencing it and binds the complete stream table/observation layout.
  checkpoint_capture_t(simif_t &sim, StreamEngine *stream, size_t mmio_base,
                       unsigned stream_index,
                       checkpoint_capture_manifest_t manifest,
                       checkpoint_native_capture_binding_t binding);
  checkpoint_capture_session_t &capture_session() { return session; }

private:
  const size_t base;
  const unsigned stream_index;
  checkpoint_capture_session_t session;
  uint32_t read_capture_mmio(uint32_t offset) override;
  void write_capture_mmio(uint32_t offset, uint32_t value) override;
  size_t pull_capture_stream(void *buffer, size_t capacity) override;
  void flush_capture_stream() override;
};

#endif
