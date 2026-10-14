// See LICENSE for license details.
#include "TestHarness.h"

class TestMemoryPipelineFixture final : public TestHarness {
public:
  using TestHarness::TestHarness;
  void run_test() override {
    poke("io_enable", 0);
    poke("io_write", 0);
    poke("io_address", 0);
    poke("io_data", 0);
    target_reset();
    expect(std::string_view{"io_count"}, 0);
    poke("io_enable", 1);
    poke("io_write", 1);
    for (uint32_t i = 0; i < 8; ++i) {
      poke("io_address", i);
      poke("io_data", 0x100 + i);
      step(1);
      expect("io_readerNew", 0x100 + i);
    }
    poke("io_write", 0);
    for (uint32_t i = 0; i < 8; ++i) {
      poke("io_address", i);
      step(1);
      check_all(0x100 + i);
    }
    // Disable reads while changing address/data. The saved port state holds.
    poke("io_enable", 0);
    poke("io_address", 2);
    poke("io_data", 0x1a2);
    step(3);
    check_all(0x107);
    // Same-address read/write: the emitter saves old data for readwriters too.
    poke("io_enable", 1);
    poke("io_write", 1);
    step(1);
    expect("io_readerOld", 0x102);
    expect("io_readerNew", 0x1a2);
    expect("io_readwriterOld", 0x102);
    expect("io_readwriterUndefined", 0x102);
    poke("io_write", 0);
    step(1);
    check_all(0x1a2);
    // Overwrite the location behind the pending read, then disable the port.
    // Address-state readers see the new array; data-state readers retain data.
    poke("io_data", 0x155);
    poke("io_write", 1);
    step(1);
    expect("io_readerOld", 0x1a2);
    expect("io_readerNew", 0x155);
    expect("io_readwriterOld", 0x1a2);
    poke("io_enable", 0);
    poke("io_address", 6);
    step(2);
    expect("io_readerOld", 0x1a2);
    expect("io_readerNew", 0x155);
    expect("io_readwriterOld", 0x1a2);
    poke("io_enable", 1);
    poke("io_write", 0);
    for (uint32_t i = 0; i < 8; ++i) {
      poke("io_address", i);
      step(1);
      check_all(i == 2 ? 0x155 : 0x100 + i);
    }
  }
private:
  void check_all(uint32_t value) {
    for (const auto *signal : {"io_readerOld", "io_readerNew",
                               "io_readwriterOld", "io_readwriterUndefined"})
      expect(signal, value);
  }
};

TEST_MAIN(TestMemoryPipelineFixture)
