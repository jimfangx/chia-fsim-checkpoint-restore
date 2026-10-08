// See LICENSE for license details.
#include "TestHarness.h"

class TestInventoryFixture final : public TestHarness {
public:
  using TestHarness::TestHarness;

  void run_test() override {
    poke("io_data", 0);
    poke("io_addr", 0);
    poke("io_wen", 0);
    target_reset();
    expect("io_count", 0);
    expect("io_phase", 0);
    expect("io_left", 0);
    expect("io_right", 0);

    // Every step advances one target cycle, irrespective of host/model stalls.
    for (uint32_t i = 0; i < 8; ++i) {
      const uint32_t data = 0x100 + i;
      poke("io_data", data);
      poke("io_addr", i);
      poke("io_wen", 1);
      step(1);
      expect("io_count", i + 1);
      expect("io_phase", (i + 1) % 3);
      expect("io_array0", data);
      expect("io_array1", data + 1);
      expect("io_array2", data + 2);
      expect("io_left", data);
      expect("io_right", data + 1);
    }
    poke("io_wen", 0);
    for (uint32_t i = 0; i < 8; ++i) {
      poke("io_addr", i);
      step(1);
      expect("io_ram", 0x100 + i);
      expect("io_count", 9 + i);
      expect("io_phase", (9 + i) % 3);
    }
  }
};

TEST_MAIN(TestInventoryFixture)
