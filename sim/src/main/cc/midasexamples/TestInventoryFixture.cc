// See LICENSE for license details.
#include "TestHarness.h"
#include <charconv>
#include <stdexcept>

class TestInventoryFixture final : public TestHarness {
public:
  TestInventoryFixture(widget_registry_t &registry,
                       const std::vector<std::string> &args,
                       std::string_view name) : TestHarness(registry, args, name) {
    bool seen = false;
    for (const auto &arg : args) {
      const std::string_view prefix = "+inventory-shadow-rounds=";
      if (arg.compare(0, prefix.size(), prefix) != 0) continue;
      const auto value = std::string_view(arg).substr(prefix.size());
      const auto result = std::from_chars(value.data(), value.data() + value.size(), shadow_rounds);
      if (seen || value.empty() || value.front() == '0' || result.ec != std::errc{} ||
          result.ptr != value.data() + value.size() || shadow_rounds == 0 || shadow_rounds > 1000000)
        throw std::invalid_argument("invalid +inventory-shadow-rounds");
      seen = true;
    }
  }

  void run_test() override {
    poke("io_data", 0);
    poke("io_addr", 0);
    poke("io_wen", 0);
    poke("io_reqValid", 0);
    poke("io_respReady", 0);
    target_reset();
    // A literal zero can select expect(bool, const char *) via null-pointer
    // conversion. An explicit signal type selects the value-checking overload.
    expect(std::string_view{"io_count"}, 0);
    expect(std::string_view{"io_phase"}, 0);
    expect(std::string_view{"io_left"}, 0);
    expect(std::string_view{"io_right"}, 0);

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

    // Fill the response slot, leave the request queue occupied, and hold
    // backpressure. Capture may occur at any of these edges, without draining.
    poke("io_addr", 3);
    poke("io_reqValid", 1);
    step(1);
    expect("io_queued", 1);
    poke("io_reqValid", 0);
    step(1);
    expect("io_outstanding", 1);
    poke("io_addr", 5);
    poke("io_reqValid", 1);
    step(1);
    expect("io_queued", 1);
    expect("io_outstanding", 1);
    poke("io_reqValid", 0);
    step(1);
    expect("io_respValid", 1);
    expect("io_respData", 0x103);
    expect("io_queued", 1);
    step(4);
    expect("io_respValid", 1);
    expect("io_respData", 0x103);
    expect("io_queued", 1);
    // Release the held response and execute the queued operation.
    poke("io_respReady", 1);
    step(4);
    expect("io_respValid", 1);
    expect("io_respData", 0x105);
    step(1);
    expect(std::string_view{"io_respValid"}, 0);
    step(3);

    // Optional sustained live-shadow workload. Keep the complete original
    // directed test above, then alternate a held response and active traffic.
    // Request address stays stable throughout each valid interval.
    if (shadow_rounds) log = false;
    for (uint32_t round = 0; round < shadow_rounds; ++round) {
      poke("io_reqValid", 0);
      poke("io_addr", round % 8);
      poke("io_data", round % 512);
      poke("io_wen", 1);
      step(1);
      poke("io_wen", 0);
      poke("io_respReady", 0);
      poke("io_reqValid", 1);
      step(32);
      expect("io_respValid", 1);
      expect("io_queued", 1);
      poke("io_respReady", 1);
      step(64);
      poke("io_reqValid", 0);
      step(8);
      expect(std::string_view{"io_respValid"}, 0);
    }
  }

private:
  uint32_t shadow_rounds = 0;
};

TEST_MAIN(TestInventoryFixture)
