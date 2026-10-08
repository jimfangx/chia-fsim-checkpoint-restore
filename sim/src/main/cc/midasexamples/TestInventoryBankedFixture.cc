// See LICENSE for license details.
#include "TestHarness.h"
#include <cinttypes>
#include <cstdio>

class TestInventoryBankedFixture final : public TestHarness {
public:
  using TestHarness::TestHarness;

  void capture(unsigned id, bool busy_error = false, bool fault = false) {
    poke("cp_take", 0);
    poke("cp_fault", fault);
    poke("cp_start", 1);
    step(1);
    poke("cp_start", 0);
    step(5); // Fill both FIFO slots and deliberately stall the producer.
    expect("cp_freeze", 1);
    const auto count = peek("io_count");
    const auto phase = peek("io_phase");
    if (busy_error) {
      poke("cp_start", 1);
      step(1);
      poke("cp_start", 0);
      expect("cp_error", 2);
    }
    unsigned words = 0;
    bool last = false;
    while (!last && words < 24) {
      expect("cp_valid", 1);
      const auto entry = peek("cp_entry");
      const auto address = peek("cp_address");
      const auto width = peek("cp_width");
      mpz_t wide_data;
      mpz_init(wide_data);
      peek("cp_data", wide_data);
      const uint64_t data = mpz_get_ui(wide_data);
      const auto error = peek("cp_beatError");
      last = peek("cp_last");
      std::printf("BANKED_WORD capture=%u entry=%u address=%u width=%u data=%" PRIx64 " error=%u last=%u\n",
                  id, entry, address, width, data, error, last);
      // Change functional inputs while frozen, including a pending SRAM write
      // and reset. Verify all state through the independent oracle, not peeks.
      poke("io_data", (words * 17 + id) & 511);
      poke("io_addr", words % 8);
      poke("io_wen", 1);
      poke("reset", 1);
      step(2 + words % 3);
      expect("cp_entry", entry);
      expect("cp_address", address);
      expect("cp_data", wide_data);
      expect("cp_last", static_cast<uint32_t>(last));
      expect("io_count", count);
      expect("io_phase", phase);
      // Functional reset is separate from controller initialization. Its
      // semantic update is suppressed throughout extraction, like all writes.
      poke("reset", 0);
      poke("cp_take", 1);
      step(1);
      poke("cp_take", 0);
      if (!last) step(1);
      mpz_clear(wide_data);
      ++words;
    }
    expect(last, "checkpoint stream terminated");
    expect(words == (fault ? 1U : 22U), "complete logical word count");
    expect("cp_error", fault ? 4 : busy_error ? 2 : 0);
    poke("io_wen", 0);
    poke("cp_fault", 0);
    // Keep this edge frozen again for the repeat capture below; ordinary
    // semantic progress resumes only when the next step is requested.
    poke("cp_clearError", 1);
  }

  void run_test() override {
    poke("cp_start", 0);
    poke("cp_command", 0);
    poke("cp_clearError", 0);
    poke("cp_take", 0);
    poke("cp_fault", 0);
    poke("io_data", 0);
    poke("io_addr", 0);
    poke("io_wen", 0);
    poke("io_reqValid", 0);
    poke("io_respReady", 0);
    poke("cp_reset", 1);
    poke("reset", 1);
    step(1); // Initial semantic edge zero also initializes the controller.
    poke("cp_reset", 0);
    step(4);
    poke("reset", 0);
    for (unsigned i = 0; i < 8; ++i) {
      poke("io_data", 0x100 + i);
      poke("io_addr", i);
      poke("io_wen", 1);
      step(1);
    }
    poke("io_wen", 0);
    capture(0); // active state and all SRAM addresses
    capture(1, true); // immediate repeat, busy request must not restart
    poke("cp_start", 0);
    step(1);
    poke("cp_clearError", 0);
    poke("io_addr", 3);
    poke("io_reqValid", 1);
    step(1);
    poke("io_reqValid", 0);
    expect("io_queued", 1);
    capture(2);
    step(1);
    expect("io_outstanding", 1);
    capture(3);
    step(2);
    expect("io_respValid", 1);
    capture(4); // response held under functional backpressure
    step(1);
    poke("cp_clearError", 0);
    poke("cp_command", 3);
    poke("cp_start", 1);
    step(1);
    poke("cp_start", 0);
    expect("cp_error", 1);
    expect(std::string_view{"cp_freeze"}, 0);
    poke("cp_command", 0);
    poke("cp_clearError", 1);
    step(1);
    poke("cp_clearError", 0);
    capture(5, false, true);
    step(1);
    expect(std::string_view{"cp_error"}, 0);
    poke("cp_clearError", 0);
    capture(6); // recovery after explicit bank error
    poke("io_respReady", 1);
    step(1);
    expect(std::string_view{"io_respValid"}, 0);
  }
};

TEST_MAIN(TestInventoryBankedFixture)
