// See LICENSE for license details.

#ifndef __EMUL_MM_H
#define __EMUL_MM_H

#include "core/config.h"
#include "core/checkpoint.h"

#include <cstring>
#include <memory>
#include <queue>
#include <stdint.h>

#include "core/config.h"

class mm_t : public checkpoint_owner_t {
public:
  mm_t(const AXI4Config conf)
      : conf(conf), word_size(conf.beat_bytes()), data(nullptr), size(0),
        line_size(0) {}
  mm_t(const mm_t &) = delete;
  mm_t &operator=(const mm_t &) = delete;
  mm_t(mm_t &&) = delete;
  mm_t &operator=(mm_t &&) = delete;

  virtual void init(size_t sz, int lsz);

  virtual bool ar_ready() = 0;
  virtual bool aw_ready() = 0;
  virtual bool w_ready() = 0;
  virtual bool b_valid() = 0;
  virtual uint64_t b_resp() = 0;
  virtual uint64_t b_id() = 0;
  virtual bool r_valid() = 0;
  virtual uint64_t r_resp() = 0;
  virtual uint64_t r_id() = 0;
  virtual void *r_data() = 0;
  virtual bool r_last() = 0;

  virtual void tick(bool reset,

                    bool ar_valid,
                    uint64_t ar_addr,
                    uint64_t ar_id,
                    uint64_t ar_size,
                    uint64_t ar_len,

                    bool aw_valid,
                    uint64_t aw_addr,
                    uint64_t aw_id,
                    uint64_t aw_size,
                    uint64_t aw_len,

                    bool w_valid,
                    uint64_t w_strb,
                    const std::vector<uint32_t> &w_data,
                    bool w_last,

                    bool r_ready,
                    bool b_ready) = 0;

  virtual void *get_data() { return data; }
  virtual size_t get_size() { return size; }
  virtual size_t get_word_size() { return word_size; }
  virtual size_t get_line_size() { return line_size; }

  void write(uint64_t addr, const uint8_t *data, uint64_t strb, uint64_t size);
  std::vector<char> read(uint64_t addr);

  virtual ~mm_t();

  void load_mem(unsigned long start, const char *fname);

  const AXI4Config &get_config() const { return conf; }

protected:
  const AXI4Config conf;

  int word_size;
  uint8_t *data;
  size_t size;
  int line_size;
};

struct mm_rresp_t {
  uint64_t id = 0;
  std::vector<char> data;
  bool last = false;

  mm_rresp_t() = default;

  mm_rresp_t(uint64_t id, const std::vector<char> &data, bool last)
      : id(id), data(data), last(last) {}
};

class mm_magic_t final : public mm_t {
public:
  mm_magic_t(const AXI4Config &conf)
      : mm_t(conf), store_inflight(false), store_addr(0), store_id(0),
        store_size(0), store_count(0), cycle(0) {}

  // Owner payload only: the future full-runtime coordinator must freeze both
  // contexts, bind this schema/geometry into its integrity-checked container,
  // and validate ALL owners before loading any. No tick or MMIO occurs here.
  std::string checkpoint_schema() const override {
    return "firesim.mm_magic.full.v1";
  }
  std::vector<uint8_t> save_checkpoint() const override;
  void validate_checkpoint(const std::vector<uint8_t> &) const override;
  void load_checkpoint(const std::vector<uint8_t> &) override;

  class restore_t {
  public:
    ~restore_t();
    restore_t(const restore_t &) = delete;
    restore_t &operator=(const restore_t &) = delete;
  private:
    friend class mm_magic_t;
    struct impl_t;
    explicit restore_t(std::unique_ptr<impl_t>);
    std::unique_ptr<impl_t> impl;
  };
  // Component transaction only. The coordinator must exclude both simulator
  // contexts and every backing-memory user from preparation through readback.
  // Seal finishes all allocating/throwing destination checks before any owner
  // in the aggregate publishes. Abandoning a candidate publishes nothing.
  std::unique_ptr<restore_t> prepare_checkpoint(const std::vector<uint8_t> &);
  void seal_checkpoint(restore_t &);
  // Preserves the mmap address, so existing backing-memory aliases stay valid.
  // A broken lifetime/thread/seal/generation contract terminates before copy.
  void publish_sealed_checkpoint(restore_t &) noexcept;

  void init(size_t sz, int lsz) override;

  bool ar_ready() override { return true; }
  bool aw_ready() override { return !store_inflight; }
  bool w_ready() override { return store_inflight; }
  bool b_valid() override { return !bresp.empty(); }
  uint64_t b_resp() override { return 0; }
  uint64_t b_id() override { return b_valid() ? bresp.front() : 0; }
  bool r_valid() override { return !rresp.empty(); }
  uint64_t r_resp() override { return 0; }
  uint64_t r_id() override { return r_valid() ? rresp.front().id : 0; }
  void *r_data() override {
    return r_valid() ? &rresp.front().data[0] : &dummy_data[0];
  }
  bool r_last() override { return r_valid() ? rresp.front().last : false; }

  void tick(bool reset,

            bool ar_valid,
            uint64_t ar_addr,
            uint64_t ar_id,
            uint64_t ar_size,
            uint64_t ar_len,

            bool aw_valid,
            uint64_t aw_addr,
            uint64_t aw_id,
            uint64_t aw_size,
            uint64_t aw_len,

            bool w_valid,
            uint64_t w_strb,
            const std::vector<uint32_t> &w_data,
            bool w_last,

            bool r_ready,
            bool b_ready) override;

protected:
  bool store_inflight;
  uint64_t store_addr;
  uint64_t store_id;
  uint64_t store_size;
  uint64_t store_count;
  std::vector<char> dummy_data;
  std::queue<uint64_t> bresp;

  std::queue<mm_rresp_t> rresp;

  uint64_t cycle;

private:
  const std::shared_ptr<const uint8_t> checkpoint_identity = std::make_shared<const uint8_t>(0);
  uint64_t checkpoint_generation = 0;
};

#endif // __EMUL_MM_H
