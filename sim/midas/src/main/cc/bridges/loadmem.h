// See LICENSE for license details.

#ifndef __LOADMEM_H
#define __LOADMEM_H

#include <cstdint>
#include <string>
#include <vector>

#include <gmp.h>

#include "core/config.h"
#include "core/widget.h"
#include "core/widget_checkpoint.h"

class simif_t;

struct LOADMEMWIDGET_struct {
  uint64_t W_ADDRESS_H;
  uint64_t W_ADDRESS_L;
  uint64_t W_LENGTH;
  uint64_t ZERO_OUT_DRAM;
  uint64_t W_DATA;
  uint64_t ZERO_FINISHED;
  uint64_t R_ADDRESS_H;
  uint64_t R_ADDRESS_L;
  uint64_t R_DATA;
};

class loadmem_t final : public widget_t {
public:
  // Audited host state only; generated RTL/MMIO remain separate owners.
  std::string checkpoint_schema() const override { return "firesim.widget.loadmem.v1"; }
  std::vector<uint8_t> save_checkpoint() const override {
    return checkpoint_state.save(checkpoint_configuration());
  }
  void validate_checkpoint(const std::vector<uint8_t> &payload) const override {
    checkpoint_state.validate(payload, checkpoint_configuration());
  }
  void load_checkpoint(const std::vector<uint8_t> &payload) override {
    validate_checkpoint(payload);
  }

  /// The identifier for the bridge type.
  static char KIND;

  loadmem_t(simif_t &simif,
            const LOADMEMWIDGET_struct &mmio_addrs,
            unsigned index,
            const std::vector<std::string> &args,
            const AXI4Config &mem_conf,
            unsigned mem_data_chunk);

  void read_mem(size_t addr, mpz_t &value);
  void write_mem(size_t addr, mpz_t &value);
  void write_mem_chunk(size_t addr, mpz_t &value, size_t bytes);

  // Helper to zero out all DRAM.
  void zero_out_dram();

  // Loads the contents of memory from a file.
  void load_mem_from_file(const std::string &filename);

  unsigned get_mem_data_chunk() const { return mem_data_chunk; }

private:
  widget_checkpoint_state_t checkpoint_state;
  widget_checkpoint_config_t checkpoint_configuration() const;
  void write_mem_impl(size_t addr, mpz_t &value);
  const LOADMEMWIDGET_struct mmio_addrs;
  const AXI4Config mem_conf;
  const unsigned mem_data_chunk;
};

#endif // __LOADMEM_H
