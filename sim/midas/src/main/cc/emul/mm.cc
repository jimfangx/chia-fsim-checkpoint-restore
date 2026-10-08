// See LICENSE for license details.

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sys/mman.h>

#include "mm.h"

void mm_t::write(uint64_t addr,
                 const uint8_t *data,
                 uint64_t strb,
                 uint64_t size) {
  auto max_strb_bytes = sizeof(uint64_t) * 8;
  assert(size <= max_strb_bytes); // Ensure the strb is wide enough to support
                                  // the desired transaction
  if (size != max_strb_bytes) {
    strb &= ((uint64_t{1} << size) - 1) << (addr % word_size);
  }
  addr %= this->size;

  uint8_t *base = this->data + (addr / word_size) * word_size;
  for (int i = 0; i < word_size; i++) {
    if (strb & 1)
      base[i] = data[i];
    strb >>= 1;
  }
}

std::vector<char> mm_t::read(uint64_t addr) {
  addr %= this->size;

  uint8_t *base = this->data + addr;
  return std::vector<char>(base, base + word_size);
}

void mm_t::init(size_t sz, int lsz) {
  assert(word_size > 0 && lsz > 0);
  assert((lsz & (lsz - 1)) == 0 && lsz % word_size == 0);
  line_size = lsz;
  data = (uint8_t *)mmap(nullptr,
                         sz,
                         PROT_READ | PROT_WRITE,
                         MAP_SHARED | MAP_ANONYMOUS | MAP_NORESERVE,
                         -1,
                         0);
  if (data == MAP_FAILED) {
    std::perror("[mm_t] mmap for backing storage failed");
    exit(-1);
  }

  size = sz;
}

mm_t::~mm_t() {
  if (data)
    munmap(data, this->size);
}

void mm_magic_t::init(size_t sz, int lsz) {
  mm_t::init(sz, lsz);
  dummy_data.resize(word_size);
}

namespace {
// Explicit little-endian fields, never C++ object layouts, pointers or padding.
constexpr uint64_t mm_checkpoint_magic = 0x3154504b434d4d46ULL; // FMMCKPT1

void checkpoint_require(bool condition, const char *message) {
  if (!condition)
    throw std::runtime_error(std::string("mm_magic checkpoint: ") + message);
}

void append_u64(std::vector<uint8_t> &out, uint64_t value) {
  for (unsigned i = 0; i < 8; ++i)
    out.push_back(static_cast<uint8_t>(value >> (8 * i)));
}

class checkpoint_reader {
public:
  explicit checkpoint_reader(const std::vector<uint8_t> &bytes) : bytes(bytes) {}
  size_t skip(size_t count) {
    checkpoint_require(count <= bytes.size() - pos, "truncated payload");
    size_t start = pos;
    pos += count;
    return start;
  }
  uint64_t u64() {
    size_t start = skip(8);
    uint64_t value = 0;
    for (unsigned i = 0; i < 8; ++i)
      value |= uint64_t(bytes[start + i]) << (8 * i);
    return value;
  }
  bool boolean() {
    uint64_t value = u64();
    checkpoint_require(value <= 1, "invalid Boolean");
    return value != 0;
  }
  size_t remaining() const { return bytes.size() - pos; }

private:
  const std::vector<uint8_t> &bytes;
  size_t pos = 0;
};

struct memory_checkpoint {
  size_t memory_offset;
  uint64_t cycle, addr, id, size, count;
  bool inflight;
  std::vector<char> dummy;
  std::queue<uint64_t> writes;
  std::queue<mm_rresp_t> reads;
};

memory_checkpoint parse_memory_checkpoint(const mm_t &memory,
                                          const std::vector<uint8_t> &bytes,
                                          size_t size, int word, int line) {
  const auto &conf = memory.get_config();
  checkpoint_require(size > 0 && word >= 4 && word <= 64 &&
                         (word & (word - 1)) == 0 && size % word == 0 &&
                         line > 0 && (line & (line - 1)) == 0 &&
                         line % word == 0 && conf.id_bits <= 64 &&
                         conf.addr_bits > 0 && conf.addr_bits <= 64 &&
                         conf.data_bits == uint64_t(word) * 8,
                     "unsupported/uninitialized memory geometry");
  checkpoint_reader in(bytes);
  checkpoint_require(in.u64() == mm_checkpoint_magic && in.u64() == 1,
                     "wrong schema/version");
  checkpoint_require(in.u64() == conf.id_bits && in.u64() == conf.addr_bits &&
                         in.u64() == conf.data_bits && in.u64() == size &&
                         in.u64() == uint64_t(line),
                     "incompatible AXI/backing-memory geometry");
  memory_checkpoint state;
  state.cycle = in.u64();
  state.inflight = in.boolean();
  state.addr = in.u64();
  state.id = in.u64();
  state.size = in.u64();
  state.count = in.u64();
  auto valid_id = [&conf](uint64_t id) {
    return conf.id_bits == 64 || (id >> conf.id_bits) == 0;
  };
  checkpoint_require(valid_id(state.id), "write ID exceeds AXI width");
  checkpoint_require(state.size == 0 ||
                         (state.size <= uint64_t(word) &&
                          (state.size & (state.size - 1)) == 0),
                     "invalid write beat size");
  checkpoint_require(state.inflight
                         ? state.size != 0 && state.count > 0 && state.count <= 256
                         : state.count == 0,
                     "invalid outstanding write burst");
  state.memory_offset = in.skip(size);
  size_t dummy_offset = in.skip(word);
  state.dummy.assign(bytes.begin() + dummy_offset,
                     bytes.begin() + dummy_offset + word);
  uint64_t writes = in.u64();
  checkpoint_require(writes <= in.remaining() / 8, "invalid write queue count");
  for (uint64_t i = 0; i < writes; ++i) {
    uint64_t id = in.u64();
    checkpoint_require(valid_id(id), "response ID exceeds AXI width");
    state.writes.push(id);
  }
  uint64_t reads = in.u64();
  checkpoint_require(reads <= in.remaining() / (16 + word),
                     "invalid read queue count");
  for (uint64_t i = 0; i < reads; ++i) {
    uint64_t id = in.u64();
    bool last = in.boolean();
    checkpoint_require(valid_id(id), "response ID exceeds AXI width");
    size_t offset = in.skip(word);
    state.reads.emplace(id, std::vector<char>(bytes.begin() + offset,
                                            bytes.begin() + offset + word), last);
  }
  checkpoint_require(state.reads.empty() || state.reads.back().last,
                     "incomplete queued read burst");
  checkpoint_require(in.remaining() == 0, "trailing payload bytes");
  return state;
}
} // namespace

std::vector<uint8_t> mm_magic_t::save_checkpoint() const {
  checkpoint_require(data != nullptr && size > 0 &&
                         dummy_data.size() == size_t(word_size),
                     "uninitialized memory");
  std::vector<uint8_t> out;
  for (uint64_t field : {mm_checkpoint_magic, uint64_t{1}, conf.id_bits,
                         conf.addr_bits, conf.data_bits, uint64_t(size),
                         uint64_t(line_size), cycle, uint64_t(store_inflight),
                         store_addr, store_id, store_size, store_count})
    append_u64(out, field);
  out.insert(out.end(), data, data + size);
  out.insert(out.end(), dummy_data.begin(), dummy_data.end());
  append_u64(out, bresp.size());
  auto writes = bresp;
  while (!writes.empty()) {
    append_u64(out, writes.front());
    writes.pop();
  }
  append_u64(out, rresp.size());
  auto reads = rresp;
  while (!reads.empty()) {
    const auto &response = reads.front();
    checkpoint_require(response.data.size() == size_t(word_size),
                       "invalid queued read data width");
    append_u64(out, response.id);
    append_u64(out, response.last);
    out.insert(out.end(), response.data.begin(), response.data.end());
    reads.pop();
  }
  validate_checkpoint(out);
  return out;
}

void mm_magic_t::validate_checkpoint(const std::vector<uint8_t> &bytes) const {
  checkpoint_require(data != nullptr, "uninitialized memory");
  (void)parse_memory_checkpoint(*this, bytes, size, word_size, line_size);
}

void mm_magic_t::load_checkpoint(const std::vector<uint8_t> &bytes) {
  checkpoint_require(data != nullptr, "uninitialized memory");
  // Parse and allocate everything first: a rejected payload changes nothing.
  auto state = parse_memory_checkpoint(*this, bytes, size, word_size, line_size);
  std::memcpy(data, bytes.data() + state.memory_offset, size);
  dummy_data.swap(state.dummy);
  bresp.swap(state.writes);
  rresp.swap(state.reads);
  cycle = state.cycle;
  store_inflight = state.inflight;
  store_addr = state.addr;
  store_id = state.id;
  store_size = state.size;
  store_count = state.count;
}

void mm_magic_t::tick(bool reset,

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
                      bool b_ready) {
  bool ar_fire = !reset && ar_valid && ar_ready();
  bool aw_fire = !reset && aw_valid && aw_ready();
  bool w_fire = !reset && w_valid && w_ready();
  bool r_fire = !reset && r_valid() && r_ready;
  bool b_fire = !reset && b_valid() && b_ready;

  if (ar_fire) {
    uint64_t start_addr = (ar_addr / word_size) * word_size;
    for (uint64_t i = 0; i <= ar_len; i++) {
      auto dat = read(start_addr + i * word_size);
      rresp.push(mm_rresp_t(ar_id, dat, i == ar_len));
    }
  }

  if (aw_fire) {
    store_addr = aw_addr;
    store_id = aw_id;
    store_count = aw_len + 1;
    store_size = 1 << aw_size;
    store_inflight = true;
  }

  if (w_fire) {
    write(store_addr, (const uint8_t *)w_data.data(), w_strb, store_size);
    store_addr += store_size;
    store_count--;

    if (store_count == 0) {
      store_inflight = false;
      bresp.push(store_id);
      assert(w_last);
    }
  }

  if (b_fire)
    bresp.pop();

  if (r_fire)
    rresp.pop();

  cycle++;

  if (reset) {
    while (!bresp.empty())
      bresp.pop();
    while (!rresp.empty())
      rresp.pop();
    cycle = 0;
  }
}

void mm_t::load_mem(unsigned long start, const char *fname) {
  std::string line;
  std::ifstream in(fname);
  unsigned long fsize = 0;

  if (!in.is_open()) {
    fprintf(stderr, "Couldn't open loadmem file %s\n", fname);
    abort();
  }

  while (std::getline(in, line)) {
#define parse_nibble(c) ((c) >= 'a' ? (c) - 'a' + 10 : (c) - '0')
    for (ssize_t i = line.length() - 2, j = 0; i >= 0; i -= 2, j++) {
      char byte = (parse_nibble(line[i]) << 4) | parse_nibble(line[i + 1]);
      ssize_t addr = (start + j) % size;
      data[addr] = byte;
    }
    start += line.length() / 2;
    fsize += line.length() / 2;

    if (fsize > this->size) {
      fprintf(stderr, "Loadmem file is too large\n");
      abort();
    }
  }
}
