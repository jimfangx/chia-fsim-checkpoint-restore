#include "mmio.h"
#include <cassert>
#include <limits>

namespace {
constexpr uint64_t checkpoint_magic = 0x3154504b434f494dULL; // MIOCKPT1

void require(bool condition, const char *message) {
  if (!condition)
    throw std::runtime_error(std::string("mmio checkpoint: ") + message);
}

bool fits(uint64_t value, uint64_t bits) {
  return bits == 64 || (bits < 64 && (value >> bits) == 0);
}

void check_geometry(const AXI4Config &conf) {
  const auto beat = conf.beat_bytes();
  require(sizeof(size_t) == 8 && conf.id_bits <= 64 && conf.addr_bits > 0 &&
              conf.addr_bits <= 64 && beat >= 4 && beat <= 64 &&
              (beat & (beat - 1)) == 0 && conf.data_bits == beat * 8,
          "unsupported AXI geometry");
}

void check_request(const AXI4Config &conf, uint64_t addr, size_t size, size_t len) {
  check_geometry(conf);
  require(fits(addr, conf.addr_bits) && size <= 6 &&
              (uint64_t{1} << size) <= conf.beat_bytes() && len <= 255,
          "invalid AXI request geometry");
}

void append(std::vector<uint8_t> &out, uint64_t value) {
  for (unsigned i = 0; i < 8; ++i)
    out.push_back(static_cast<uint8_t>(value >> (8 * i)));
}

class reader {
public:
  explicit reader(const std::vector<uint8_t> &bytes) : bytes(bytes) {}
  uint64_t u64() {
    require(remaining() >= 8, "truncated payload");
    uint64_t value = 0;
    for (unsigned i = 0; i < 8; ++i)
      value |= uint64_t(bytes[pos++]) << (8 * i);
    return value;
  }
  bool boolean() {
    const auto value = u64();
    require(value <= 1, "invalid Boolean");
    return value != 0;
  }
  size_t count(size_t record_bytes) {
    const auto value = u64();
    require(value <= remaining() / record_bytes, "truncated/oversized queue");
    return value;
  }
  std::vector<char> data(size_t size) {
    require(size <= remaining(), "truncated beat");
    std::vector<char> value(bytes.begin() + pos, bytes.begin() + pos + size);
    pos += size;
    return value;
  }
  size_t remaining() const { return bytes.size() - pos; }
private:
  const std::vector<uint8_t> &bytes;
  size_t pos = 0;
};

struct checkpoint_state {
  bool read_inflight, write_inflight;
  std::vector<char> dummy;
  std::queue<mmio_req_addr_t> ar, aw;
  std::queue<mmio_req_data_t> w;
  std::queue<mmio_resp_data_t> r;
  std::queue<size_t> b;
};

checkpoint_state parse(const AXI4Config &conf, const std::vector<uint8_t> &bytes) {
  check_geometry(conf);
  reader in(bytes);
  require(in.u64() == checkpoint_magic && in.u64() == 1, "wrong schema/version");
  require(in.u64() == conf.id_bits && in.u64() == conf.addr_bits &&
              in.u64() == conf.data_bits, "incompatible AXI geometry");
  checkpoint_state state;
  state.read_inflight = in.boolean();
  state.write_inflight = in.boolean();
  state.dummy = in.data(conf.beat_bytes());
  for (auto *queue : {&state.ar, &state.aw}) {
    const size_t count = in.count(32);
    for (size_t i = 0; i < count; ++i) {
      const auto id = in.u64(), addr = in.u64(), size = in.u64(), len = in.u64();
      check_request(conf, addr, size, len);
      require(id == 0, "unsupported request ID"); // read_req/write_req issue ID 0
      queue->emplace(id, addr, size, len);
    }
  }
  const auto writes = in.count(16 + conf.beat_bytes());
  for (size_t i = 0; i < writes; ++i) {
    const auto strb = in.u64();
    const bool last = in.boolean();
    require(fits(strb, conf.strb_bits()), "strobe exceeds AXI width");
    state.w.emplace(in.data(conf.beat_bytes()), strb, last);
  }
  const auto reads = in.count(16 + conf.beat_bytes());
  for (size_t i = 0; i < reads; ++i) {
    const auto id = in.u64();
    const bool last = in.boolean();
    require(fits(id, conf.id_bits), "response ID exceeds AXI width");
    state.r.emplace(id, in.data(conf.beat_bytes()), last);
  }
  const auto responses = in.count(8);
  for (size_t i = 0; i < responses; ++i) {
    const auto id = in.u64();
    require(fits(id, conf.id_bits), "response ID exceeds AXI width");
    state.b.push(id);
  }
  require(in.remaining() == 0, "trailing bytes");
  require(!state.read_inflight || !state.ar.empty(), "read without request");
  require(!state.write_inflight || !state.aw.empty(), "write without request");
  require(state.r.empty() || (state.read_inflight &&
              state.r.size() <= state.ar.front().len + 1), "invalid read response count");
  auto r = state.r;
  for (size_t i = 0; !r.empty(); ++i, r.pop())
    require(r.front().id == state.ar.front().id &&
                r.front().last == (i == state.ar.front().len), "invalid read response order");
  require(state.b.empty() || (state.write_inflight && state.b.size() == 1 &&
              state.b.front() == state.aw.front().id), "invalid write response");

  size_t total = 0;
  auto aw = state.aw;
  while (!aw.empty()) {
    require(total <= std::numeric_limits<size_t>::max() - aw.front().len - 1,
            "write queue length overflow");
    total += aw.front().len + 1;
    aw.pop();
  }
  // AXI AW and W are independent: an ingress buffer may accept W before AW
  // or buffer later bursts while the first B response awaits driver service.
  // The retained W queue must be a suffix of all uncompleted request beats.
  require(state.w.size() <= total, "write data/request count mismatch");
  const size_t consumed = total - state.w.size();
  require(state.b.empty() || consumed >= state.aw.front().len + 1,
          "response before write data completion");
  auto w = state.w;
  aw = state.aw;
  size_t index = 0;
  while (!aw.empty()) {
    for (size_t i = 0; i <= aw.front().len; ++i, ++index) {
      if (index < consumed)
        continue;
      require(!w.empty() && w.front().last == (i == aw.front().len),
              "invalid write beat order");
      w.pop();
    }
    aw.pop();
  }
  return state;
}
} // namespace

std::vector<uint8_t> mmio_t::save_checkpoint() const {
  std::vector<uint8_t> out;
  for (auto value : {checkpoint_magic, uint64_t{1}, conf.id_bits, conf.addr_bits,
                     conf.data_bits, uint64_t(read_inflight), uint64_t(write_inflight)})
    append(out, value);
  out.insert(out.end(), dummy_data.begin(), dummy_data.end());
  for (auto queue : {ar, aw}) {
    append(out, queue.size());
    while (!queue.empty()) {
      const auto &req = queue.front();
      for (auto value : {uint64_t(req.id), req.addr, uint64_t(req.size), uint64_t(req.len)})
        append(out, value);
      queue.pop();
    }
  }
  append(out, w.size());
  auto writes = w;
  while (!writes.empty()) {
    const auto &beat = writes.front();
    append(out, beat.strb);
    append(out, beat.last);
    require(beat.data.size() == conf.beat_bytes(), "wrong write beat width");
    out.insert(out.end(), beat.data.begin(), beat.data.end());
    writes.pop();
  }
  append(out, r.size());
  auto reads = r;
  while (!reads.empty()) {
    const auto &beat = reads.front();
    append(out, beat.id);
    append(out, beat.last);
    require(beat.data.size() == conf.beat_bytes(), "wrong read beat width");
    out.insert(out.end(), beat.data.begin(), beat.data.end());
    reads.pop();
  }
  append(out, b.size());
  auto responses = b;
  while (!responses.empty()) {
    append(out, responses.front());
    responses.pop();
  }
  validate_checkpoint(out);
  return out;
}

void mmio_t::validate_checkpoint(const std::vector<uint8_t> &bytes) const {
  (void)parse(conf, bytes);
}

void mmio_t::load_checkpoint(const std::vector<uint8_t> &bytes) {
  // All parsing/allocation precedes mutation; malformed payloads leave this
  // owner unchanged. The coordinator must prevalidate every other owner, too.
  auto state = parse(conf, bytes);
  ar.swap(state.ar);
  aw.swap(state.aw);
  w.swap(state.w);
  r.swap(state.r);
  b.swap(state.b);
  dummy_data.swap(state.dummy);
  read_inflight = state.read_inflight;
  write_inflight = state.write_inflight;
}

void mmio_t::swap_checkpoint_state(mmio_t &other) noexcept {
  static_assert(noexcept(ar.swap(other.ar)) && noexcept(aw.swap(other.aw)) &&
                noexcept(w.swap(other.w)) && noexcept(r.swap(other.r)) &&
                noexcept(b.swap(other.b)) && noexcept(dummy_data.swap(other.dummy_data)));
  ar.swap(other.ar);
  aw.swap(other.aw);
  w.swap(other.w);
  r.swap(other.r);
  b.swap(other.b);
  dummy_data.swap(other.dummy_data);
  std::swap(read_inflight, other.read_inflight);
  std::swap(write_inflight, other.write_inflight);
}

void mmio_t::read_req(uint64_t addr, size_t size, size_t len) {
  check_request(conf, addr, size, len);
  mmio_req_addr_t ar(0, addr, size, len);
  this->ar.push(ar);
}

void mmio_t::write_req(
    uint64_t addr, size_t size, size_t len, const void *data, size_t *strb) {
  check_request(conf, addr, size, len);
  require(data != nullptr && strb != nullptr, "null write request buffer");
  const size_t nbytes = size_t{1} << size;

  mmio_req_addr_t aw(0, addr, size, len);
  this->aw.push(aw);

  for (size_t i = 0; i < len + 1; i++) {
    // Own the bus payload, rather than retaining a driver stack/buffer pointer.
    // Narrow requests retain their existing low-byte layout with zero padding.
    std::vector<char> beat(conf.beat_bytes(), 0);
    memcpy(beat.data(), static_cast<const char *>(data) + i * nbytes, nbytes);
    const size_t mask = conf.strb_bits() == 64 ? ~size_t{0} :
                           (size_t{1} << conf.strb_bits()) - 1;
    mmio_req_data_t w(std::move(beat), strb[i] & mask, i == len);
    this->w.push(w);
  }
}

void mmio_t::tick(bool reset,
                  bool ar_ready,
                  bool aw_ready,
                  bool w_ready,
                  size_t r_id,
                  const std::vector<uint32_t> &r_data,
                  bool r_last,
                  bool r_valid,
                  size_t b_id,
                  bool b_valid) {
  const bool ar_fire = !reset && ar_ready && ar_valid();
  const bool aw_fire = !reset && aw_ready && aw_valid();
  const bool w_fire = !reset && w_ready && w_valid();
  const bool r_fire = !reset && r_valid && r_ready();
  const bool b_fire = !reset && b_valid && b_ready();

  if (ar_fire)
    read_inflight = true;
  if (aw_fire)
    write_inflight = true;
  if (w_fire)
    this->w.pop();
  if (r_fire) {
    require(r_data.size() * sizeof(uint32_t) == dummy_data.size(), "wrong AXI read data width");
    std::vector<char> dat(dummy_data.size());
    memcpy(dat.data(), r_data.data(), dummy_data.size());
    mmio_resp_data_t r(r_id, std::move(dat), r_last);
    this->r.push(r);
  }
  if (b_fire) {
    this->b.push(b_id);
  }
}

bool mmio_t::read_resp(void *data) {
  if (ar.empty() || r.size() <= ar.front().len) {
    return false;
  } else {
    auto ar = this->ar.front();
    size_t word_size = 1 << ar.size;
    for (size_t i = 0; i <= ar.len; i++) {
      const auto &r = this->r.front();
      assert(i < ar.len || r.last);
      memcpy(((char *)data) + i * word_size, r.data.data(), word_size);
      this->r.pop();
    }
    this->ar.pop();
    read_inflight = false;
    return true;
  }
}

bool mmio_t::write_resp() {
  if (aw.empty() || b.empty()) {
    return false;
  } else {
    aw.pop();
    b.pop();
    write_inflight = false;
    return true;
  }
}
