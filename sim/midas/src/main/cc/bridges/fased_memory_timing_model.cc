// See LICENSE for license details.

#include <algorithm>
#include <exception>
#include <iostream>
#include <stdio.h>
#include <sstream>

#include "fased_memory_timing_model.h"

void Histogram::init() {
  // Read out the initial values
  write(enable, 1);
  for (size_t i = 0; i < HISTOGRAM_SIZE; i++) {
    write(addr, i);
    latency[i] = read64(dataH, dataL, BIN_H_MASK);
  }
  // Disable readout enable; otherwise histogram updates will be gated
  write(enable, 0);
}

void Histogram::finish() {
  // Read out the initial values
  write(enable, 1);
  for (size_t i = 0; i < HISTOGRAM_SIZE; i++) {
    write(addr, i);
    latency[i] = read64(dataH, dataL, BIN_H_MASK) - latency[i];
  }
  // Disable readout enable; otherwise histogram updates will be gated
  write(enable, 0);
}

void AddrRangeCounter::init() {
  nranges = read("numRanges");
  range_bytes.resize(nranges);

  write(enable, 1);
  for (size_t i = 0; i < nranges; i++) {
    write(addr, i);
    range_bytes[i] = read64(dataH, dataL, RANGE_H_MASK);
  }
  write(enable, 0);
}

void AddrRangeCounter::finish() {
  write(enable, 1);
  for (size_t i = 0; i < nranges; i++) {
    write(addr, i);
    range_bytes[i] = read64(dataH, dataL, RANGE_H_MASK);
  }
  write(enable, 0);
}

char FASEDMemoryTimingModel::KIND;

FASEDMemoryTimingModel::FASEDMemoryTimingModel(
    simif_t &simif,
    AddressMap &&addr_map,
    unsigned modelno,
    const std::vector<std::string> &args,
    const std::string &stats_file_name,
    size_t mem_size)
    : bridge_driver_t(simif, &KIND), addr_map(std::move(addr_map)),
      model_number(modelno), stats_path(stats_file_name),
      mem_size(mem_size) {

  std::string suffix = "_" + std::to_string(modelno);
  for (auto &arg : args) {
    // Find arguments: +mm_{name}{index}={value}
    if (arg.find("+mm_") != 0 || arg.find(suffix) == std::string::npos) {
      continue;
    }

    auto sub_arg = arg.substr(4);
    size_t delimit_idx = sub_arg.find("=");
    size_t suffix_idx = sub_arg.find(suffix);
    std::string key = sub_arg.substr(0, suffix_idx).c_str();

    // This is the only nullary plusarg supported by fased
    if (key == std::string("useHardwareDefaultRuntimeSettings")) {
      useHardwareDefaults = true;
      continue;
    }

    // All other plusargs are key-value pairs that will be written to the
    // bridge module
    if (suffix_idx == std::string::npos) {
      throw std::runtime_error("[FASED] unknown nullary plusarg: " + key);
    }
    if (!addr_map.w_reg_exists(key)) {
      throw std::runtime_error("[FASED] unknown register: " + key);
    }
    int value = std::stoi(sub_arg.substr(delimit_idx + 1).c_str());
    user_configuration[key] = value;
  }

  stats_file.open(stats_file_name, std::ofstream::out);
  if (!stats_file.is_open()) {
    throw std::runtime_error("Could not open output file: " + stats_file_name);
  }

  for (const auto &pair : addr_map.r_registers) {
    // Only profile readable registers
    if (!addr_map.w_reg_exists((pair.first))) {
      // Iterate through substrings to exclude
      bool exclude = false;
      for (auto &substr : profile_exclusion) {
        if (pair.first.find(substr) != std::string::npos) {
          exclude = true;
        }
      }
      if (!exclude) {
        profile_reg_addrs.push_back(pair.second);
        stats_header += pair.first + ",";
      }
    }
  }
  stats_header += '\n';
  append_stats(stats_header);

  if (addr_map.w_reg_exists("hostReadLatencyHist_enable")) {
    histograms.emplace_back(&simif, this->addr_map, "hostReadLatency");
    histograms.emplace_back(&simif, this->addr_map, "hostWriteLatency");
    histograms.emplace_back(&simif, this->addr_map, "targetReadLatency");
    histograms.emplace_back(&simif, this->addr_map, "targetWriteLatency");
    histograms.emplace_back(&simif, this->addr_map, "ingressReadLatency");
    histograms.emplace_back(&simif, this->addr_map, "ingressWriteLatency");
    histograms.emplace_back(&simif, this->addr_map, "totalReadLatency");
    histograms.emplace_back(&simif, this->addr_map, "totalWriteLatency");
  }

  if (addr_map.w_reg_exists("readRanges_enable")) {
    rangectrs.emplace_back(&simif, this->addr_map, "read");
    rangectrs.emplace_back(&simif, this->addr_map, "write");
  }
}

void FASEDMemoryTimingModel::profile() {
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  if (lifecycle != 1)
    throw std::runtime_error("FASED profile outside initialized lifecycle");
  replay_stats();
  std::ostringstream row;
  for (auto addr : profile_reg_addrs) {
    row << read(addr) << ",";
  }
  row << '\n';
  append_stats(row.str());
}

void FASEDMemoryTimingModel::init() {
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  if (lifecycle != 0)
    throw std::runtime_error("FASED repeated initialization");
  for (auto &[key, addr] : addr_map.w_registers) {
    // If the user provided a configuration option, use it and
    // overwrite whatever the hardware register was initialized to.
    auto user_it = user_configuration.find(key);
    if (user_it != user_configuration.end()) {
      write(addr, user_it->second);
      continue;
    }

    // If the registers is excluded, emit a warning only when some, but not
    // all, registers have been specified through arguments.
    bool exclude = false;
    for (auto &substr : configuration_exclusion) {
      if (key.find(substr) != std::string::npos) {
        exclude = true;
      }
    }
    if (exclude && !user_configuration.empty()) {
      fprintf(stderr, "[FASED] Ignoring writeable register: %s\n", key.c_str());
      continue;
    }

    // If the register is not excluded and some other registers were
    // given initial values, emit a diagnostic message. If hardware
    // defaults are allowed to be mixed in with the configuration, produce
    // the initial value. Otherwise, error out.
    if (!user_configuration.empty()) {
      if (useHardwareDefaults) {
        auto init_val = simif.read(addr_map.r_addr(key));
        fprintf(stderr,
                "[FASED] Using hardware default of %u for configuration "
                "register %s\n",
                init_val,
                key.c_str());
      } else {
        char buf[100];
        sprintf(buf,
                "[FASED] No value provided for configuration register: %s",
                key.c_str());
        throw std::runtime_error(buf);
      }
    }
  }

  for (auto &hist : histograms) {
    hist.init();
  }
  for (auto &rctr : rangectrs) {
    rctr.init();
  }
  lifecycle = 1;
}

void FASEDMemoryTimingModel::finish() {
  widget_checkpoint_state_t::call_t call(checkpoint_state);
  if (lifecycle != 1)
    throw std::runtime_error("FASED finish outside initialized lifecycle");
  replay_stats();
  for (auto &hist : histograms) {
    hist.finish();
  }
  for (auto &rctr : rangectrs) {
    rctr.finish();
  }

  std::ofstream histogram_file;
  histogram_file.open("latency_histogram.csv", std::ofstream::out);
  if (!histogram_file.is_open()) {
    throw std::runtime_error("Could not open histogram output file");
  }

  // Header
  for (auto &hist : histograms) {
    histogram_file << hist.name << ",";
  }
  histogram_file << std::endl;
  // Data
  for (size_t i = 0; i < HISTOGRAM_SIZE; i++) {
    for (auto &hist : histograms) {
      histogram_file << hist.latency[i] << ",";
    }
    histogram_file << std::endl;
  }
  histogram_file.close();

  if (!rangectrs.empty()) {
    size_t nranges = rangectrs[0].nranges;
    for (const auto &rctr : rangectrs)
      if (rctr.nranges != nranges)
        throw std::runtime_error("FASED inconsistent range-counter geometry");
    std::ofstream rangectr_file;

    rangectr_file.open("range_counters.csv", std::ofstream::out);
    if (!rangectr_file.is_open()) {
      throw std::runtime_error("Could not open range counter file");
    }

    rangectr_file << "Address,";
    for (auto &rctr : rangectrs) {
      rangectr_file << rctr.name << ",";
    }
    rangectr_file << std::endl;

    for (size_t i = 0; i < nranges; i++) {
      rangectr_file << std::hex << (i * mem_size / nranges) << ",";
      for (auto &rctr : rangectrs) {
        rangectr_file << std::dec << rctr.range_bytes[i] << ",";
      }
      rangectr_file << std::endl;
    }
    rangectr_file.close();
  }

  stats_file.close();
  lifecycle = 2;
}

void FASEDMemoryTimingModel::append_stats(const std::string &row) {
  stats_file << row << std::flush;
  if (!stats_file)
    throw std::runtime_error("FASED profiling output failed");
  stats_history += row;
}

void FASEDMemoryTimingModel::replay_stats() {
  if (!stats_replay_pending)
    return;
  stats_file.close();
  stats_file.clear();
  stats_file.open(stats_path, std::ofstream::out);
  stats_file << stats_history << std::flush;
  if (!stats_file)
    throw std::runtime_error("FASED restored profiling output failed");
  stats_replay_pending = false;
}

widget_checkpoint_config_t FASEDMemoryTimingModel::checkpoint_config() const {
  widget_checkpoint_config_t config(6, {model_number, mem_size, useHardwareDefaults});
  for (const auto *map : {&addr_map.r_registers, &addr_map.w_registers}) {
    config.word(map->size());
    for (const auto &[name, address] : *map) {
      config.text(name);
      config.word(address);
    }
  }
  const std::map<std::string, uint32_t> sorted(user_configuration.begin(),
                                               user_configuration.end());
  config.word(sorted.size());
  for (const auto &[name, value] : sorted) {
    config.text(name);
    config.word(value);
  }
  config.word(histograms.size());
  for (const auto &hist : histograms)
    config.text(hist.name);
  config.word(rangectrs.size());
  for (const auto &range : rangectrs)
    config.text(range.name);
  return config;
}

void FASEDMemoryTimingModel::require_checkpointable() const {
  checkpoint_state.require_idle();
  // finish() has external file publication effects. Terminal snapshots need
  // aggregate output ownership, which this completed-call adapter does not
  // yet provide; never silently advertise them as resumable.
  if (lifecycle == 2)
    throw std::runtime_error("unsupported finished FASED output lifecycle");
}

std::vector<uint8_t> FASEDMemoryTimingModel::save_checkpoint() const {
  require_checkpointable();
  auto data = checkpoint_config();
  data.word(lifecycle);
  data.text(stats_history);
  for (const auto &hist : histograms)
    for (auto value : hist.latency)
      data.word(value);
  for (const auto &range : rangectrs) {
    if (range.nranges != range.range_bytes.size())
      throw std::runtime_error("FASED invalid range-counter buffer");
    data.word(range.nranges);
    for (auto value : range.range_bytes)
      data.word(value);
  }
  return std::move(data.bytes);
}

FASEDMemoryTimingModel::decoded_checkpoint_t
FASEDMemoryTimingModel::decode_checkpoint(const std::vector<uint8_t> &data) const {
  require_checkpointable();
  const auto config = checkpoint_config();
  if (data.size() < config.bytes.size() ||
      !std::equal(config.bytes.begin(), config.bytes.end(), data.begin()))
    throw std::runtime_error("FASED checkpoint configuration mismatch");
  size_t offset = config.bytes.size();
  auto word = [&]() {
    if (data.size() - offset < 8)
      throw std::runtime_error("FASED truncated checkpoint");
    uint64_t value = 0;
    for (unsigned i = 0; i < 8; ++i)
      value |= uint64_t(data[offset++]) << (8 * i);
    return value;
  };
  decoded_checkpoint_t result;
  result.lifecycle = word();
  if (result.lifecycle > 1)
    throw std::runtime_error("FASED unsupported checkpoint lifecycle");
  const auto length = word();
  if (length > data.size() - offset)
    throw std::runtime_error("FASED truncated profiling history");
  result.history.assign(reinterpret_cast<const char *>(data.data() + offset), length);
  offset += length;
  if (result.history.compare(0, stats_header.size(), stats_header) != 0 ||
      (result.lifecycle == 0 && result.history != stats_header))
    throw std::runtime_error("FASED invalid profiling header/lifecycle");
  // Require the original CSV format: complete rows of uint32 MMIO values.
  size_t pos = stats_header.size();
  while (pos < result.history.size()) {
    for (size_t column = 0; column < profile_reg_addrs.size(); ++column) {
      uint64_t value = 0;
      const size_t start = pos;
      while (pos < result.history.size() && result.history[pos] >= '0' &&
             result.history[pos] <= '9') {
        value = value * 10 + (result.history[pos++] - '0');
        if (value > UINT32_MAX)
          throw std::runtime_error("FASED invalid profiling value");
      }
      if (pos == start || pos == result.history.size() || result.history[pos++] != ',')
        throw std::runtime_error("FASED incomplete profiling row");
    }
    if (pos == result.history.size() || result.history[pos++] != '\n')
      throw std::runtime_error("FASED incomplete profiling row");
  }
  for (size_t i = 0; i < histograms.size(); ++i) {
    std::vector<uint64_t> values;
    values.reserve(HISTOGRAM_SIZE);
    for (size_t bin = 0; bin < HISTOGRAM_SIZE; ++bin) {
      const auto value = word();
      if (value >= (uint64_t(1) << BIN_SIZE) || (!result.lifecycle && value))
        throw std::runtime_error("FASED invalid histogram baseline");
      values.push_back(value);
    }
    result.histograms.push_back(std::move(values));
  }
  for (size_t i = 0; i < rangectrs.size(); ++i) {
    const auto count = word();
    if (count > UINT32_MAX || count > (data.size() - offset) / 8 ||
        (!result.lifecycle && count) || (i && count != result.ranges[0].size()))
      throw std::runtime_error("FASED invalid range-counter geometry");
    std::vector<uint64_t> values;
    values.reserve(count);
    for (size_t bin = 0; bin < count; ++bin) {
      const auto value = word();
      if (value >= (uint64_t(1) << RANGE_COUNT_SIZE))
        throw std::runtime_error("FASED invalid range-counter value");
      values.push_back(value);
    }
    result.ranges.push_back(std::move(values));
  }
  if (offset != data.size())
    throw std::runtime_error("FASED trailing checkpoint bytes");
  return result;
}

void FASEDMemoryTimingModel::validate_checkpoint(const std::vector<uint8_t> &data) const {
  (void)decode_checkpoint(data);
}

void FASEDMemoryTimingModel::load_checkpoint(const std::vector<uint8_t> &data) {
  // All validation/allocation precedes mutation. No MMIO, file access or
  // target advancement occurs here, including on a malformed payload.
  auto decoded = decode_checkpoint(data);
  for (size_t i = 0; i < histograms.size(); ++i)
    std::copy(decoded.histograms[i].begin(), decoded.histograms[i].end(), histograms[i].latency);
  for (size_t i = 0; i < rangectrs.size(); ++i) {
    rangectrs[i].range_bytes.swap(decoded.ranges[i]);
    rangectrs[i].nranges = rangectrs[i].range_bytes.size();
  }
  stats_history.swap(decoded.history);
  lifecycle = decoded.lifecycle;
  stats_replay_pending = true;
}
