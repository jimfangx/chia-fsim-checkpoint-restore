// See LICENSE for license details.
#include "driver_boundary_diagnostic.h"
#include <charconv>
#include <filesystem>
#include <fstream>
#include <limits>
#include <unordered_set>

namespace {
void require(bool ok, const char *message) {
  if (!ok)
    throw std::runtime_error(std::string("driver boundary diagnostic: ") +
                             message);
}
void append(std::ostream &out, uint64_t value) {
  for (unsigned i = 0; i < 8; ++i)
    out.put(static_cast<char>(value >> (8 * i)));
}
void append(std::ostream &out, const std::string &value) {
  append(out, value.size());
  out.write(value.data(), value.size());
}
struct record_t {
  std::string id, schema;
  checkpoint_owner_t *owner;
  std::vector<uint8_t> payload;
};
} // namespace

driver_boundary_diagnostic_t::driver_boundary_diagnostic_t(
    const std::vector<std::string> &args) {
  bool have_cut = false, have_path = false;
  for (const auto &arg : args) {
    if (arg.rfind("+fs-driver-cut=", 0) == 0) {
      require(!have_cut, "duplicate cut");
      have_cut = true;
      const auto value = arg.substr(15);
      const auto result =
          std::from_chars(value.data(), value.data() + value.size(), cut);
      require(result.ec == std::errc() &&
                  result.ptr == value.data() + value.size() && cut > 0,
              "invalid completed-operation cut");
    }
    if (arg.rfind("+fs-driver-diagnostic=", 0) == 0) {
      require(!have_path, "duplicate diagnostic path");
      have_path = true;
      path = arg.substr(22);
      require(!path.empty(), "empty diagnostic path");
    }
  }
  require(have_cut == have_path,
          "cut and diagnostic path must be supplied together");
}

void driver_boundary_diagnostic_t::observe(const owners_t &owners) {
  require(!active && !failed, "reentrant or failed diagnostic");
  require(completed < std::numeric_limits<uint64_t>::max(),
          "operation counter overflow");
  if (++completed != cut || !requested())
    return;
  active = true;
  const auto temporary = path + ".tmp";
  bool created = false;
  try {
    require(!owners.empty(), "empty owner inventory");
    std::unordered_set<std::string> ids;
    std::unordered_set<checkpoint_owner_t *> pointers;
    std::vector<record_t> records;
    for (const auto &[id, owner] : owners) {
      require(!id.empty() && ids.insert(id).second,
              "empty/duplicate owner identity");
      require(!owner || pointers.insert(owner).second, "aliased owner");
      record_t record{id, owner ? owner->checkpoint_schema() : "", owner, {}};
      if (!record.schema.empty()) {
        record.payload = owner->save_checkpoint();
        owner->validate_checkpoint(record.payload);
      }
      records.push_back(std::move(record));
    }
    // Validate all owners before opening a file; check stability again after
    // all save hooks, including cross-owner mutation caused by a bad adapter.
    for (const auto &record : records) {
      require(!record.owner ||
                  record.owner->checkpoint_schema() == record.schema,
              "owner schema changed during capture");
      if (!record.schema.empty())
        require(record.owner->save_checkpoint() == record.payload,
                "owner changed during parked capture");
    }
    require(!std::filesystem::exists(path) &&
                !std::filesystem::exists(temporary),
            "diagnostic destination already exists");
    std::ofstream output(temporary, std::ios::binary);
    created = true;
    require(bool(output), "cannot open diagnostic temporary file");
    output.write("FSDIAG01", 8);
    append(output, completed);
    append(output, records.size());
    for (const auto &record : records) {
      append(output, record.id);
      append(output, record.schema);
      append(output, record.payload.size());
      if (!record.payload.empty())
        output.write(reinterpret_cast<const char *>(record.payload.data()),
                     record.payload.size());
    }
    output.close();
    require(bool(output), "diagnostic write failed");
    std::filesystem::rename(temporary, path);
    captured = true;
    active = false;
  } catch (...) {
    if (created) {
      std::error_code error;
      std::filesystem::remove(temporary, error);
    }
    active = false;
    failed = true;
    throw;
  }
}

void driver_boundary_diagnostic_t::finish() const {
  require(!active && !failed, "active or failed diagnostic");
  require(!requested() || captured,
          "requested operation cut was never reached");
}
