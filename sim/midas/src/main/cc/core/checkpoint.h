// See LICENSE for license details.
#ifndef __CHECKPOINT_OWNER_H
#define __CHECKPOINT_OWNER_H

#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>

// Level-1 software state is separate from generated RTL. Empty schema means
// unsupported, including apparently passive owners: statelessness requires an
// explicit audit/adapter. Hooks throw by default so a schema alone cannot omit
// state. A restore coordinator must validate every owner before loading any.
class checkpoint_owner_t {
public:
  virtual ~checkpoint_owner_t() = default;
  virtual std::string checkpoint_schema() const { return {}; }
  virtual std::vector<uint8_t> save_checkpoint() const {
    throw std::runtime_error("unsupported host checkpoint save");
  }
  virtual void validate_checkpoint(const std::vector<uint8_t> &) const {
    throw std::runtime_error("unsupported host checkpoint validation");
  }
  virtual void load_checkpoint(const std::vector<uint8_t> &) {
    throw std::runtime_error("unsupported host checkpoint load");
  }
};

#endif
