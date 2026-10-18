// See LICENSE for license details.
#ifndef __CHECKPOINT_OWNER_H
#define __CHECKPOINT_OWNER_H

#include <cstdint>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

// Preparation owns all replacement storage. seal() finishes every throwing
// destination check without publishing; publish() requires external exclusion
// through complete aggregate readback. Destruction, including after a seal,
// cancels without publication. This is a native component contract, not CPU
// image admission, process exclusion or a durable image format.
class prepared_checkpoint_t {
public:
  virtual ~prepared_checkpoint_t() = default;
  virtual void seal() = 0;
  virtual void publish() noexcept = 0;
};

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
  // Never implement this by deferring a legacy allocating load to publish().
  // Even owners with an audited legacy schema must explicitly opt in.
  virtual std::unique_ptr<prepared_checkpoint_t>
  prepare_checkpoint_publication(const std::vector<uint8_t> &) {
    throw std::runtime_error("unsupported prepared host checkpoint publication");
  }
};

#endif
