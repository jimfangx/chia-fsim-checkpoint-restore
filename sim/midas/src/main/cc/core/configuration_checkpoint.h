// See LICENSE for license details.
#ifndef __CONFIGURATION_CHECKPOINT_H
#define __CONFIGURATION_CHECKPOINT_H

#include "core/checkpoint.h"
#include "core/checkpoint_publication.h"

// Explicitly opted-in configuration-only host owners. Generated RTL, MMIO,
// memory and suspended calls remain separately owned. This adapter is unsuitable
// for owners with mutable result flags or any other restorable host state.
// Validation must require that the source configuration equals the destination.
class configuration_checkpoint_t final : public prepared_checkpoint_t {
  checkpoint_owner_t &owner;
  checkpoint_publication_t::guard_t guard;
  const std::vector<uint8_t> expected;
public:
  configuration_checkpoint_t(checkpoint_owner_t &owner,
                             checkpoint_publication_t &publication,
                             const std::vector<uint8_t> &payload)
      : owner(owner), guard(publication), expected(owner.save_checkpoint()) {
    owner.validate_checkpoint(payload);
    if (payload != expected)
      throw std::runtime_error("configuration-only checkpoint contains mutable state");
  }
  void seal() override {
    // Check lifetime/thread/generation before dereferencing the owner.
    guard.require_unsealed();
    if (owner.save_checkpoint() != expected)
      throw std::runtime_error("configuration checkpoint destination changed");
    guard.seal();
  }
  void publish() noexcept override {
    // No parsing, allocation, MMIO or host-state assignment is necessary:
    // validation established identical immutable configuration in both owners.
    // The coordinator must retain exclusion through complete cut readback.
    guard.publish();
  }
};

#endif
