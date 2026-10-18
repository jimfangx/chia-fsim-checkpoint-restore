// See LICENSE for license details.
#ifndef __CHECKPOINT_PUBLICATION_H
#define __CHECKPOINT_PUBLICATION_H

#include <cstdint>
#include <exception>
#include <memory>
#include <stdexcept>
#include <thread>

// Lifetime/generation guard for explicitly audited native publication adapters.
// The owner cannot move or copy. Candidates keep only a weak lifetime handle,
// so a recycled destination address cannot revive an obsolete candidate.
// This guard does not acquire exclusion or detect arbitrary writes after seal.
class checkpoint_publication_t {
  struct state_t { uint64_t generation = 0; };
  std::shared_ptr<state_t> state = std::make_shared<state_t>();
public:
  checkpoint_publication_t() = default;
  checkpoint_publication_t(const checkpoint_publication_t &) = delete;
  checkpoint_publication_t &operator=(const checkpoint_publication_t &) = delete;

  class guard_t {
    std::weak_ptr<state_t> lifetime;
    const uint64_t generation;
    const std::thread::id creator = std::this_thread::get_id();
    bool sealed = false, published = false;
    bool matches(const std::shared_ptr<state_t> &live) const noexcept {
      return live && creator == std::this_thread::get_id() && !published &&
             live->generation == generation && generation != UINT64_MAX;
    }
  public:
    explicit guard_t(const checkpoint_publication_t &owner)
        : lifetime(owner.state), generation(owner.state->generation) {}
    guard_t(const guard_t &) = delete;
    guard_t &operator=(const guard_t &) = delete;
    void require_unsealed() const {
      if (sealed || !matches(lifetime.lock()))
        throw std::runtime_error("expired/thread/consumed/sealed/stale checkpoint candidate");
    }
    void seal() {
      require_unsealed();
      sealed = true;
    }
    void publish() noexcept {
      auto live = lifetime.lock();
      if (!sealed || !matches(live))
        std::terminate();
      ++live->generation;
      published = true;
    }
  };

  // Invoke after parsing and before any legacy assignment, even for identical
  // bytes. No throwing work may follow this call in an audited legacy load.
  void changed() {
    if (state->generation == UINT64_MAX)
      throw std::runtime_error("checkpoint publication generation exhausted");
    ++state->generation;
  }
};
#endif
