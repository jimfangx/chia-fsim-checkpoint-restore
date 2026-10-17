// See LICENSE for license details.
#ifndef __EMUL_DRIVER_THREAD_H
#define __EMUL_DRIVER_THREAD_H

#include <condition_variable>
#include <exception>
#include <functional>
#include <memory>
#include <mutex>
#include <stdexcept>
#include <thread>
#include <utility>

// An optional pre-edge mailbox for the eventual driver thread. Only the
// reserving controller may submit work. A callback finishes (including capture
// destruction) before invoke returns; failures propagate without releasing the
// worker. launch transfers the SAME thread to the normal driver continuation.
// This coordinates a component, not process-wide exclusion or atomic restore.
class emul_driver_thread_t {
public:
  using task_t = std::function<void()>;
  // A coordinator may keep one prepared owner bundle on its creator across
  // launch. Its destructor must not throw. No live owner crosses to VCS.
  struct resource_t {
    virtual ~resource_t() = default;
  };
  using publication_factory_t = std::function<std::unique_ptr<resource_t>()>;
  emul_driver_thread_t() = default;
  emul_driver_thread_t(const emul_driver_thread_t &) = delete;
  emul_driver_thread_t &operator=(const emul_driver_thread_t &) = delete;
  ~emul_driver_thread_t() { cancel_reserved(); }

  void reserve() {
    std::lock_guard<std::mutex> lock(mutex);
    if (state != state_t::fresh || worker.joinable())
      throw std::runtime_error("driver creator already reserved/started");
    // Do not publish reservation if native thread creation fails.
    worker = std::thread([this] { serve(); });
    controller = std::this_thread::get_id();
    state = state_t::reserved;
  }

  void invoke(task_t task) {
    submit(std::move(task), false);
  }

  // Final creator validation may acquire coordinator-owned exclusion guards.
  // Keep them on this thread across controller-side publication/readback. No
  // further preparation is accepted after success. A throwing factory leaves
  // the reservation retryable and must unwind its own partially built guards.
  // This mechanism alone does not establish process-wide exclusion.
  void seal_launch(publication_factory_t prepare) {
    if (!prepare)
      throw std::runtime_error("missing driver publication guard factory");
    submit([this, prepare = std::move(prepare)] {
      auto guard = prepare();
      if (!guard)
        throw std::runtime_error("missing driver publication guard");
      publication_guard = std::move(guard);
    }, true);
  }

private:
  void submit(task_t task, bool seal) {
    std::unique_lock<std::mutex> lock(mutex);
    require_controller();
    if (state != state_t::reserved || !task || (seal && !continuation))
      throw std::runtime_error("driver creator not available for preparation");
    pending = std::move(task);
    sealing = seal;
    state = state_t::preparing;
    changed.notify_one();
    completed.wait(lock, [this] { return state != state_t::preparing; });
    auto failure = std::exchange(error, nullptr);
    lock.unlock();
    if (failure)
      std::rethrow_exception(failure);
  }

public:
  void launch(task_t task) {
    std::lock_guard<std::mutex> lock(mutex);
    if (!task)
      throw std::runtime_error("missing driver continuation");
    if (state == state_t::fresh) {
      worker = std::thread(std::move(task));
      controller = std::this_thread::get_id();
      state = state_t::launched;
      return;
    }
    require_controller();
    if (state != state_t::reserved || continuation)
      throw std::runtime_error("driver creator cannot launch twice/during preparation");
    pending = std::move(task);
    state = state_t::launched;
    changed.notify_one();
  }

  // Allocate/copy the continuation before any restore publication. This does
  // not wake the worker; invoke remains available for final owner validation.
  void prepare_launch(task_t task) {
    std::lock_guard<std::mutex> lock(mutex);
    require_controller();
    if (state != state_t::reserved || !task || continuation)
      throw std::runtime_error("driver continuation requires idle unprepared creator");
    continuation = std::move(task);
  }

  // Publication has already happened. A broken coordinator contract is fatal,
  // never a retryable exception that could release a partially restored DUT.
  void launch_prepared() noexcept {
    std::lock_guard<std::mutex> lock(mutex);
    if (controller != std::this_thread::get_id() ||
        (state != state_t::reserved && state != state_t::sealed) || !continuation)
      std::terminate();
    pending = std::move(continuation);
    state = state_t::launched;
    changed.notify_one();
  }

  bool launch_prepared_pending() const {
    std::lock_guard<std::mutex> lock(mutex);
    return bool(continuation);
  }

  bool publication_sealed() const {
    std::lock_guard<std::mutex> lock(mutex);
    return state == state_t::sealed;
  }

  void retain(std::unique_ptr<resource_t> resource) {
    std::lock_guard<std::mutex> lock(mutex);
    if (state != state_t::preparing || worker.get_id() != std::this_thread::get_id() ||
        !resource || resident)
      throw std::runtime_error("driver resource requires empty preparing creator");
    resident = std::move(resource);
  }

  bool reserved() const {
    std::lock_guard<std::mutex> lock(mutex);
    return state == state_t::reserved;
  }
  void require_controller_if_started() const {
    std::lock_guard<std::mutex> lock(mutex);
    if (state != state_t::fresh)
      require_controller();
  }
  std::thread::id get_id() const { return worker.get_id(); }
  void join() { worker.join(); }

  // Cancel an idle or sealed reservation. Running simulation threads retain the
  // existing contract: the owner must finish and join them before destruction.
  void cancel_reserved() noexcept {
    {
      std::lock_guard<std::mutex> lock(mutex);
      if (state != state_t::reserved && state != state_t::sealed)
        return;
      if (controller != std::this_thread::get_id())
        std::terminate();
      state = state_t::cancelled;
      changed.notify_one();
    }
    worker.join();
  }

private:
  enum class state_t { fresh, reserved, preparing, sealed, launched, cancelled };
  mutable std::mutex mutex;
  std::condition_variable changed, completed;
  std::thread worker;
  std::thread::id controller;
  state_t state = state_t::fresh;
  task_t pending;
  task_t continuation;
  std::unique_ptr<resource_t> resident;
  std::unique_ptr<resource_t> publication_guard;
  bool sealing = false;
  std::exception_ptr error;

  void require_controller() const {
    if (controller != std::this_thread::get_id())
      throw std::runtime_error("driver creator request from foreign thread");
  }
  void serve() {
    std::unique_lock<std::mutex> lock(mutex);
    for (;;) {
      changed.wait(lock, [this] {
        return state == state_t::preparing || state == state_t::launched ||
               state == state_t::cancelled;
      });
      if (state == state_t::cancelled) {
        auto abandoned = std::move(continuation);
        lock.unlock();
        // A staged continuation can hold references to creator-owned owners.
        abandoned = nullptr;
        publication_guard.reset();
        resident.reset();
        return;
      }
      const bool launching = state == state_t::launched;
      auto task = std::move(pending);
      lock.unlock();
      if (launching) {
        // Release creator-affine locks/masks before entering the continuation.
        // Destruction must not throw or allocate after owner publication.
        publication_guard.reset();
        task();
        // Continuation captures may refer to the resident bundle. Release
        // them first, then destroy the bundle on its original creator.
        task = nullptr;
        resident.reset();
        return;
      }
      std::exception_ptr failure;
      try { task(); } catch (...) { failure = std::current_exception(); }
      // Destroy captures on the creator before acknowledging the request.
      task = nullptr;
      lock.lock();
      error = failure;
      state = sealing && !failure ? state_t::sealed : state_t::reserved;
      completed.notify_one();
    }
  }
};
#endif
