// See LICENSE for license details.
#ifndef __EMUL_DRIVER_THREAD_H
#define __EMUL_DRIVER_THREAD_H

#include <condition_variable>
#include <exception>
#include <functional>
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
    std::unique_lock<std::mutex> lock(mutex);
    require_controller();
    if (state != state_t::reserved || !task)
      throw std::runtime_error("driver creator not available for preparation");
    pending = std::move(task);
    state = state_t::preparing;
    changed.notify_one();
    completed.wait(lock, [this] { return state == state_t::reserved; });
    auto failure = std::exchange(error, nullptr);
    lock.unlock();
    if (failure)
      std::rethrow_exception(failure);
  }

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
    if (state != state_t::reserved)
      throw std::runtime_error("driver creator cannot launch twice/during preparation");
    pending = std::move(task);
    state = state_t::launched;
    changed.notify_one();
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

  // Cancel only an idle reservation. Running simulation threads retain the
  // existing contract: the owner must finish and join them before destruction.
  void cancel_reserved() noexcept {
    {
      std::lock_guard<std::mutex> lock(mutex);
      if (state != state_t::reserved)
        return;
      if (controller != std::this_thread::get_id())
        std::terminate();
      state = state_t::cancelled;
      changed.notify_one();
    }
    worker.join();
  }

private:
  enum class state_t { fresh, reserved, preparing, launched, cancelled };
  mutable std::mutex mutex;
  std::condition_variable changed, completed;
  std::thread worker;
  std::thread::id controller;
  state_t state = state_t::fresh;
  task_t pending;
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
      if (state == state_t::cancelled)
        return;
      const bool launching = state == state_t::launched;
      auto task = std::move(pending);
      lock.unlock();
      if (launching) {
        task();
        return;
      }
      std::exception_ptr failure;
      try { task(); } catch (...) { failure = std::current_exception(); }
      // Destroy captures on the creator before acknowledging the request.
      task = nullptr;
      lock.lock();
      error = failure;
      state = state_t::reserved;
      completed.notify_one();
    }
  }
};
#endif
