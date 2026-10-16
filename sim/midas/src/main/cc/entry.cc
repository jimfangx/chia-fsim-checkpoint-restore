// See LICENSE for license details.

#include <memory>

#include "core/config.h"
#include "core/simif.h"
#include "core/simulation.h"
#include "entry.h"
#include "core/checkpoint_entry.h"

// NOLINTBEGIN
#define GET_INCLUDES
#include "FireSim-generated.const.h"
#undef GET_INCLUDES
#include "bridges/loadmem.h" // always ensure that loadmem.h header is present for tsibridge to use loadmem

#define GET_METASIM_INTERFACE_CONFIG
#define GET_SUBSTRUCT_CHECKS
#include "FireSim-generated.const.h"
#undef GET_SUBSTRUCT_CHECKS
// NOLINTEND

// The user-defined part of the driver implements this method to return
// a simulation instance implementing all simulation-specific logic.
extern std::unique_ptr<simulation_t>
create_simulation(simif_t &simif,
                  widget_registry_t &registry,
                  const std::vector<std::string> &args);

// The platform-specific component of the driver implements this method
// to return a handle to the simulation.
extern std::unique_ptr<simif_t>
create_simif(const TargetConfig &config, int argc, char **argv);

std::unique_ptr<simif_t> simulator; // must exist for dpi.cc calls in emulation,
                                    // must be global to avoid deletion
std::unique_ptr<simulation_t> simulation; // must be global to avoid deletion
#ifdef FS_CHECKPOINT_ENTRY
static std::unique_ptr<checkpoint_entry_t> checkpoint_entry;
#endif

// Entry point of the driver.
int entry(int argc, char **argv) {
  try {
  // must have this exist for *.const.h
  std::vector<std::string> args(argv + 1, argv + argc);
  const auto request = checkpoint_request_t::parse(args);
  if (request.requested()) checkpoint_require_owned_observers();
#ifdef FS_CHECKPOINT_ENTRY
  checkpoint_entry = create_checkpoint_entry(args);
  if (!checkpoint_entry)
    throw std::runtime_error("enabled checkpoint entry has no capability");
  checkpoint_entry->authorize_request(request);
  puts("FS_CHECKPOINT_ENTRY_CONFIGURED before_platform=1");
#else
  if (request.durable())
    throw std::runtime_error("unsupported metasim runtime checkpoint cut/restore "
                             "(no public durable checkpoint capability)");
#endif

  // Create the hardware interface.
  simulator = create_simif(conf_target, argc, argv);
  // must have this exist for *.const.h
  auto &simif = *simulator;

  /* clang-format off */
  // NOLINTBEGIN
  widget_registry_t *widget_registry_ptr = new widget_registry_t(); // must be never deallocated to avoid deletion
  // must have this for *.const.h
  widget_registry_t &registry = *widget_registry_ptr;
  {
    // Here we instantiate our driver once for each bridge in the target. The
    // generated header contains a list of constructor calls. The different
    // macros ensure that widgets that are required by other bridges are
    // initialized in the correct order: the master, clock and loadmem widgets
    // are built first, followed by stream engines and the rest of the bridges
    // of the design, which can make use of widgets.
    #define GET_CORE_CONSTRUCTOR
    #include "FireSim-generated.const.h"
    #undef GET_CORE_CONSTRUCTOR
    #define GET_MANAGED_STREAM_CONSTRUCTOR
    #include "FireSim-generated.const.h"
    #undef GET_MANAGED_STREAM_CONSTRUCTOR
    #define GET_BRIDGE_CONSTRUCTOR
    #include "FireSim-generated.const.h"
    #undef GET_BRIDGE_CONSTRUCTOR
  }
  // NOLINTEND
  /* clang-format on */

  // A capability cannot bypass unsupported generated widgets. Inventory-only
  // preflight retains its diagnostic failure, without starting a simulation.
  if (request.kind != checkpoint_request_t::kind_t::none || request.periodic()) {
    registry.checkpoint_preflight();
    if (request.kind == checkpoint_request_t::kind_t::preflight)
      throw std::runtime_error("unsupported metasim runtime checkpoint cut/restore");
  }

  // Create the simulation instance.
  simulation = create_simulation(simif, *widget_registry_ptr, args);
  if (request.durable()) {
    const auto owners = simulation->checkpoint_driver_owners();
    if (owners.empty())
      throw std::runtime_error("unsupported empty checkpoint driver inventory");
    for (const auto &[name, owner] : owners)
      if (!owner || owner->checkpoint_schema().empty())
        throw std::runtime_error("unsupported checkpoint driver: " + name);
  }

#ifdef FS_CHECKPOINT_ENTRY
  puts("FS_CHECKPOINT_ENTRY_CONSTRUCTED before_run=1");
  const auto action = checkpoint_entry->prepare(simif, *simulation, request);
  if (request.kind == checkpoint_request_t::kind_t::restore &&
      action != checkpoint_entry_t::action_t::restore_reserved)
    throw std::runtime_error("checkpoint restore did not reserve fresh clocks");
  if (request.kind == checkpoint_request_t::kind_t::capture &&
      action != checkpoint_entry_t::action_t::run)
    throw std::runtime_error("checkpoint capture reserved restore clocks");
  if (action == checkpoint_entry_t::action_t::restore_reserved) {
    puts("FS_CHECKPOINT_ENTRY_RESERVED before_run=1");
    return 0;
  }
#endif

  // Run the simulation with the given implementation.
  return simif.run(*simulation);
  } catch (const std::exception &error) {
#ifdef FS_CHECKPOINT_ENTRY
    fprintf(stderr, "FS_CHECKPOINT_ENTRY_ERROR: %s\n", error.what());
#else
    fprintf(stderr, "FS_CHECKPOINT_UNSUPPORTED: %s\n", error.what());
#endif
    // VCS ignores entry's return value. Never continue after a failed prepare.
    std::exit(EXIT_FAILURE);
  }
}
