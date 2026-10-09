// See LICENSE for license details.

#ifndef __ENTRY_H
#define __ENTRY_H

#include <memory>

class simif_t;
// Shared ownership declaration for the entry and DPI translation units.
// Declaring this unique_ptr as a raw derived pointer is an ABI/type mismatch.
extern std::unique_ptr<simif_t> simulator;

int entry(int argc, char **argv);

#endif // __ENTRY_H
