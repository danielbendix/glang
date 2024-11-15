#include <cstdint>
#include <iostream>
#include <new>

#include "layout.h"

enum class CPU: uint8_t {
    x86_64 = 1,
    arm64 = 2,
};

CPU detectCPU();

struct alignas(128) Architecture {
    const char *name;
    Layout pointer;

    Layout intptr;

    Layout int8;
    Layout int16;
    Layout int32;
    Layout int64;

    Layout fpSingle;
    Layout fpDouble;

    /// Size in bytes of a general-purpose register.
    uint8_t registerSize;
    /// How many register-sized slots to use to pass or return an aggregate value.
    uint8_t registerPackSize;

    static void populate(CPU cpu);

    static const Architecture& current();

};

extern Architecture currentArchitecture;
