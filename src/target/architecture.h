#include <cstdint>
#include <iostream>
#include <new>

#include "layout.h"

enum class CPU: u8 {
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

    bool allowsUnalignedLoads = false;

    static void populate(CPU cpu);

    static const Architecture& current();

};

extern Architecture currentArchitecture;
