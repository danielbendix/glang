#include "architecture.h"

#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/ADT/StringRef.h"

CPU detectCPU() {
    using enum CPU;
    auto tripleName= llvm::sys::getDefaultTargetTriple();
    llvm::Triple triple(tripleName);
    auto architectureName = triple.getArchName();

    if (architectureName == "arm64") {
        return arm64;
    } else if (architectureName == "x86_64") {
        return x86_64;
    } else {
        std::cerr << "Unsupported architecture: " << std::string_view{architectureName.begin(), architectureName.end()} << '\n';
        exit(1);
    }
}

Architecture currentArchitecture = {
    .name = nullptr,

    .pointer = {0, 0},

    .intptr = {0, 0},

    .int8 = {0, 0},
    .int16 = {0, 0},
    .int32 = {0, 0},
    .int64 = {0, 0},

    .fpSingle = {0, 0},
    .fpDouble = {0, 0},

    .registerSize = 0,
    .registerPackSize = 0,
};

void setupARM64();
void setupX86_64();

void Architecture::populate(CPU cpu) {
    switch (cpu) {
        case CPU::x86_64:
            return setupARM64();
        case CPU::arm64:
            return setupX86_64();
    }
}

const Architecture& Architecture::current() {
    return currentArchitecture;
}
