#include "architecture.h"

void setupARM64() {
    currentArchitecture = {
        .name = "arm64",

        .pointer = Layout{Align{3}, 8},
        .intptr = Layout{Align{3}, 8},

        .int8 = Layout{Align{0}, 1},
        .int16 = Layout{Align{1}, 2},
        .int32 = Layout{Align{2}, 4},
        .int64 = Layout{Align{3}, 8},

        .fpSingle = Layout{Align{2}, 4},
        .fpDouble = Layout{Align{3}, 8},

        .registerSize = 8,
        .registerPackSize = 4,
    };
}
