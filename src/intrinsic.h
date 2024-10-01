#ifndef LANG_intrinsic_h
#define LANG_intrinsic_h

#include "common.h"


enum class IntrinsicKind: uint8_t {
    Truncate,
    Print,
    Assert,
    Bitcast,
//    SignExtend,
//    ZeroExtend,
//    Memory,
};

#endif // LANG_intrinsic_h
