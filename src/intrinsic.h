#ifndef LANG_intrinsic_h
#define LANG_intrinsic_h

#include "common.h"

enum class IntrinsicKind: u8 {
    Truncate,
    Print,
    Assert,
    Bitcast,
//    SignExtend,
//    ZeroExtend,
//    Memory,
};

#endif // LANG_intrinsic_h
