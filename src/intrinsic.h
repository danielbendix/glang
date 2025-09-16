#ifndef LANG_intrinsic_h
#define LANG_intrinsic_h

#include "common.h"

enum class IntrinsicKind: u8 {
    /// Truncation of integer values.
    Truncate,
    /// Intermittent support for printing.
    Print,
    /// Assert a condition, trapping or breaking if false.
    Assert,
    /// Cast a value to another, e.g. from one pointer type to another.
    Cast,
    /// Reinterpret a value as another in a bitwise manner, e.g. `i64` to `double`.
    Bitcast,
    /// Intermittent support for heap allocation, calls `malloc` under the hood.
    Allocate,
    /// Intermittent support for freeing values return from #allocate.
    Free,
};

#endif // LANG_intrinsic_h
