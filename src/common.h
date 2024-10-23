#ifndef LANG_common_h
#define LANG_common_h

#include "templates.h"
#include <memory>
#include <vector>
#include <cstdint>

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;

using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;

#ifdef __clang__
    #define NULLABLE _Nullable
    #define NONNULL _Nonnull
#else
    #define NULLABLE
    #define NONNULL
#endif

enum class PassResultKind : u8 {
    OK = 0,
    ERROR = 1,
};

class PassResult {
    PassResultKind kind;

public:
    PassResult(PassResultKind kind) : kind{kind} {}

    void error_if_false(bool value) {
        if (!value) {
            kind = PassResultKind::ERROR;
        }
    }

    void error_if_true(bool value) {
        if (value) {
            kind = PassResultKind::ERROR;
        }
    }

    bool ok() const {
        return kind == PassResultKind::OK;
    }

    bool failed() const {
        return kind != PassResultKind::OK;
    }

    // TODO: Consider implicit conversion to bool, where error is true,
    // Just like functions that return 0 on success.

    friend PassResult operator|(PassResult lhs, PassResult rhs);
    friend PassResult& operator|=(PassResult& lhs, PassResult rhs);
};

#endif // LANG_common_h
