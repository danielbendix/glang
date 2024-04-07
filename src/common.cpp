#include "common.h"

PassResult operator|(PassResult lhs, PassResult rhs) {
    using enum PassResultKind;
    switch (lhs.kind) {
        case OK: return rhs;
        case ERROR: return ERROR;
    }
}

PassResult& operator|=(PassResult& lhs, PassResult rhs) {
    using enum PassResultKind;
    switch (lhs.kind) {
        case OK: lhs = rhs; break;
        case ERROR: break;
    }

    return lhs;
}
