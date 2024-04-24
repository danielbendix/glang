#ifndef LANG_typecheck_internal_h
#define LANG_typecheck_internal_h

#include "typecheck.h"

extern BooleanType *boolean;
extern IntegerType *signed64;

struct TypeResult {
    Type *type;
    bool canAssign;

    TypeResult() : type{nullptr}, canAssign{false} {}
    explicit TypeResult(nullptr_t) : type{nullptr}, canAssign{false} {}
    TypeResult(Type *type) : type{type}, canAssign{false} {}
    TypeResult(Type *type, bool canAssign) : type{type}, canAssign{canAssign} {}

    operator Type*() {
        return type;
    }

    operator bool() {
        return type;
    }
};

#endif // LANG_typecheck_internal_h
