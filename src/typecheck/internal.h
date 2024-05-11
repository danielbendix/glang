#ifndef LANG_typecheck_internal_h
#define LANG_typecheck_internal_h

#include "typecheck.h"
#include "typeconstraint.h"

#include "llvm/ADT/PointerUnion.h"

extern BooleanType *boolean;
extern IntegerType *signed64;

class TypeResult {
    // Pointer to type or constraint
    llvm::PointerUnion<Type *, TypeConstraint *> pointer;
    // Pointer to new value if the expression was folded, and a bit set if lvalue.
    llvm::PointerIntPair<AST::Expression *, 1, bool> folded;
public:
    TypeResult() : pointer{nullptr}, folded{nullptr, false} {}
    explicit TypeResult(nullptr_t) : pointer{(Type *)nullptr}, folded{nullptr, false} {}
    TypeResult(Type *type) : pointer{type=type}, folded{nullptr, false} {}
    TypeResult(Type *type, bool canAssign) : pointer{type}, folded{nullptr, canAssign} {}
    TypeResult(TypeConstraint *constraint) : pointer{constraint}, folded{nullptr, false} {}

    //TypeResult(const TypeResult& result) : pointer{result.pointer}, canAssign{result.canAssign} {}
    
    bool canAssign() {
        return folded.getInt();
    }

    bool isType() const {
        return pointer.is<Type *>();
    }

    bool isConstraint() const {
        return pointer.is<TypeConstraint *>();
    }

    Type *type() const {
        return pointer.get<Type *>();
    }

    Type *asType() const {
        return pointer.get<Type *>();
    }

    operator Type*() {
        return pointer.get<Type *>();
    }

    TypeConstraint *constraint() const {
        return pointer.get<TypeConstraint *>();
    }

    TypeConstraint *asConstraint() const {
        return pointer.get<TypeConstraint *>();
    }

    operator TypeConstraint*() {
        return pointer.get<TypeConstraint *>();
    }

    operator bool() {
        return pointer.getOpaqueValue() != nullptr;
    }
};

#endif // LANG_typecheck_internal_h
