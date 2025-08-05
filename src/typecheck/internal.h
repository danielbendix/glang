#ifndef LANG_typecheck_internal_h
#define LANG_typecheck_internal_h

#include "typecheck.h"
#include "typeconstraint.h"

#include "llvm/ADT/PointerUnion.h"

using llvm::isa;
using llvm::cast;

extern BooleanType *boolean;
extern IntegerType *signed64;

class TypeResult {
    // Pointer to type or constraint
    llvm::PointerUnion<Type *, TypeConstraint *> pointer;
    // Pointer to new value if the expression was folded, and a bit set if lvalue.
    llvm::PointerIntPair<AST::Expression *, 1, bool> folded;
public:
    TypeResult() : pointer{nullptr}, folded{nullptr, false} {}
    explicit TypeResult(std::nullptr_t) : pointer{(Type *)nullptr}, folded{nullptr, false} {}
    TypeResult(Type *type) : pointer{type=type}, folded{nullptr, false} {}
    TypeResult(Type *type, bool canAssign) : pointer{type}, folded{nullptr, canAssign} {}
    TypeResult(TypeConstraint *constraint) : pointer{constraint}, folded{nullptr, false} {}

    //TypeResult(const TypeResult& result) : pointer{result.pointer}, canAssign{result.canAssign} {}
    
    bool canAssign() {
        return folded.getInt();
    }

    bool isType() const {
        return isa<Type *>(pointer);
    }

    bool isConstraint() const {
        return isa<TypeConstraint *>(pointer);
    }

    Type *type() const {
        return cast<Type *>(pointer);
    }

    Type *asType() const {
        return cast<Type *>(pointer);
    }

    Type *asTypeOrNull() const {
        return *this;
    }

    operator Type*() const {
        return llvm::dyn_cast_or_null<Type *>(pointer);
    }

    TypeConstraint *constraint() const {
        return cast<TypeConstraint *>(pointer);
    }

    TypeConstraint *asConstraint() const {
        return cast<TypeConstraint *>(pointer);
    }

    operator TypeConstraint*() {
        return cast<TypeConstraint *>(pointer);
    }

    operator bool() {
        return pointer.getOpaqueValue() != nullptr;
    }
};

#endif // LANG_typecheck_internal_h
