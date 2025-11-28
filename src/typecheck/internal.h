#ifndef LANG_typecheck_internal_h
#define LANG_typecheck_internal_h

#include "typecheck.h"
#include "typeconstraint.h"

#include "llvm/ADT/PointerUnion.h"

using llvm::isa;
using llvm::cast;

extern BooleanType *boolean;
extern IntegerType *signed64;

// TODO: Fix naming
class TypeResult {
    // Pointer to type or constraint
    llvm::PointerUnion<Type *, TypeConstraint *> pointer;
    // Pointer to new value if the expression was folded, and a bit set if lvalue.
    AST::Expression *folded;
public:
    TypeResult() : pointer{nullptr}, folded{nullptr} {}
    explicit TypeResult(std::nullptr_t) : pointer{(Type *)nullptr}, folded{nullptr} {}
    TypeResult(Type *type) : pointer{type}, folded{nullptr} {}
    TypeResult(TypeConstraint *constraint) : pointer{constraint}, folded{nullptr} {}
    
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
