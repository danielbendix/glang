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
    enum class Kind : u8 {
        Empty       = 0b00,
        Constraint  = 0b01,
        Type        = 0b10,
        Metatype    = 0b11,
    };

    constexpr static inline std::size_t BITS = 2;
    constexpr static inline std::size_t MIN_ALIGN = 1 << BITS;

    static_assert(alignof(Type) >= MIN_ALIGN);
    static_assert(alignof(TypeConstraint) >= MIN_ALIGN);

    llvm::PointerIntPair<void *, BITS, u8> pointer;

    TypeResult(llvm::PointerIntPair<void *, BITS, u8> pointer) : pointer{pointer} {}
    TypeResult(Type *type, Kind kind) : pointer{type, u8(kind)} {
        assert(kind == Kind::Type || kind == Kind::Metatype);
    }

    TypeResult(TypeConstraint *constraint) : pointer{constraint, u8(Kind::Constraint)} {}
public:
    TypeResult() : pointer{nullptr, u8(Kind::Empty)} {}

    /// This exists to potentially reset certain values back to default,
    /// but clone type information.
    /// It also serves to show that we're passing a type result upwards.
    [[nodiscard]]
    TypeResult clone() const {
        return TypeResult{pointer};
    }

    [[nodiscard]]
    static TypeResult failure() {
        return {};
    }

    static TypeResult constraint(TypeConstraint *constraint) {
        return {constraint};
    }

    [[nodiscard]]
    static TypeResult type(Type *type) {
        return {type, Kind::Type};
    }

    [[nodiscard]]
    static TypeResult metatype(Type *type) {
        return {type, Kind::Metatype};
    }

    [[nodiscard]]
    Kind kind() const {
        return Kind(pointer.getInt());
    }

    operator bool() const {
        return pointer.getOpaqueValue() != nullptr;
    }

    [[nodiscard]]
    bool isConstraint() const {
        return bool(*this) && kind() == Kind::Constraint;
    }

    [[nodiscard]]
    bool isType() const {
        return kind() == Kind::Type;
    }

    [[nodiscard]]
    bool isMetatype() const {
        return bool(*this) && kind() == Kind::Metatype;
    }

    [[nodiscard]]
    TypeConstraint *asConstraint() const {
        assert(kind() == Kind::Constraint);
        return (TypeConstraint *) pointer.getPointer();
    }

    [[nodiscard]]
    Type *asType() const {
        assert(kind() == Kind::Type);
        return (Type *) pointer.getPointer();
    }

    [[nodiscard]]
    Type *asMetatype() const {
        assert(kind() == Kind::Metatype);
        return (Type *) pointer.getPointer();
    }

    [[nodiscard]]
    Type *asTypeOrNull() const {
        if (kind() == Kind::Type) {
            return asType();
        } else {
            return nullptr;
        }
    }
};

// TODO: Fix naming
class _TypeResult {
    // Pointer to type or constraint
    llvm::PointerUnion<Type *, TypeConstraint *> pointer;
    // Pointer to new value if the expression was folded, and a bit set if lvalue.
    AST::Expression *folded;
public:
    _TypeResult() : pointer{nullptr}, folded{nullptr} {}
    explicit _TypeResult(std::nullptr_t) : pointer{(Type *)nullptr}, folded{nullptr} {}
    _TypeResult(Type *type) : pointer{type}, folded{nullptr} {}
    _TypeResult(TypeConstraint *constraint) : pointer{constraint}, folded{nullptr} {}
    
    bool isType() const {
        return isa<Type *>(pointer);
    }

    bool isConstraint() const {
        return isa<TypeConstraint *>(pointer);
    }

    Type *asType() const {
        return cast<Type *>(pointer);
    }

    Type *asTypeOrNull() const {
        return dyn_cast_or_null<Type *>(pointer);
    }

    TypeConstraint *asConstraint() const {
        return cast<TypeConstraint *>(pointer);
    }

    operator bool() {
        return pointer.getOpaqueValue() != nullptr;
    }
};

#endif // LANG_typecheck_internal_h
