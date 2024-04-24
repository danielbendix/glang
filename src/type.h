#ifndef LANG_type_h
#define LANG_type_h

#include "common.h"

#include "llvm/ADT/StringSet.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Support/Casting.h"

enum TypeConstraintKind {
    TK_Void,
    TK_Boolean,
    TK_Num_Integer,
    TK_Num_Floating,
    TK_String,

    TK_Pointer,
    TK_Optional,

    TK_Function,
    TK_Struct,
    TK_Protocol,

    TC_Literal,
};

class TypeConstraint {
protected:
    const TypeConstraintKind kind;

    TypeConstraint(TypeConstraintKind kind) : kind{kind} {}

public:
    TypeConstraintKind getKind() const {
        return kind;
    }

    static void deleteValue(TypeConstraint *type);
};

class PointerType;
class OptionalType;

class Type : public TypeConstraint {
    mutable llvm::Type *llvmType = nullptr;
    mutable PointerType *pointerType = nullptr;
    mutable OptionalType *optionalType = nullptr;

    llvm::Type *_getLLVMType(llvm::LLVMContext& context) const;
    PointerType *_getPointerType();
    OptionalType *_getOptionalType();

protected:
    using TypeConstraint::TypeConstraint;

public:
    llvm::Type *getLLVMType(llvm::LLVMContext& context) const {
        if (llvmType) {
            return llvmType;
        } else {
            return (llvmType = _getLLVMType(context));
        }
    }

    PointerType *getPointerType() {
        if (pointerType) {
            return pointerType;
        } else {
            return (pointerType = _getPointerType());
        }
    }

    OptionalType *getOptionalType() {
        if (optionalType) {
            return optionalType;
        } else {
            return (optionalType = _getOptionalType());
        }
    }

    bool isVoid() {
        return getKind() == TK_Void;
    }

    static bool classof(const TypeConstraint *typeConstraint) {
        return typeConstraint->getKind() <= TK_Protocol;
    }
};

class VoidType : public Type {
public:
    VoidType() : Type{TK_Void} {}
};

class BooleanType : public Type {
    mutable llvm::IntegerType *type;
public:
    BooleanType(llvm::IntegerType *type = nullptr) : Type{TK_Boolean}, type{type} {}

    llvm::IntegerType * getIntegerType(llvm::LLVMContext& context) const {
        if (!type) {
            type = llvm::Type::getInt1Ty(context);
        }
        return type;
    }

    static bool classof(const Type *type) {
        return type->getKind() == TK_Boolean;
    }
};

class NumericType : public Type {
protected:
    using Type::Type;

public:
    static bool classof(const Type *type) {
        auto kind = type->getKind();
        return kind == TK_Num_Integer || kind == TK_Num_Floating;
    }
};

class IntegerType : public NumericType {
public: // TODO: Remove
    unsigned bitWidth;
    bool isSigned;

    mutable llvm::IntegerType *type;

public:
    IntegerType(unsigned bitWidth, bool isSigned, llvm::IntegerType *type = nullptr) 
        : NumericType{TK_Num_Integer}
        , bitWidth{bitWidth}
        , isSigned{isSigned}
        , type{type} {}

    // FIXME: Currently this only supports positive values.
    // Signed integer types have an "extra" value in the negative because of two's complement.
    bool canFitValue(uint64_t value) {
        if (value == 0 || bitWidth > 64) {
            return true;
        }

        int signBit = isSigned ? 1 : 0;
        int bits = bitWidth - signBit;
        int leadingZeros = __builtin_clzl(value);

        return (64 - leadingZeros) <= bits;
    }

    unsigned getBitWidth() const { return bitWidth; }
    bool getIsSigned() const { return isSigned; }

    llvm::IntegerType * getIntegerType(llvm::LLVMContext& context) const {
        if (!type) {
            type = llvm::Type::getIntNTy(context, bitWidth);
        }
        return type;
    }
};

class PointerType : public Type {
    Type& pointeeType;

public:
    PointerType(Type *pointeeType) : Type{TK_Pointer}, pointeeType{*pointeeType} {
        // TODO: Set pointerType type on pointeeType
    }

    Type *getPointeeType() const {
        return &pointeeType;
    }

    static bool classof(const TypeConstraint *typeConstraint) {
        return typeConstraint->getKind() == TK_Pointer;
    }
};

class OptionalType : public Type {
    Type& contained;

public:
    OptionalType(Type *contained) : Type{TK_Optional}, contained{*contained} {}

    Type *getContained() const {
        return &contained;
    }

    llvm::Type *_getLLVMType(llvm::LLVMContext& context) const;

    static bool classof(const TypeConstraint *typeConstraint) {
        return typeConstraint->getKind() == TK_Optional;
    }
};

class FunctionType : public Type {
    Type *returnType;
    std::vector<Type *> parameters;
public:
    FunctionType(Type *returnType, std::vector<Type *>&& parameters) : Type{TK_Function}, returnType{returnType}, parameters{std::move(parameters)} {}

    Type *getReturnType() const {
        return returnType;
    }

    int parameterCount() const {
        return parameters.size();
    }

    Type *getParameter(int i) {
        return parameters[i];
    }

    Type *getParameter(int i) const {
        return parameters[i];
    }

    llvm::FunctionType *getFunctionType(llvm::LLVMContext& context) const;

    Iterable<Type *> getParameters() {
        return Iterable<Type *>(parameters);
    }

    ConstIterable<Type *> getParameters() const {
        return ConstIterable<Type *>(parameters);
    }

    static bool classof(const Type *type) {
        return type->getKind() == TK_Function;
    }
};

class FloatingType : public NumericType {
    unsigned bitWidth;

public:
    FloatingType(unsigned bitWidth) 
        : NumericType{TK_Num_Floating}
        , bitWidth{bitWidth} {}
};

class StringType : public Type {
    StringType() : Type{TK_String} {}
};

/// A type that can become multiple types, based on context.
class LiteralTypeConstraint : public TypeConstraint {
public:
    enum class Constraint {
        NumericLiteral,
        FloatingPointLiteral,
    };

    LiteralTypeConstraint(Constraint constraint) : TypeConstraint{TC_Literal} {}

};

void createPrimitiveTypes(llvm::LLVMContext& context, llvm::StringMap<Type>& table);

#endif // LANG_type_h
