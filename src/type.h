#ifndef LANG_type_h
#define LANG_type_h

#include "common.h"

#include "llvm/ADT/StringSet.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Support/Casting.h"

enum TypeKind {
    TK_Void,
    TK_Boolean,
    TK_Num_Integer,
    TK_Num_Floating,
    TK_Function,
    TK_String,
    TK_Struct,
    TK_Class,
    TK_Protocol,
};

class Type {
    TypeKind kind;
    mutable llvm::Type *llvmType;
    
    llvm::Type *_getLLVMType(llvm::LLVMContext& context);

protected:
    Type(TypeKind kind) : kind{kind} {}

public:
    TypeKind getKind() const {
        return kind;
    }

    llvm::Type *getLLVMType(llvm::LLVMContext& context) {
        if (llvmType) {
            return llvmType;
        } else {
            return (llvmType = _getLLVMType(context));
        }
    }

    bool isVoid() {
        return kind == TK_Void;
    }
};

class VoidType : public Type {
public:
    VoidType() : Type{TK_Void} {}
};

class BooleanType : public Type {
    llvm::IntegerType *type;
public:
    BooleanType(llvm::IntegerType *type = nullptr) : Type{TK_Boolean}, type{type} {}

    llvm::IntegerType * getIntegerType(llvm::LLVMContext& context) {
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
        TypeKind kind = type->getKind();
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

#include <iostream>

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

    llvm::FunctionType *getFunctionType(llvm::LLVMContext& context);

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
    llvm::Type *type;

public:
    FloatingType(unsigned bitWidth, llvm::Type *type) 
        : NumericType{TK_Num_Floating}
        , bitWidth{bitWidth}
        , type{type} {}
};

class StringType : public Type {
    StringType() : Type{TK_String} {}
};

void createPrimitiveTypes(llvm::LLVMContext& context, llvm::StringMap<Type>& table);

#endif // LANG_type_h
