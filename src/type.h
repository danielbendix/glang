#ifndef LANG_type_h
#define LANG_type_h

#include "llvm/ADT/StringSet.h"
#include "llvm/IR/Type.h"

enum class TypeKind {
    Primitive,
    Class,
    Struct,
    Enum,
    Protocol,
};

enum class PrimitiveKind {
    Boolean,
    Integer,
    Floating,
    String,
};

class alignas(4) Type {
    //int a;

};

class PrimitiveType : public Type {

};

class BooleanType : public PrimitiveType {
    llvm::IntegerType *type;
public:
    BooleanType(llvm::IntegerType *type = nullptr) : type{type} {}

    llvm::IntegerType * getIntegerType(llvm::LLVMContext& context) {
        if (!type) {
            type = llvm::Type::getInt1Ty(context);
        }
        return type;
    }
};

class IntegerType : public PrimitiveType {
public: // TODO: Remove
    unsigned bitWidth;
    bool isSigned;

    llvm::IntegerType *type;

public:
    IntegerType(unsigned bitWidth, bool isSigned, llvm::IntegerType *type = nullptr) : bitWidth{bitWidth}, isSigned{isSigned}, type{type} {}

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

    llvm::IntegerType * getIntegerType(llvm::LLVMContext& context) {
        if (!type) {
            type = llvm::Type::getIntNTy(context, bitWidth);
        }
        return type;
    }
};

class FloatingType : public PrimitiveType {
    unsigned bitWidth;
    llvm::Type *type;

public:
    FloatingType(unsigned bitWidth, llvm::Type *type) : bitWidth{bitWidth}, type{type} {}
};

class StringType : public PrimitiveType {

};

class BuiltinType : public Type {

};

void createPrimitiveTypes(llvm::LLVMContext& context, llvm::StringMap<Type>& table);

#endif // LANG_type_h
