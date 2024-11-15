#ifndef LANG_type_h
#define LANG_type_h

#include "common.h"
#include "layout.h"
#include "containers/symbol_table.h"

#include "llvm/ADT/StringSet.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Support/Casting.h"

enum TypeKind {
    TK_Void,
    TK_Boolean,
    TK_Num_Integer,
    TK_Num_FP,
    TK_String,

    TK_Function,
    TK_Struct,
    TK_Enum,
    TK_Protocol,

    TK_Pointer,
    TK_Optional,

    TK_Array,
    TK_Range,
};

class PointerType;
class OptionalType;
class ArrayType;
class RangeType;

class Type {
    // Used to intern types, but keep base Type objects small.
    struct TypeIndex {
        ArrayType *boundedArrayType = nullptr;
        ArrayType *unboundedArrayType = nullptr;
        RangeType *openRangeType = nullptr;
        RangeType *closedRangeType = nullptr;
    };

    const TypeKind kind;
    mutable llvm::Type *llvmType = nullptr;
    mutable PointerType *pointerType = nullptr;
    mutable OptionalType *optionalType = nullptr;
    mutable TypeIndex *index = nullptr;

    llvm::Type *_getLLVMType(llvm::LLVMContext& context) const;
    PointerType *_getPointerType();
    OptionalType *_getOptionalType();

protected:
    TypeIndex& getTypeIndex();

    Type(TypeKind kind) : kind{kind} {}
    Type(const Type&) = delete;
    Type& operator=(const Type&) = delete;
    Type(Type&&) = delete;
    Type& operator=(Type&&) = delete;

public:
    Layout getLayout() const;

    llvm::Type *getLLVMType(llvm::LLVMContext& context) const {
        if (llvmType) {
            return llvmType;
        } else {
            return (llvmType = _getLLVMType(context));
        }
    }

    std::string makeName() const;

    operator std::string() const {
        return makeName();
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

    ArrayType *getBoundedArrayType();
    ArrayType *getUnboundedArrayType();

    Type *removeImplicitWrapperTypes();

    bool isVoid() {
        return getKind() == TK_Void;
    }

    TypeKind getKind() const {
        return kind;
    }

    static void deleteValue(Type *type);
};

class VoidType : public Type {
public:
    const Symbol& name;
    VoidType(Symbol& name) : Type{TK_Void}, name{name} {}

    void getName(std::string& result) const {
        result.append(name.string_view());
    }

    static bool classof(const Type *type) {
        return type->getKind() == TK_Void;
    }
};

class BooleanType : public Type {
    Symbol& name;
public:
    const Layout layout;
public:
    BooleanType(Symbol& name, Layout layout) : Type{TK_Boolean}, name{name}, layout{layout} {}

    void getName(std::string& result) const {
        result.append(name.string_view());
    }

    llvm::IntegerType *_getLLVMType(llvm::LLVMContext& context) const {
        return llvm::Type::getInt1Ty(context);
    }

    static bool classof(const Type *type) {
        return type->getKind() == TK_Boolean;
    }

    friend class Type;
};

class NumericType : public Type {
protected:
    using Type::Type;

public:
    static bool classof(const Type *type) {
        auto kind = type->getKind();
        return kind == TK_Num_Integer || kind == TK_Num_FP;
    }
};

class IntegerType : public NumericType {
public:
    const Symbol& name;
    const bool isSigned;
    const uint32_t bitWidth;
    const Layout layout;

    IntegerType(Symbol& name, unsigned bitWidth, bool isSigned, Layout layout) 
        : NumericType{TK_Num_Integer}
        , name{name}
        , bitWidth{bitWidth}
        , isSigned{isSigned}
        , layout{layout} {}

    void getName(std::string& result) const {
        result.append(name.string_view());
    }

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

    RangeType *getOpenRangeType();
    RangeType *getClosedRangeType();

    unsigned getBitWidth() const { return bitWidth; }
    bool getIsSigned() const { return isSigned; }

    llvm::IntegerType *_getLLVMType(llvm::LLVMContext& context) const {
        return llvm::Type::getIntNTy(context, bitWidth);
    }

    static bool classof(const Type *type) {
        return type->getKind() == TK_Num_Integer;
    }
};

class PointerType : public Type {
    Type& pointeeType;

public:
    PointerType(Type *pointeeType) : Type{TK_Pointer}, pointeeType{*pointeeType} {}

    void getName(std::string& result) const;

    Type *getPointeeType() const {
        return &pointeeType;
    }

    static bool classof(const Type *typeConstraint) {
        return typeConstraint->getKind() == TK_Pointer;
    }
};

class OptionalType : public Type {
    Type& contained;

public:
    OptionalType(Type *contained) : Type{TK_Optional}, contained{*contained} {}

    void getName(std::string& result) const;

    Type *getContained() const {
        return &contained;
    }

    llvm::Type *_getLLVMType(llvm::LLVMContext& context) const;

    static bool classof(const Type *typeConstraint) {
        return typeConstraint->getKind() == TK_Optional;
    }
};

class FunctionType : public Type {
    Type *returnType;
    Type **parameters;
    size_t parametersSize;
public:
    FunctionType(Type *returnType, Type ** parameters, size_t parametersSize) 
        : Type{TK_Function}, returnType{returnType}, parameters{parameters}, parametersSize{parametersSize} {}

    void getName(std::string& result) const;

    Type *getReturnType() const {
        return returnType;
    }

    int parameterCount() const {
        return parametersSize;
    }

    Type *getParameter(int i) {
        return parameters[i];
    }

    Type *getParameter(int i) const {
        return parameters[i];
    }

    llvm::FunctionType *getFunctionType(llvm::LLVMContext& context) const;

    static bool classof(const Type *type) {
        return type->getKind() == TK_Function;
    }
};

class FPType : public NumericType {
public:
    enum class Precision: uint8_t {
        Single = 0,
        Double = 1,
    };
    const Symbol& name;
    const Precision precision;
    const Layout layout;

    FPType(Symbol& name, Precision precision, Layout layout) 
        : NumericType{TK_Num_FP}
        , name{name}
        , precision{precision} 
        , layout{layout} {}

    void getName(std::string& result) const {
        result.append(name.string_view());
    }

    llvm::Type *_getLLVMType(llvm::LLVMContext& context) const;

    uint32_t fractionBits() {
        switch (precision) {
            case Precision::Single:
                return 23;
            case Precision::Double:
                return 52;
        }
    }

    uint32_t exponentBits() {
        switch (precision) {
            case Precision::Single:
                return 8;
            case Precision::Double:
                return 11;
        }
    }

    static bool classof(const Type *type) {
        return type->getKind() == TK_Num_FP;
    }
};

class StringType : public Type {
public:
    StringType() : Type{TK_String} {}

    void getName(std::string& result) const;
};

class ArrayType : public Type {
    static llvm::Type *llvmTypeBounded;
    static llvm::Type *llvmTypeUnbounded;

    Type& contained;
public:
    const bool isBounded;

    ArrayType(Type& contained, bool isBounded) : Type{TK_Array}, contained{contained}, isBounded{isBounded} {}

    void getName(std::string& result) const;

    Type *getContained() const {
        return &contained;
    }

    llvm::Type *_getLLVMType(llvm::LLVMContext& context) const;

    static bool classof(const Type *type) {
        return type->getKind() == TK_Array;
    }
};

// This is a quick and dirty type to get ranges working.
// Ideally it would be based on conformance to a protocol.
class RangeType : public Type {
    IntegerType& integerType;
public:
    bool isClosed;

    RangeType(IntegerType& integerType, bool isClosed) : Type{TK_Range}, integerType{integerType}, isClosed{isClosed} {}

    void getName(std::string& result) const;

    IntegerType *getBoundType() const {
        return &integerType;
    }

    llvm::Type *_getLLVMType(llvm::LLVMContext& context) const;

    static bool classof(const Type *type) {
        return type->getKind() == TK_Range;
    }
};

#endif // LANG_type_h
