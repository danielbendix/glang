#include "type.h"
#include "type/struct.h"
#include "type/enum.h"

#include "containers/string_map.h"

using llvm::dyn_cast;

Type::TypeIndex& Type::getTypeIndex() {
    if (index) {
        return *index;
    } else {
        auto index = new TypeIndex{};
        this->index = index;
        return *index;
    }
}

ArrayType *Type::getBoundedArrayType() {
    auto index = getTypeIndex();

    if (index.boundedArrayType) {
        return index.boundedArrayType;
    } else {
        auto type = new ArrayType(*this, true);
        index.boundedArrayType = type;
        return type;
    }
}

ArrayType *Type::getUnboundedArrayType() {
    auto index = getTypeIndex();

    if (index.unboundedArrayType) {
        return index.unboundedArrayType;
    } else {
        auto type = new ArrayType(*this, false);
        index.unboundedArrayType = type;
        return type;
    }
}

RangeType *IntegerType::getOpenRangeType() {
    auto index = getTypeIndex();

    if (index.openRangeType) {
        return index.openRangeType;
    } else {
        auto type = new RangeType(*this, false);
        index.openRangeType = type;
        return type;
    }
}

RangeType *IntegerType::getClosedRangeType() {
    auto index = getTypeIndex();

    if (index.closedRangeType) {
        return index.closedRangeType;
    } else {
        auto type = new RangeType(*this, true);
        index.closedRangeType = type;
        return type;
    }
}

Type *Type::removeImplicitWrapperTypes()
{
    switch (getKind()) {
        case TK_Void:
        case TK_Boolean:
        case TK_Num_Integer:
        case TK_Num_FP:
        case TK_String:
        case TK_Function:
        case TK_Struct:
        case TK_Protocol:
        case TK_Enum:
        case TK_Pointer:
        case TK_Array:
        case TK_Range:
            return this;
        case TK_Optional:
            return static_cast<OptionalType *>(this)->getContained()->removeImplicitWrapperTypes();
    }
}

void Type::deleteValue(Type *type) {
    switch (type->getKind()) {
        case TK_Void: return delete static_cast<VoidType *>(type);
        case TK_Boolean: return delete static_cast<BooleanType *>(type);
        case TK_Num_Integer: return delete static_cast<IntegerType *>(type);
        case TK_Num_FP: return delete static_cast<FPType *>(type);
        case TK_String: return delete static_cast<StringType *>(type);
        case TK_Function: return delete static_cast<FunctionType *>(type);
        case TK_Struct: return delete static_cast<StructType *>(type);
        case TK_Protocol: llvm_unreachable("Unsupported type type.");
        case TK_Enum: return delete static_cast<EnumType *>(type);

        case TK_Optional: return delete static_cast<OptionalType *>(type);
        case TK_Pointer: return delete static_cast<PointerType *>(type);

        case TK_Array: return delete static_cast<ArrayType *>(type);
        case TK_Range: return delete static_cast<RangeType *>(type);
    }
}

void createNumericTypes(StringMap<Type *>& table, std::vector<Type *>& owner)
{
    Type *booleanType = new BooleanType;
    table.insert("bool", booleanType);
    owner.push_back(booleanType);

    Type *f32Type = new FPType{FPType::Precision::Single};
    table.insert("f32", f32Type);
    owner.push_back(f32Type);

    Type *f64Type = new FPType{FPType::Precision::Double};
    table.insert("f64", f64Type);
    owner.push_back(f64Type);

    // TODO: Add [ui]size, [ui]ptr
    

#define INT_TYPE(bits) { \
    Type *type = new IntegerType{bits, true}; \
    table.insert("i" #bits, type); \
    table.insert("int" #bits, type); \
    owner.push_back(type); }
    INT_TYPE(8);
    INT_TYPE(16);
    INT_TYPE(32);
    INT_TYPE(64);
#undef INT_TYPE

#define UINT_TYPE(bits) { \
    Type *type = new IntegerType{bits, false}; \
    table.insert("u" #bits, type); \
    table.insert("uint" #bits, type); \
    owner.push_back(type); }
    UINT_TYPE(8);
    UINT_TYPE(16);
    UINT_TYPE(32);
    UINT_TYPE(64);
#undef INT_TYPE
}

void createStringType(StringMap<Type *>& table)
{
    

}

void createPrimitiveTypes(StringMap<Type *>& table, std::vector<Type *>& owner)
{
    createNumericTypes(table, owner);
    createStringType(table);
}

PointerType *Type::_getPointerType() {
    PointerType *pointerType = new PointerType(this);
    return pointerType;
}

OptionalType *Type::_getOptionalType() {
    OptionalType *optionalType = new OptionalType(this);
    return optionalType;
}

llvm::FunctionType *FunctionType::getFunctionType(llvm::LLVMContext& context) const {
    auto llvmReturnType = returnType->getLLVMType(context);
    if (parameters.size() > 0) {
        llvm::Type *parameterTypes[parameters.size()];
        int i = 0;
        for (auto* type : parameters) {
            parameterTypes[i] = type->getLLVMType(context);
            ++i;
        }
        return llvm::FunctionType::get(llvmReturnType, llvm::ArrayRef(parameterTypes, parameters.size()), false);
    } else {
        return llvm::FunctionType::get(llvmReturnType, false);
    }
}

llvm::Type *ArrayType::llvmTypeBounded = nullptr;
llvm::Type *ArrayType::llvmTypeUnbounded = nullptr;

llvm::Type *ArrayType::_getLLVMType(llvm::LLVMContext& context) const {
    if (isBounded) {
        if (llvmTypeBounded) {
            return llvmTypeBounded;
        } else {
            llvm::Type *childTypes[2] = {
                llvm::PointerType::getUnqual(context),
                // TODO: This should be architecture dependent, usize
                llvm::IntegerType::get(context, 64),
            };
            auto type = llvm::StructType::get(context, childTypes, false);
            llvmTypeBounded = type;
            return type;
        }
    } else {
        if (llvmTypeUnbounded) {
            return llvmTypeUnbounded;
        } else {
            llvm::Type *childTypes[1] = {
                llvm::PointerType::getUnqual(context),
            };
            auto type = llvm::StructType::get(context, childTypes, false);
            llvmTypeUnbounded = type;
            return type;
        }
    }
}

llvm::Type *RangeType::_getLLVMType(llvm::LLVMContext& context) const {
    auto llvmBoundType = integerType.getLLVMType(context);

    llvm::Type *childTypes[2] = {
        llvmBoundType,
        llvmBoundType,
    };

    return llvm::StructType::get(context, childTypes, false);
}

llvm::Type *Type::_getLLVMType(llvm::LLVMContext& context) const {
    switch (kind) {
        case TK_Void:
            return llvm::Type::getVoidTy(context);
        case TK_Boolean:
            return static_cast<const BooleanType *>(this)->getIntegerType(context);
        case TK_Num_Integer:
            return static_cast<const IntegerType *>(this)->getIntegerType(context);
        case TK_Num_FP:
            return static_cast<const FPType *>(this)->_getLLVMType(context);
        case TK_Function:
            return static_cast<const FunctionType *>(this)->getFunctionType(context);
        case TK_Struct:
            return static_cast<const StructType *>(this)->getStructType(context);
        case TK_Pointer:
            return llvm::PointerType::getUnqual(context);
        case TK_Optional:
            return static_cast<const OptionalType *>(this)->_getLLVMType(context);
        case TK_Array:
            return static_cast<const ArrayType *>(this)->_getLLVMType(context);
        case TK_Range:
            return static_cast<const RangeType *>(this)->_getLLVMType(context);

        default:
            assert(false);
    }
}

llvm::Type *FPType::_getLLVMType(llvm::LLVMContext& context) const {
    switch (precision) {
        case FPType::Precision::Single:
            return llvm::Type::getFloatTy(context);
        case FPType::Precision::Double:
            return llvm::Type::getDoubleTy(context);
    }
}

llvm::Type *OptionalType::_getLLVMType(llvm::LLVMContext& context) const {
    if (auto pointerType = dyn_cast<PointerType>(&contained)) {
        return pointerType->getLLVMType(context);
    } else {
        auto child = contained.getLLVMType(context);
        auto flag = llvm::IntegerType::get(context, 1);
        return llvm::StructType::get(flag, child);
    }
}
