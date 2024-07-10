#include "type.h"
#include "type/struct.h"
#include "type/enum.h"

#include "containers/string_map.h"

using llvm::dyn_cast;

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
