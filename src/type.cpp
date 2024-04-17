#include "type.h"
#include "type/struct.h"

#include "containers/string_map.h"

void Type::deleteValue(Type *type) {
    switch (type->getKind()) {
        case TK_Void: return delete static_cast<VoidType *>(type);
        case TK_Boolean: return delete static_cast<BooleanType *>(type);
        case TK_Num_Integer: return delete static_cast<IntegerType *>(type);
        case TK_Num_Floating: return delete static_cast<FloatingType *>(type);
        case TK_String: return delete static_cast<StringType *>(type);
        case TK_Function: return delete static_cast<FunctionType *>(type);
        case TK_Struct: return delete static_cast<StructType *>(type);
        case TK_Protocol: llvm_unreachable("Unsupported type type.");
    }
}

void createNumericTypes(StringMap<Type *>& table)
{
    table.insert("bool", new BooleanType);

    table.insert("float", new FloatingType{32});
    table.insert("double", new FloatingType{64});

#define INT_TYPE(bits) { \
    Type *type = new IntegerType{bits, true}; \
    table.insert("i" #bits, type); \
    table.insert("int" #bits, type); }
    INT_TYPE(8);
    INT_TYPE(16);
    INT_TYPE(32);
    INT_TYPE(64);
#undef INT_TYPE

#define UINT_TYPE(bits) { \
    Type *type = new IntegerType{bits, false}; \
    table.insert("u" #bits, type); \
    table.insert("uint" #bits, type); }
    UINT_TYPE(8);
    UINT_TYPE(16);
    UINT_TYPE(32);
    UINT_TYPE(64);
#undef INT_TYPE
}

void createStringType(StringMap<Type *>& table)
{
    

}

void createPrimitiveTypes(StringMap<Type *>& table)
{
    createNumericTypes(table);
    createStringType(table);
}

llvm::FunctionType *FunctionType::getFunctionType(llvm::LLVMContext& context) {
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

llvm::Type *Type::_getLLVMType(llvm::LLVMContext& context) {
    switch (kind) {
        case TK_Void:
            return llvm::Type::getVoidTy(context);
        case TK_Boolean:
            return static_cast<BooleanType *>(this)->getIntegerType(context);
        case TK_Num_Integer:
            return static_cast<IntegerType *>(this)->getIntegerType(context);
        case TK_Function:
            return static_cast<FunctionType *>(this)->getFunctionType(context);
        case TK_Struct:
            return static_cast<StructType *>(this)->getStructType(context);
        default:
            assert(false);
    }
}
