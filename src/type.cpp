#include "type.h"

void createNumericTypes(llvm::LLVMContext& context, llvm::StringMap<std::unique_ptr<Type>>& table)
{
    table.insert(std::make_pair("bool", std::make_unique<BooleanType>(llvm::Type::getInt1Ty(context))));

//    table.insert(std::make_pair("float", FloatingType(32, llvm::Type::getFloatTy(context))));
//    table.insert(std::make_pair("double", FloatingType(64, llvm::Type::getDoubleTy(context))));

//    table.insert(std::make_pair("int8", IntegerType(8, true, llvm::Type::getInt8Ty(context))));
//    table.insert(std::make_pair("int16", IntegerType(16, true, llvm::Type::getInt16Ty(context))));
//    table.insert(std::make_pair("int32", IntegerType(32, true, llvm::Type::getInt32Ty(context))));
    table.insert(std::make_pair("int64", std::make_unique<IntegerType>(64, true, llvm::Type::getInt64Ty(context))));

//    table.insert(std::make_pair("uint8", IntegerType(8, false, llvm::Type::getInt8Ty(context))));
//    table.insert(std::make_pair("uint16", IntegerType(16, false, llvm::Type::getInt16Ty(context))));
//    table.insert(std::make_pair("uint32", IntegerType(32, false, llvm::Type::getInt32Ty(context))));
//    table.insert(std::make_pair("uint64", IntegerType(64, false, llvm::Type::getInt64Ty(context))));
}

void createStringType(llvm::LLVMContext& context, llvm::StringMap<std::unique_ptr<Type>>& table)
{

}

void createPrimitiveTypes(llvm::LLVMContext& context, llvm::StringMap<std::unique_ptr<Type>>& table)
{
    createNumericTypes(context, table);
    createStringType(context, table);
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
        default:
            assert(false);
    }
}
