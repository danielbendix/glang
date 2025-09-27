#include "type.h"
#include "type/struct.h"
#include "type/enum.h"
#include "type/visitor.h"
#include "target/architecture.h"

#include "containers/string_map.h"

using llvm::dyn_cast;

using namespace TypeVisitor;

template <typename T, typename... Args>
T *create(Args&&... args) {
    auto& allocator = typeAllocator();
    return allocate(allocator, [&](void *space) {
        return new(space) T(std::forward<Args>(args)...);
    });
}

Type::TypeIndex& Type::getTypeIndex() {
    if (index) {
        return *index;
    } else {
        auto index = create<TypeIndex>();
        this->index = index;
        return *index;
    }
}

ArrayType *Type::getBoundedArrayType() {
    auto& index = getTypeIndex();

    if (index.boundedArrayType) {
        return index.boundedArrayType;
    } else {
        auto type = create<ArrayType>(*this, true);
        index.boundedArrayType = type;
        return type;
    }
}

ArrayType *Type::getUnboundedArrayType() {
    auto& index = getTypeIndex();

    if (index.unboundedArrayType) {
        return index.unboundedArrayType;
    } else {
        auto type = create<ArrayType>(*this, false);
        index.unboundedArrayType = type;
        return type;
    }
}

RangeType *IntegerType::getOpenRangeType() {
    auto& index = getTypeIndex();

    if (index.openRangeType) {
        return index.openRangeType;
    } else {
        auto type = create<RangeType>(*this, false);
        index.openRangeType = type;
        return type;
    }
}

RangeType *IntegerType::getClosedRangeType() {
    auto& index = getTypeIndex();

    if (index.closedRangeType) {
        return index.closedRangeType;
    } else {
        auto type = create<RangeType>(*this, true);
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

PointerType *Type::_getPointerType() {
    return create<PointerType>(this);
}

OptionalType *Type::_getOptionalType() {
    return create<OptionalType>(this);
}

llvm::FunctionType *FunctionType::getFunctionType(llvm::LLVMContext& context) const {
    auto llvmReturnType = returnType->getLLVMType(context);
    if (parametersSize > 0) {
        llvm::Type *parameterTypes[parametersSize];
        for (int i = 0; i < parametersSize; ++i) {
            Type *type = parameters[i];
            parameterTypes[i] = type->getLLVMType(context);
        }
        return llvm::FunctionType::get(llvmReturnType, llvm::ArrayRef(parameterTypes, parametersSize), false);
    } else {
        return llvm::FunctionType::get(llvmReturnType, false);
    }
}

Layout Type::getLayout() const {
    switch (getKind()) {
        case TK_Void:
            llvm_unreachable("Cannot store void type.");
        case TK_Boolean:
            return static_cast<const BooleanType *>(this)->layout;
        case TK_Num_Integer:
            return static_cast<const IntegerType *>(this)->layout;
        case TK_Num_FP:
            return static_cast<const FPType *>(this)->layout;
        case TK_String:
            llvm_unreachable("TODO: Implement string type.");
        case TK_Function:
            llvm_unreachable("TODO: Cannot store functions currently.");
        case TK_Struct:
            return static_cast<const StructType *>(this)->layout;
        case TK_Enum:
            llvm_unreachable("TODO: Implement enums.");
        case TK_Protocol:
            llvm_unreachable("TODO: Cannot store functions currently.");
        case TK_Pointer:
            return Architecture::current().pointer;
        case TK_Optional: {
            auto optionalType = static_cast<const OptionalType *>(this);
            if (llvm::isa<PointerType>(optionalType->getContained())) {
                return Architecture::current().pointer;
            } else {
                Layout containedLayout = optionalType->getContained()->getLayout();
                return incorporateLayoutAsField(containedLayout, {0, 1}).first;
            }
            // TODO: Do we memoize optional layout?
        }
        case TK_Array: {
            auto arrayType = static_cast<const ArrayType *>(this);
            // TODO: Precompute this.
            if (arrayType->isBounded) {

            } else {
                return Architecture::current().pointer;
            }
            llvm_unreachable("TODO: Add functionality for getting array layout.");
        }
        case TK_Range: {
            auto rangeType = static_cast<const RangeType *>(this);
            Layout boundLayout = rangeType->getBoundType()->getLayout();
            llvm_unreachable("TODO: Add functionality for getting range layout.");
        }
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
            return static_cast<const BooleanType *>(this)->_getLLVMType(context);
        case TK_Num_Integer:
            return static_cast<const IntegerType *>(this)->_getLLVMType(context);
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
            llvm_unreachable("Unknown type kind.");
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
        auto flag = llvm::IntegerType::get(context, 1);
        auto child = contained.getLLVMType(context);
        return llvm::StructType::get(flag, child);
    }
}

void getNameOfType(const Type& type, std::string& result) {
    return visit(type, [&result](auto& type) {
        type.getName(result);
    });
}

std::string Type::makeName() const {
    std::string result;
    getNameOfType(*this, result);
    return result;
}

void PointerType::getName(std::string& result) const {
    getNameOfType(pointeeType, result);
    result.push_back('*');
}

void OptionalType::getName(std::string& result) const {
    getNameOfType(contained, result);
    result += '?';
}

void ArrayType::getName(std::string& result) const {
    getNameOfType(contained, result);
    if (isBounded) {
        result += "[]";
    } else {
        result += "[!]";
    }
}

void RangeType::getName(std::string& result) const {
    if (isClosed) {
        result += "_closed_range<";
    } else {
        result += "_range<";
    }
    integerType.getName(result);
    result += '>';
}

void FunctionType::getName(std::string& result) const {
    result += "fn (";

    bool needsSeparator = false;
    for (auto *parameter : std::span(parameters, parametersSize)) {
        if (needsSeparator) {
            result += ", ";
        } else {
            needsSeparator = true;
        }

        getNameOfType(*parameter, result);
    }

    result += ") -> ";

    getNameOfType(*returnType, result);
}

void StringType::getName(std::string& result) const {
    llvm_unreachable("String type not supported.");
}
