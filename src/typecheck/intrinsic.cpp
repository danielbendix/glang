#include "typecheck.h"
#include "typecheck/expression.h"
#include "typecheck/coerce.h"
#include "type/visitor.h"

using Result = PassResult;
using enum PassResultKind;

using llvm::dyn_cast;
using llvm::isa;

Type *ExpressionTypeChecker::typeCheckTruncateIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType) {
    if (!intrinsic.hasCall || intrinsic.getArguments().size() != 1) {
        Diagnostic::error(intrinsic, "#truncate intrinsic takes exactly one argument.");
        return nullptr;
    }

    Type *toType = nullptr;

    if (intrinsic.hasTypeArguments) {
        auto& typeArguments = intrinsic.getTypeArguments();
        switch (typeArguments.size()) {
            default:
                Diagnostic::error(intrinsic, "#truncate takes 0 or 1 type parameters.");
                return nullptr;
            case 1:
                toType = typeResolver.resolveType(*typeArguments[0]);
                if (!toType) {
                    return nullptr;
                }
            case 0:
                break;
        }
    }

    if (!toType) {
        if (!declaredType) {
            Diagnostic::error(intrinsic, "Unable to determine destination type in #truncate intrinsic.");
            return nullptr;
        }
        toType = declaredType;
    }

    TypeResult valueType = typeCheckExpression(*intrinsic.getArguments()[0]);

    if (!valueType) {
        return nullptr;
    } else if (valueType.isConstraint()) {
        Diagnostic::error(intrinsic, "Unable to determins source type in #truncate intrinsic.");
        return nullptr;
    }
    Type *fromType = valueType;

    IntegerType *toIntegerType;
    IntegerType *fromIntegerType;

    if (!(toIntegerType = dyn_cast<IntegerType>(toType))) {
        Diagnostic::error(intrinsic, "Destination type must be an integer type, not " + toType->makeName() + ".");
        return nullptr;
    }
    if (!(fromIntegerType = dyn_cast<IntegerType>(fromType))) {
        Diagnostic::error(intrinsic, "Destination type must be an integer type, not " + fromType->makeName() + ".");
        return nullptr;
    }

    if (toIntegerType->isSigned != fromIntegerType->isSigned) {
        std::string message;
        if (fromIntegerType->isSigned) {
            message += "Cannot truncate signed type '" + fromIntegerType->makeName() + "' to unsigned type '" + toIntegerType->makeName() + "'.";
        } else {
            message += "Cannot truncate unsigned type '" + fromIntegerType->makeName() + "' to signed type '" + toIntegerType->makeName() + "'.";
        }
        Diagnostic::error(intrinsic, std::move(message));
        return nullptr;
    }

    if (toIntegerType->bitWidth > fromIntegerType->bitWidth) {
        Diagnostic::error(
            intrinsic, 
            "Destination type '" + 
            toIntegerType->makeName() + 
            "' is larger than source type '" + 
            fromIntegerType->makeName() + 
            "'."
        );
        return nullptr;
    }

    intrinsic.setType(toIntegerType);

    return toIntegerType;
}

Type *ExpressionTypeChecker::typeCheckPrintIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType) {
    if (intrinsic.hasTypeArguments) {
        Diagnostic::error(intrinsic, "#print intrinsic takes no type arguments.");
        return nullptr;
    }

    if (!intrinsic.hasCall || intrinsic.getArguments().size() == 0) {
        Diagnostic::error(intrinsic, "#print intrinsic must have one argument.");
        return nullptr;
    }
    auto& arguments = intrinsic.getArguments();
    if (arguments.size() > 1) {
        Diagnostic::error(intrinsic, "#print intrinsic takes only one argument.");
        return nullptr;
    }

    auto argument = arguments[0];

    if (auto literal = dyn_cast<AST::Literal>(argument)) {
        // Let literals pass through untyped.
    } else {
        TypeResult argumentResult = typeCheckExpression(*argument);

        if (!argumentResult) {
            return nullptr;
        } else if (argumentResult.isConstraint()) {
            Diagnostic::error(*argument, "Unable to determine type of expression.");
            return nullptr;
        }
    }

    auto type = typeResolver.voidType();
    intrinsic.setType(type);
    return type;
}

Type *ExpressionTypeChecker::typeCheckAssertIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType) {
    if (intrinsic.hasTypeArguments) {
        Diagnostic::error(intrinsic, "#assert intrinsic takes no type arguments.");
        return nullptr;
    }

    if (!intrinsic.hasCall || intrinsic.getArguments().size() == 0) {
        Diagnostic::error(intrinsic, "#assert intrinsic must have at least one argument.");
        return nullptr;
    }
    auto& arguments = intrinsic.getArguments();

    if (arguments.size() > 2) {
        Diagnostic::error(intrinsic, "#assert intrinsic takes only one or two arguments.");
        return nullptr;
    }

    auto condition = arguments[0];

    auto conditionResult = typeCheckExpression(*condition, typeResolver.booleanType());

    if (!conditionResult || conditionResult.isConstraint()) {
        Diagnostic::error(*condition, "First argument to #assert intrinsic must be a boolean.");
        return nullptr;
    }

    if (arguments.size() == 2) {
        auto message = arguments[1];

        if (!isa<AST::StringLiteral>(message)) {
            Diagnostic::error(*message, "Second argument to #assert intrinsic must be a string literal.");
            return nullptr;
        }
    }

    return builtins.voidType;
}

bool typesSupportCast(Type& from, Type& to, AST::IntrinsicExpression& intrinsic) {
    return TypeVisitor::visit(from, overloaded {
        [&](PointerType& fromPointer) -> bool {
            return TypeVisitor::visit(to, overloaded {
                [&](PointerType& toPointer) -> bool {
                    return true;
                },
                [&](IntegerType& toInteger) -> bool {
                    Diagnostic::error(intrinsic, "Cannot cast from " + fromPointer.makeName() + " to " + toInteger.makeName() + ". Use #bitcast instead.");
                    return false;
                },
                [&](auto& to) {
                    Diagnostic::error(intrinsic, "Cannot cast from " + fromPointer.makeName() + " to " + to.makeName() + ".");
                    return false;
                }
            });
        },
        [&](auto& type) -> bool {
            Diagnostic::error(intrinsic, "Cannot use #cast to cast from " + from.makeName());
            return false;
        }
    });
}

Type *ExpressionTypeChecker::typeCheckCastIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType) {
    if (!intrinsic.hasCall || intrinsic.getArguments().size() != 1) {
        Diagnostic::error(intrinsic, "#cast intrinsic takes exactly one argument.");
        return nullptr;
    }
    auto* target = intrinsic.getArguments()[0];

    Type *destinationType = nullptr;
    if (intrinsic.hasTypeArguments) {
        auto typeArguments = intrinsic.getTypeArguments();
        switch (typeArguments.size()) {
            case 0: break;
            case 1:
                destinationType = typeResolver.resolveType(*typeArguments[0]);
                if (!destinationType) {
                    return {};
                }
                break;
            default:
                Diagnostic::error(intrinsic, "#cast intrinsic takes only a single type argument.");
                return {};
        }
    }

    if (!destinationType) {
        if (declaredType) {
            destinationType = declaredType;
        } else {
            Diagnostic::error(intrinsic, "Unable to determine destination type in #cast intrinsic.");
            return {};
        }
    }

    Type *sourceType = typeCheckExpression(*target);
    if (!sourceType) {
        Diagnostic::error(intrinsic, "Unable to determine source type in #cast intrinsic.");
        return {};
    }
    
    intrinsic.setType(destinationType);

    if (!typesSupportCast(*sourceType, *destinationType, intrinsic)) {
        return {};
    }

    return destinationType;
}

bool typeSupportsBitcast(Type& type) {
    // TODO: We may want to support bitcasting for aggregate types later on.
    switch (type.getKind()) {
        case TK_Boolean:
        case TK_Num_Integer:
        case TK_Num_FP:
        case TK_Pointer:
            return true;
        case TK_Void:
        case TK_String:
        case TK_Function:
        case TK_Array:
        case TK_Struct:
        case TK_Enum:
        case TK_Protocol:
        case TK_Optional:
        case TK_Range:
            return false;
    }
}

Type *ExpressionTypeChecker::typeCheckBitcastIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType) {
    if (!intrinsic.hasCall || intrinsic.getArguments().size() != 1) {
        Diagnostic::error(intrinsic, "#bitcast intrinsic takes exactly one argument.");
        return nullptr;
    }

    Type *toType;
    if (intrinsic.hasTypeArguments) {
        auto& typeArguments = intrinsic.getTypeArguments();
        if (typeArguments.size() > 1) {
            Diagnostic::error(intrinsic, "#bitcast intrinsic takes only a single type argument.");
            return nullptr;
        }
        toType = typeResolver.resolveType(*typeArguments[0]);
        if (!toType) {
            return nullptr;
        }
    } else {
        if (!declaredType) {
            Diagnostic::error(intrinsic, "Unable to detemine destination type in #bitcast intrinsic.");
        }
        toType = declaredType;
    }

    AST::Expression *argument = intrinsic.getArguments()[0];

    // NOTE: At this point we might want to introduce a size constraint when type checking,
    // as this would allow bitcasting e.g. an integer literal to a double or i8 x 8.

    auto argumentResult = typeCheckExpression(*argument);

    if (!argumentResult) {
        return nullptr;
    } else if (argumentResult.isConstraint()) {
        Diagnostic::error(intrinsic, "Unable to determins source type in #bitcast intrinsic.");
        return nullptr;
    }
    auto fromType = argumentResult.type();

    if (isa<PointerType>(fromType) && isa<PointerType>(toType)) {
        Diagnostic::error(intrinsic, "Cannot bitcast between pointer types. Use #cast instead.");
        return nullptr;
    }

    if (!typeSupportsBitcast(*fromType)) {
        Diagnostic::error(intrinsic, "Cannot bitcast from " + fromType->makeName());
        return nullptr;
    }

    if (!typeSupportsBitcast(*toType)) {
        Diagnostic::error(intrinsic, "Cannot bitcast to " + toType->makeName());
        return nullptr;
    }

    auto fromLayout = fromType->getLayout();
    auto toLayout = toType->getLayout();

    if (fromLayout.size() != toLayout.size()) {
        Diagnostic::error(intrinsic, "Cannot bitcast from " + fromType->makeName() + " with size " + std::to_string(fromLayout.size()) + " to " + toType->makeName() + " with size " + std::to_string(toLayout.size()) + ". ");
        return nullptr;
    }

    intrinsic.setType(toType);

    return toType;
}

Type *ExpressionTypeChecker::typeCheckAllocateIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType) {
    if (!intrinsic.hasTypeArguments || intrinsic.getTypeArguments().size() != 1) {
        Diagnostic::error(intrinsic, "#allocate intrinsic takes exactly one type argument.");
        return {};
    }

    if (!intrinsic.hasCall || intrinsic.getArguments().size() != 1) {
        Diagnostic::error(intrinsic, "#allocate intrinsic takes exactly one argument.");
        return {};
    }
    
    // TODO: Allow propagating down an array type.
    Type *type = typeResolver.resolveType(*intrinsic.getTypeArguments()[0]);
    if (!type) {
        return {};
    }

    Type *usizeType = builtins.usizeType;
    auto *countExpression = intrinsic.getArguments()[0];
    auto countTypeResult = typeCheckExpression(*countExpression, usizeType);
    if (countTypeResult.isConstraint()) {
        Diagnostic::error(*countExpression, "Unable to determine type of allocation count.");
        return {};
    }
    Type *countType = countTypeResult.asTypeOrNull();
    if (!countType) {
        return {};
    }
   
    if (!isa<IntegerType>(countType)) {
        Diagnostic::error(*countExpression, "Count must be an integer type.");
        return {};
    }
    if (countType != usizeType) {
        auto [result, wrapped] = coerceType(*usizeType, *countType, *countExpression);

        if (result.failed()) {
            return {};
        } else if (wrapped) {
            intrinsic.getArguments()[0] = wrapped;
        }
    }

    ArrayType *arrayType = type->getBoundedArrayType();
    intrinsic.setType(arrayType);

    return arrayType;
}

Type *ExpressionTypeChecker::typeCheckFreeIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType) {
    if (intrinsic.getArguments().size() != 1) {
        Diagnostic::error(intrinsic, "#free intrinsic takes exactly one argument.");
        return {};
    }
    if (intrinsic.hasTypeArguments) {
        Diagnostic::error(intrinsic, "#free intrinsic should not have type arguments.");
        return {};
    }
    
    auto *memory = intrinsic.getArguments()[0];
    auto memoryTypeResult = typeCheckExpression(*memory);
    if (memoryTypeResult.isConstraint()) {
        Diagnostic::error(*memory, "Unable to determine type of value to be freed.");
        return {};
    }
    Type *memoryType = memoryTypeResult.asTypeOrNull();
    if (!memoryType) {
        return {};
    }

    if (!(isa<ArrayType>(memoryType) || isa<PointerType>(memoryType))) {
        Diagnostic::error(*memory, "Can only free array or pointer.");
        return {};
    }
    intrinsic.setType(builtins.voidType);
    return builtins.voidType;
}

TypeResult ExpressionTypeChecker::visitIntrinsicExpression(AST::IntrinsicExpression& intrinsic, Type *declaredType) {
    auto kind = builtins.intrinsics.lookup(intrinsic.getName());

    if (!kind) {
        Diagnostic::error(intrinsic, "Unknown intrinsic #" + intrinsic.getName().string() + ".");
        return {};
    }

    intrinsic.setIntrinsic(*kind);

    using enum IntrinsicKind;

    switch (*kind) {
        case Truncate:
            return typeCheckTruncateIntrinsic(intrinsic, declaredType);
        case Print:
            return typeCheckPrintIntrinsic(intrinsic, declaredType);
        case Assert:
            return typeCheckAssertIntrinsic(intrinsic, declaredType);
        case Cast:
            return typeCheckCastIntrinsic(intrinsic, declaredType);
        case Bitcast:
            return typeCheckBitcastIntrinsic(intrinsic, declaredType);
        case Allocate:
            return typeCheckAllocateIntrinsic(intrinsic, declaredType);
        case Free:
            return typeCheckFreeIntrinsic(intrinsic, declaredType);
    }

    llvm_unreachable("[PROGAMMER ERROR]");
    Diagnostic::error(intrinsic, "TODO: Implement intrinsics.");
    return {};
}
