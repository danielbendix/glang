#include "typecheck.h"
#include "typecheck/expression.h"

using Result = PassResult;
using enum PassResultKind;

using llvm::dyn_cast;

Type *ExpressionTypeChecker::typeCheckTruncateIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType) {
    if (!intrinsic.hasCall || intrinsic.getArguments().size() != 1) {
        Diagnostic::error(intrinsic, "#truncate takes exactly one argument.");
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
    }
    auto& arguments = intrinsic.getArguments();
    if (arguments.size() > 1) {
        Diagnostic::error(intrinsic, "#print intrinsic takes only one argument..");
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

TypeResult ExpressionTypeChecker::visitIntrinsicExpression(AST::IntrinsicExpression& intrinsic, Type *declaredType) {
    auto kind = builtins.intrinsics.lookup(intrinsic.getName());

    if (!kind) {
        Diagnostic::error(intrinsic, "Unknown intrinsic #" + intrinsic.getName().string() + ".");
        return {};
    }

    intrinsic.setIntrinsic(*kind);

    switch (*kind) {
        case IntrinsicKind::Truncate:
            return typeCheckTruncateIntrinsic(intrinsic, declaredType);
        case IntrinsicKind::Print:
            return typeCheckPrintIntrinsic(intrinsic, declaredType);
    }

    llvm_unreachable("[PROGAMMER ERROR]");
    Diagnostic::error(intrinsic, "TODO: Implement intrinsics.");
    return {};
}
