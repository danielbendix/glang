#include "typecheck/expression.h"
#include "type.h"
#include "type/struct.h"
#include "type/enum.h"

#include "sema/fold.h"

#include "containers/bitmap.h"

#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

#include <format>

using llvm::dyn_cast;

// TODO: This needs to be refactored a bit in terms of where diagnostics are emitted, and their text needs to be more specific to the operation being checked.

Type *typeCheckExpressionOrError(AST::Expression& expression, ExpressionTypeChecker& typeChecker) {
    auto result = typeChecker.typeCheckExpression(expression);

    if (!result) {
        return nullptr;
    }

    if (result.isConstraint()) {
        Diagnostic::error(expression, "Unable to determine type of expression.");
        return nullptr;
    }

    return result.asType();
}

LValueTypeResult ExpressionLValueTypeChecker::visitUnaryExpression(AST::UnaryExpression& unary, Type *propagatedType) {
    switch (unary.getOp()) {
        case AST::UnaryOperator::ForceUnwrap: {

        }
        case AST::UnaryOperator::PrefixDereference:
        case AST::UnaryOperator::PostfixDereference: {
            ExpressionTypeChecker rvalueChecker{scopeManager, typeResolver};
            Type *type = typeCheckExpressionOrError(unary.getTarget(), rvalueChecker);
            if (!type) {
                return {};
            }
            if (auto pointerType = dyn_cast<PointerType>(type)) {
                auto type = pointerType->getPointeeType();
                unary.setType(type);
                return {type, true};
            } else {
                Diagnostic::error(unary, "Cannot dereference non-pointer type.");
                return {};
            }
        }
        case AST::UnaryOperator::Negate:
        case AST::UnaryOperator::BitwiseNegate:
        case AST::UnaryOperator::Not:
        case AST::UnaryOperator::AddressOf:
        case AST::UnaryOperator::ZeroExtend:
        case AST::UnaryOperator::SignExtend:
        case AST::UnaryOperator::IntegerToFP:
        case AST::UnaryOperator::FPExtend:
        case AST::UnaryOperator::OptionalWrap:
            break;

    }
    Diagnostic::error(unary, "Cannot assign to result of expression.");
    return {};
}

LValueTypeResult ExpressionLValueTypeChecker::visitBinaryExpression(AST::BinaryExpression& binary, Type *declaredType) {
    Diagnostic::error(binary, "Cannot assign to result of expression.");
    return {};
}

LValueTypeResult ExpressionLValueTypeChecker::visitIntrinsicExpression(AST::IntrinsicExpression& intrinsic, Type *declaredType) {
    // This may be possible with some intrinsics later.
    Diagnostic::error(intrinsic, "Cannot assign to result of intrinsic.");
    return {};
}

LValueTypeResult ExpressionLValueTypeChecker::visitCallExpression(AST::CallExpression& call, Type *declaredType) {
    Diagnostic::error(call, "Cannot assign to result of function call.");
    return {};
}

LValueTypeResult ExpressionLValueTypeChecker::visitSubscriptExpression(AST::SubscriptExpression& subscript, Type *declaredType) {
    ExpressionTypeChecker rvalueChecker{scopeManager, typeResolver};
    // Array types cannot be const right now. So we check target as an r-value.
    Type *targetType = typeCheckExpressionOrError(subscript.getTarget(), rvalueChecker);
    if (!targetType) {
        return {};
    }

    auto arrayType = dyn_cast<ArrayType>(targetType);

    if (!arrayType) {
        Diagnostic::error(subscript, "Cannot apply subscript to non-array type.");
        return {};
    }

    // FIXME: This should be a default unsigned/index type, so we get free checking of negative literals.
    Type *indexType = rvalueChecker.typeCheckExpressionUsingDeclaredOrDefaultType(subscript.getIndex(), typeResolver.defaultIntegerType());
    
    auto integerType = dyn_cast<IntegerType>(indexType);

    if (!integerType) {
        Diagnostic::error(subscript, "Cannot use non-integer type as array index.");
        return {};
    }

    auto elementType = arrayType->getContained();
    subscript.setType(elementType);

    // TODO: When arrays can be be const, this needs to reflect that.
    return {elementType, true};
}

LValueTypeResult ExpressionLValueTypeChecker::visitInitializerExpression(AST::InitializerExpression& initializer, Type *declaredType) {
    Diagnostic::error(initializer, "Cannot assign to struct r-value.");
    return {};
}

LValueTypeResult ExpressionLValueTypeChecker::visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess, Type *declaredType) {
    auto result = typeCheckExpression(memberAccess.getTarget());
    
    if (!result) {
        return {};
    }

    ExpressionTypeChecker rvalueChecker{scopeManager, typeResolver};

    LValueTypeResult target = typeCheckExpression(memberAccess.getTarget());
    
    if (!target) {
        return {};
    }

    Type *type = target.type;

    if (auto structType = llvm::dyn_cast_if_present<StructType>(type)) {
        auto [memberResolution, memberType] = structType->resolveMember(memberAccess.getMemberName());
        if (memberResolution) {
            memberAccess.setType(memberType.getPointer());
            memberAccess.setResolution(memberResolution);
            if (!memberType.getInt()) {
                Diagnostic::error(memberAccess, "Cannot assign to readonly member.");
                return {};
            } else {
                return {memberType.getPointer(), target.isLValue && memberType.getInt()};
            }
        } else {
            Diagnostic::error(memberAccess, "Unable to resolve struct member");
            return {};
        }
    }

    Diagnostic::error(memberAccess, "Type does not support member access.");
    return {};
}

LValueTypeResult ExpressionLValueTypeChecker::visitInferredMemberAccessExpression(AST::InferredMemberAccessExpression& inferredMemberAccess, Type *declaredType) {
    Diagnostic::error(inferredMemberAccess, "Cannot infer member access in assignment.");
    return {};
}

LValueTypeResult ExpressionLValueTypeChecker::visitLiteral(AST::Literal& literal, Type *declaredType) {
    Diagnostic::error(literal, "Cannot assign to literal value.");
    return {};
}

LValueTypeResult ExpressionLValueTypeChecker::visitIdentifier(AST::Identifier& identifier, Type *declaredType) {
    bool isRead = kind == LValueKind::AddressOf || kind == LValueKind::CompoundAssignment;
    auto resolution = scopeManager.getResolution(identifier.getName(), isRead, true);

    if (resolution) {
        identifier.setResolution(resolution);
    } else {
        Diagnostic::error(identifier, "Unable to resolve identifier.");
        return {};
    }

    auto nested = [this, resolution, &identifier] () -> LValueTypeResult {
        switch (resolution.getKind()) {
            case IdentifierResolution::Kind::UNRESOLVED:
                llvm_unreachable("UNDEFINED resolution in type check.");
                break;
            case IdentifierResolution::Kind::Global: {

                auto *binding = resolution.as.global.binding;
                if (binding->hasType() || (globalHandler && (*globalHandler)(resolution.as.global.bindingIndex).ok())) {
                    return LValueTypeResult{binding->getType(), binding->getIsMutable()};
                } else {
                    return {};
                }
            }
            case IdentifierResolution::Kind::Function: {
                Diagnostic::error(identifier, "Cannot assign to function.");
                return {};
            }
            case IdentifierResolution::Kind::Parameter: {
                Diagnostic::error(identifier, "Cannot assign to function parameter.");
                return {};
            }
            case IdentifierResolution::Kind::Local: {
                auto *binding = resolution.as.local.binding;
                return {binding->getType(), binding->getIsMutable()};
            }
            case IdentifierResolution::Kind::Type: {
                // TODO: We need a metatype
                llvm_unreachable("Implement metatype.");
                break;
            }
        }
    };

    auto result = nested();
    if (result.type) {
        identifier.setType(result.type);
    }
    return result;
}
