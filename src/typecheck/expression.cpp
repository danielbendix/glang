#include "typecheck/expression.h"
#include "type.h"
#include "type/struct.h"

#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

using llvm::isa;
using llvm::dyn_cast;

TypeResult ExpressionTypeChecker::visitUnaryExpression(AST::UnaryExpression& unary, Type *propagatedType) {
    Type *type;
    using enum AST::UnaryOperator;
    switch (unary.getOp()) {
        case Not: {
            type = typeCheckBooleanNegationOperator(unary);
            break;
        }
        case Negate: {
            type = typeCheckNegationOperator(unary, propagatedType);
            break;
        case AddressOf:
            type = typeCheckAddressOfOperator(unary, propagatedType);
            break;
        case Dereference:
            type = typeCheckDereferenceOperator(unary);
            unary.setType(type);
            if (type) {
                return {type, true};
            }
            break;
        }
    }

    unary.setType(type);
    return type;
}

TypeResult ExpressionTypeChecker::visitBinaryExpression(AST::BinaryExpression& binary, Type *declaredType) {
    auto left = typeCheckExpression(binary.getLeft());
    auto right = typeCheckExpression(binary.getRight());

    if (!left || !right) {
        return {};
    }

    Type *type;

    using enum AST::BinaryOperator;
    switch (binary.getOp()) {
        case LogicalOr:
        case LogicalAnd:
            type = typeCheckLogicalOperator(binary, left, right);
            break;
        case Less:
        case LessEqual:
        case Greater:
        case GreaterEqual:
            type = typeCheckComparison(binary, left, right);
            break;
        case BitwiseAnd:
        case BitwiseOr:
        case BitwiseXor:
            type = typeCheckBitwise(binary, left, right);
            break;
        case Equal:
        case NotEqual:
            type = typeCheckEquality(binary, left, right);
            break;
        case Add:
        case Subtract:
        case Multiply:
        case Divide:
        case Modulo:
            type = typeCheckArithmetic(binary, left, right, declaredType);
            break;
    }

    binary.setType(type);
    return type;
}

TypeResult ExpressionTypeChecker::visitCallExpression(AST::CallExpression& call, Type *declaredType) {
    // Verify that function exists, and call matches arity.
    // Return return type of function.
    auto target = typeCheckExpression(call.getTarget());

    if (!target) {
        return {};
    }

    if (FunctionType *functionType = dyn_cast<FunctionType>(target.type)) {
        if (functionType->parameterCount() != call.argumentCount()) {
            std::cout << "Non-matching number of arguments in call.";
            return {};
        }

        // This needs to be whether one type can be converted to the other.
        for (int i = 0; i < functionType->parameterCount(); ++i) {
            Type *argumentType = typeCheckExpression(call.getArgument(i));
            Type *parameterType = functionType->getParameter(i);
            if (argumentType != parameterType) {
                std::cout << "Wrong argument type in call." << argumentType->getKind() << " " << parameterType->getKind();
                return nullptr;
            }
        }

        Type *type = functionType->getReturnType();
        call.setType(type);
        return type;
    } else {
        Diagnostic::error(call, "Attempting to call non-function value.");
        return nullptr;
    }
}

TypeResult ExpressionTypeChecker::visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess, Type *declaredType) {
    TypeResult target = typeCheckExpression(memberAccess.getTarget(), nullptr);
    
    if (!target) {
        return {};
    }

    if (auto structType = llvm::dyn_cast_if_present<StructType>(target.type)) {
        auto [memberResolution, memberType] = structType->resolveMember(memberAccess.getMemberName());
        if (memberResolution) {
            memberAccess.setType(memberType);
            memberAccess.setResolution(std::move(memberResolution));
            return memberType;
        } else {
            Diagnostic::error(memberAccess, "Unable to resolve struct member");
            return {};
        }
    }

    Diagnostic::error(memberAccess, "Type does not support member access.");

    return {};
}

TypeResult ExpressionTypeChecker::visitLiteral(AST::Literal& literal, Type *declaredType) {
    using enum AST::Literal::Type;
    // TODO: Validate against declaration type.
    switch (literal.getLiteralType()) {
        case Boolean:
            literal.setType(boolean);
            return boolean;
        case Integer:
            // FIXME: Check declared integer type
            // Can be converted to smaller integer types or a floating point value.
            literal.setType(signed64);
            return signed64;
        case Double:
            Diagnostic::error(literal, "Floating-point literals are currently not supported.");
            return {};
            break;
        case String:
            Diagnostic::error(literal, "String literals are currently not supported.");
            return {};
            break;
        case Nil:
            if (declaredType) {
                if (isa<OptionalType>(declaredType)) {
                    literal.setType(declaredType);
                    return declaredType;
                }
            }
            // TODO: Verify that the type is optional.
            Diagnostic::error(literal, "'nil' is currently not supported.");
            return {};
            break;
    }
}

TypeResult ExpressionTypeChecker::visitIdentifier(AST::Identifier& identifier, Type *declaredType) {
    auto *resolution = identifier.getResolution();
    TypeResult result = llvm::TypeSwitch<IdentifierResolution *, TypeResult>(resolution)
        .Case<LocalResolution>([](auto local) {
            auto& declaration = local->getVariableDeclaration();
            return TypeResult{declaration.getType(), declaration.getIsMutable()};
        })
        .Case<FunctionResolution>([](auto functionResolution) {
            AST::FunctionDeclaration *function = functionResolution->getFunctionDeclaration();
            return function->getType();
        })
        .Case<FunctionParameterResolution>([](auto parameter) {
            return parameter->getFunctionDeclaration()->getParameter(parameter->getParameterIndex()).type;
        })
    ;
    identifier.setType(result.type);
    return result;
}
