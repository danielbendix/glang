#include "typecheck/expression.h"
#include "typecheck/coerce.h"
#include "type.h"
#include "type/struct.h"
#include "type/enum.h"

#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

#include <format>

using llvm::isa;
using llvm::dyn_cast;
using enum PassResultKind;

TypeResult ExpressionTypeChecker::visitUnaryExpression(AST::UnaryExpression& unary, Type *propagatedType) {
    Type *type;
    using enum AST::UnaryOperator;
    switch (unary.getOp()) {
        case Not: {
            type = typeCheckBooleanNegationOperator(unary);
            break;
        }
        case Negate: {
            TypeResult target = typeCheckNegationOperator(unary, propagatedType);
            if (target.isConstraint()) {
                return target.asConstraint();
            }
            type = target.type();
            break;
        }
        case BitwiseNegate: {
            TypeResult target = typeCheckBitwiseNegationOperator(unary, propagatedType);
            if (target.isConstraint()) {
                return target.asConstraint();
            }
            type = target.type();
            break;
        }
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
        case AST::UnaryOperator::ZeroExtend:
        case AST::UnaryOperator::SignExtend:
        case AST::UnaryOperator::IntegerToFP:
        case AST::UnaryOperator::FPExtend:
        case AST::UnaryOperator::OptionalWrap:
            llvm_unreachable("Synthetic operations should not be occurring during type checking.");
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

    if (left.isConstraint()) {
        if (right.isConstraint()) {
            // Nothing we can do right now.
        } else {
            left = typeCheckExpression(binary.getLeft(), right.asType());
        }
    } else {
        if (right.isConstraint()) {
            right = typeCheckExpression(binary.getRight(), left.asType());
        } else {
            // Both are types, and should be unifiable.
        }
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
        case ShiftLeft:
        case ShiftRight:

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
    TypeResult target;
    if (declaredType && llvm::isa<AST::InferredMemberAccessExpression>(&call.getTarget())) {
        target = typeCheckExpression(call.getTarget(), declaredType);
    } else {
        target = typeCheckExpression(call.getTarget());
    }

    if (!target) {
        return {};
    }

    if (target.isConstraint()) {
        Diagnostic::error(call, "Attempting to call value with unbound type.");
        return {};
    }

    Type *type = target.asType();

    if (FunctionType *functionType = dyn_cast<FunctionType>(type)) {
        if (functionType->parameterCount() != call.argumentCount()) {
            Diagnostic::error(call, std::format("Wrong number of arguments in call. Expected {}, got {}", functionType->parameterCount(), call.argumentCount()));
            return {};
        }

        // This needs to be whether one type can be converted to the other.
        Result parameterResult = OK;
        for (int i = 0; i < functionType->parameterCount(); ++i) {
            AST::Expression& argument = call.getArgument(i);
            Type *parameterType = functionType->getParameter(i);
            TypeResult argumentResult = typeCheckExpression(argument, parameterType);

            if (argumentResult.isConstraint()) {
                parameterResult |= ERROR;
                Diagnostic::error(argument, "Unable to determine type of parameter.");
                continue;
            }

            Type *argumentType = argumentResult.type();

            if (argumentType != parameterType) {
                auto [coerceResult, wrapped] = coerceType(*parameterType, *argumentType, argument);

                if (wrapped) {
                    call.setWrappedArgument(i, std::move(wrapped));
                }
                parameterResult |= coerceResult;
                continue;

                // TODO: Printable types.
                Diagnostic::error(argument, std::format("Wrong argument type in call. Expected {}, got {}", int(parameterType->getKind()), int(argumentType->getKind())));
                return {};
            }
        }

        Type *type = functionType->getReturnType();
        call.setType(type);
        if (parameterResult.failed()) {
            return {};
        } else {
            return type;
        }
    } else {
        Diagnostic::error(call, "Attempting to call non-function value.");
        return {};
    }
}

TypeResult ExpressionTypeChecker::visitSubscriptExpression(AST::SubscriptExpression& subscript, Type *declaredType) {
    assert(false);
}

TypeResult ExpressionTypeChecker::visitInitializerExpression(AST::InitializerExpression& initializer, Type *declaredType) {
    Type *resolvedType = nullptr;
    if (auto identifier = initializer.getIdentifier()) {
        resolvedType = typeResolver.resolveType(*identifier);

        if (!resolvedType) {
            return {};
        }
    }

    Type *initializingType = nullptr;
    if (resolvedType) {
        initializingType = resolvedType;
    } else if (declaredType) {
        initializingType = declaredType->removeImplicitWrapperTypes();
    } else {
        Diagnostic::error(initializer, "No type available to infer initializer type from.");
        return {};
    }

    if (auto structType = dyn_cast<StructType>(initializingType)) {
        initializer.setType(structType);
        // TODO: Use a set of fields to ensure that all values are being set.
        // TODO: Use a set of fields to ensure no duplicates.
        Result result = OK;
        for (size_t i = 0; i < initializer.getNumberOfPairs(); ++i) {
            auto& pair = initializer.getPair(i);

            auto [resolution, fieldType] = structType->resolveMember(pair.first->getMemberName());
            if (!resolution) {
                Diagnostic::error(*pair.first, "Unable to resolve member in initializer expression.");
                return {};
            }
            auto resolutionPtr = resolution.get();
            pair.first->setResolution(std::move(resolution));
            if (!isa<StructFieldResolution>(resolutionPtr)) {
                Diagnostic::error(*pair.first, "Cannot assign to [GET FIELD TYPE] in struct initializer expression.");
                return {};
            }

            auto valueType = typeCheckExpression(*pair.second, fieldType);
            if (!valueType) {
                return {};
            } else if (valueType.isConstraint()) {
                assert(false);
            }

            auto [coerceResult, wrapped] = coerceType(*fieldType, *valueType.asType(), *pair.second);
        
            if (wrapped) {
                std::ignore = pair.second.release();
                pair.second = std::move(wrapped);
            }

            result |= coerceResult;
        }
        if (result.failed()) {
            return {};
        } else {
            return structType;
        }
    } else {
        Diagnostic::error(initializer, "Initializer expression is not supported for [INSERT TYPE] types.");
        return {};
    }
}

TypeResult ExpressionTypeChecker::visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess, Type *declaredType) {
    TypeResult target = typeCheckExpression(memberAccess.getTarget());
    
    if (!target) {
        return {};
    }

    if (target.isConstraint()) {
        Diagnostic::error(memberAccess, "Attempting to access member on value with unbound type.");
        return {};
    }

    Type *type = target.asType();

    if (auto structType = llvm::dyn_cast_if_present<StructType>(type)) {
        auto [memberResolution, memberType] = structType->resolveMember(memberAccess.getMemberName());
        if (memberResolution) {
            memberAccess.setType(memberType);
            memberAccess.setResolution(std::move(memberResolution));
            return {memberType, target.canAssign()};
        } else {
            Diagnostic::error(memberAccess, "Unable to resolve struct member");
            return {};
        }
    }

    Diagnostic::error(memberAccess, "Type does not support member access.");
    return {};
}

TypeResult ExpressionTypeChecker::visitInferredMemberAccessExpression(AST::InferredMemberAccessExpression &inferredMemberAccess, Type *declaredType) {
    if (!declaredType) {
        Diagnostic::error(inferredMemberAccess, "Unable to infer expected type of implicit member access expression.");
        return {};
    }

    std::pair<unique_ptr_t<MemberResolution>, Type *> resolution;
    llvm::TypeSwitch<Type *, void>(declaredType)
        .Case<EnumType>([&](auto enumType) {
            resolution = enumType->resolveStaticMember(inferredMemberAccess.getMemberName());
        })
        .Case<StructType>([&](auto structType) {
            resolution = structType->resolveStaticMember(inferredMemberAccess.getMemberName());
        })
        .Default([](auto type) {
        })
    ;

    if (resolution.first) {
        inferredMemberAccess.setResolution(std::move(resolution.first));
        inferredMemberAccess.setType(resolution.second);
        return resolution.second;
    }

    Diagnostic::error(inferredMemberAccess, "Type checking of implicit member access is not yet implemented.");
    return {};
}

TypeResult ExpressionTypeChecker::visitLiteral(AST::Literal& literal, Type *propagatedType) {
    using enum AST::Literal::Type;
    // TODO: Validate against declaration type.
    switch (literal.getLiteralType()) {
        case Boolean:
            literal.setType(boolean);
            return boolean;
        case Integer:
            // FIXME: We need to distinguish between plain integer literals, and hex, octal, and binary.
            if (propagatedType) {
                if (auto integerType = dyn_cast<IntegerType>(propagatedType)) {
                    // Check if type can hold literal.
                    literal.setType(propagatedType);
                    return propagatedType;
                } else if (auto fpType = dyn_cast<FPType>(propagatedType)) {
                    literal.setType(propagatedType);
                    return propagatedType;
                } else if (auto optionalType = dyn_cast<OptionalType>(propagatedType)) {
                    auto rootType = optionalType->removeImplicitWrapperTypes();
                    if (auto type = visitLiteral(literal, rootType); type.isType()) {
                        literal.setType(type.asType());
                        return type;
                    }
                }
                // TODO: Get numeric type
            } else {
                return TypeConstraint::Numeric;
            }
            // FIXME: Check declared integer type
            // Can be converted to smaller integer types or a floating point value.
            literal.setType(signed64);
            return signed64;
        case Double:
            if (propagatedType) {
                if (auto fpType = dyn_cast<FPType>(propagatedType)) {
                    literal.setType(propagatedType);
                    return propagatedType;
                }
            } else {
                return TypeConstraint::Floating;
            }
            // TODO: Add default type for floating point.
            Diagnostic::error(literal, "Unbound loating-point literals are currently not supported.");
            return {};
            break;
        case String:
            Diagnostic::error(literal, "String literals are currently not supported.");
            return {};
            break;
        case Nil:
            if (propagatedType) {
                if (isa<OptionalType>(propagatedType)) {
                    literal.setType(propagatedType);
                    return propagatedType;
                } else {
                    // Perhaps this should just also return the type constraint.
                    // Add expected type to diagnostic.
                    Diagnostic::error(literal, "Unable to infer type of nil literal.");
                    return {};
                }
            } else {
                return TypeConstraint::Optional;
            }
    }
}

TypeResult ExpressionTypeChecker::visitIdentifier(AST::Identifier& identifier, Type *declaredType) {
    auto *resolution = identifier.getResolution();
    TypeResult result = llvm::TypeSwitch<IdentifierResolution *, TypeResult>(resolution)
        .Case<LocalResolution>([](auto local) {
            auto& binding = local->getBinding();
            return TypeResult{binding.getType(), binding.getIsMutable()};
        })
        .Case<FunctionResolution>([](auto functionResolution) {
            AST::FunctionDeclaration *function = functionResolution->getFunctionDeclaration();
            return function->getType();
        })
        .Case<FunctionParameterResolution>([](auto parameter) {
            return parameter->getFunctionDeclaration()->getParameter(parameter->getParameterIndex()).type;
        })
    ;
    identifier.setType(result.asType());
    return result;
}
