#include "typecheck/expression.h"
#include "typecheck/coerce.h"
#include "type.h"
#include "type/struct.h"
#include "type/enum.h"

#include "sema/fold.h"

#include "containers/bitmap.h"

#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

#include <format>

using llvm::isa;
using llvm::dyn_cast;
using enum PassResultKind;

TypeResult ExpressionTypeChecker::visitUnaryExpression(AST::UnaryExpression& unary, Type *propagatedType) {
    Type *type = nullptr;
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
        case PrefixDereference:
        case PostfixDereference: {
            type = typeCheckDereferenceOperator(unary);
            unary.setType(type);
            if (type) {
                return {type, true};
            }
            break;
        }
        case ForceUnwrap: {
            TypeResult target = typeCheckForceUnwrapOperator(unary);
            if (!target) {
                break;
            }
            unary.setType(target);
            return target;
        }
        case AST::UnaryOperator::ZeroExtend:
        case AST::UnaryOperator::SignExtend:
        case AST::UnaryOperator::IntegerToFP:
        case AST::UnaryOperator::FPExtend:
        case AST::UnaryOperator::OptionalWrap:
            llvm_unreachable("Synthetic operations should not be occurring during type checking.");
    }

    if (type) {
        unary.setType(type);
    }
    return type;
}

TypeResult ExpressionTypeChecker::visitBinaryExpression(AST::BinaryExpression& binary, Type *declaredType) {
    using enum AST::BinaryOperator;

    TypeResult result;

    switch (binary.getOp()) {
        case LogicalOr:
        case LogicalAnd:
            result = typeCheckLogicalOperator(binary);
            break;
        case Equal:
        case NotEqual:
            result = typeCheckEquality(binary);
            break;
        case Less:
        case LessEqual:
        case Greater:
        case GreaterEqual:
            result = typeCheckComparison(binary);
            break;
        case OpenRange:
        case ClosedRange:
            result = typeCheckRangeOperator(binary);
            break;
        case ShiftLeft:
        case ShiftRight:
            result = typeCheckShift(binary, declaredType);
            break;
        case BitwiseAnd:
        case BitwiseOr:
        case BitwiseXor:
            result = typeCheckBitwise(binary, declaredType);
            break;
        case Add:
        case Subtract:
        case Multiply:
        case Divide:
        case Modulo:
            result = typeCheckArithmetic(binary, declaredType);
            break;
    }

    if (result.isType()) {
        binary.setType(result);
    } 
    return result;
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
            
            if (!argumentResult) {
                parameterResult |= ERROR;
                Diagnostic::error(argument, "Unable to determine type of parameter.");
                continue;
            }
            if (argumentResult.isConstraint()) {
                parameterResult |= ERROR;
                Diagnostic::error(argument, "Unable to determine type of parameter (is constraint).");
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
    auto target = typeCheckExpression(subscript.getTarget());

    if (!target) {
        return {};
    } else if (target.isConstraint()) {
        Diagnostic::error(subscript, "Cannot determine type of subscript target.");
        return {};
    }

    auto targetType = target.asType();
    auto arrayType = dyn_cast<ArrayType>(targetType);

    if (!arrayType) {
        Diagnostic::error(subscript, "Cannot apply subscript to non-array type.");
        return {};
    }

    // FIXME: This should be a default unsigned/index type, so we get free checking of negative literals.
    auto index = typeCheckExpression(subscript.getIndex(), typeResolver.defaultIntegerType());
    
    if (!index) {
        return {};
    } else if (index.isConstraint()) {
        Diagnostic::error(subscript, "Cannot determine type of subscript index.");
        return {};
    }

    auto indexType = index.asType();
    auto integerType = dyn_cast<IntegerType>(indexType);

    if (!integerType) {
        Diagnostic::error(subscript, "Cannot use non-integer type as array index.");
        return {};
    }

    auto elementType = arrayType->getContained();
    subscript.setType(elementType);

    // TODO: When arrays can be be const, this needs to reflect that.
    return {elementType, true};

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

        Bitmap definedFields{(u32) structType->getFields().size()};

        Result result = OK;
        for (size_t i = 0; i < initializer.getNumberOfPairs(); ++i) {
            auto& pair = initializer.getPair(i);

            auto [resolution, memberType] = structType->resolveMember(pair.name->getMemberName());
            if (!resolution) {
                Diagnostic::error(*pair.name, "Unable to resolve member in initializer expression.");
                return {};
            }
            pair.name->setResolution(resolution);
            if (resolution.getKind() != MemberResolution::Kind::StructField) {
                Diagnostic::error(*pair.name, "Cannot assign to [GET FIELD TYPE] in struct initializer expression.");
                return {};
            }

            auto fieldType = memberType.getPointer();
            auto valueType = typeCheckExpression(*pair.value, fieldType);
            if (!valueType) {
                return {};
            } else if (valueType.isConstraint()) {
                Diagnostic::error(*pair.value, "Unable to determine type of expression.");
                return {};
            }

            auto [coerceResult, wrapped] = coerceType(*fieldType, *valueType.asType(), *pair.value);
        
            if (wrapped) {
                pair.value = wrapped;
            }

            auto fieldIndex = resolution.as.structField.index;
            if (!definedFields.set(fieldIndex)) {
                Diagnostic::error(*pair.name, "Duplicate definition of field in struct initializer.");
                result |= ERROR;
            }

            result |= coerceResult;
        }

        const size_t blocks = Bitmap::block_count(structType->getFields().size());
        definedFields |= {structType->getInitializedFields(), blocks};
        if (definedFields.countr_ones() < structType->getFields().size()) {
            std::vector<const Symbol *> missing;
            definedFields.iterate_zeros([&](u32 index) {
                auto& binding = llvm::cast<AST::IdentifierBinding>(structType->getFields()[index]->getBinding());
                missing.push_back(&binding.getIdentifier());
            });

            std::string names;

            bool needsSeparator = false;
            for (auto symbol : missing) {
                if (needsSeparator) {
                    names += ", ";
                } else {
                    needsSeparator = true;
                }
                names += symbol->string_view();
            }

            Diagnostic::error(initializer, "Uninitialized field(s) in initializer declaration: " + names);
            result |= ERROR;
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
            memberAccess.setType(memberType.getPointer());
            memberAccess.setResolution(memberResolution);
            return {memberType.getPointer(), target.canAssign() && memberType.getInt()};
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

    std::pair<MemberResolution, Type *> resolution;
    llvm::TypeSwitch<Type *, void>(declaredType)
        .Case<EnumType>([&](auto enumType) {
            resolution = enumType->resolveStaticMember(inferredMemberAccess.getMemberName());
        })
        .Case<StructType>([&](auto structType) {
            auto [first, second] = structType->resolveStaticMember(inferredMemberAccess.getMemberName());
            resolution = {std::move(first), second.getPointer()};
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


// TODO: Move somewhere else.
bool canHold(const AST::IntegerLiteral& literal, IntegerType& type) {
    auto isNegative = literal.getValue().isNegative();
    auto significantBits = literal.getValue().getSignificantBits();

    if (type.isSigned) {
        return significantBits <= type.bitWidth;
    } else {
        if (isNegative) {
            return false;
        } else {
            return (significantBits - 1) <= type.bitWidth;
        }
    }
}

TypeResult ExpressionTypeChecker::visitLiteral(AST::Literal& literal, Type *propagatedType) {
    // TODO: Validate against declaration type.
    
    using namespace AST;

    return AST::visitLiteral(literal, overloaded {
        [&](const NilLiteral& nil) -> TypeResult {
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
        },
        [&](const BooleanLiteral& boolean) -> TypeResult {
            literal.setType(typeResolver.booleanType());
            return typeResolver.booleanType();
        },
        [&](const IntegerLiteral& integer) -> TypeResult {
            // FIXME: We need to distinguish between plain integer literals, and hex, octal, and binary.
            if (propagatedType) {
                if (auto integerType = dyn_cast<IntegerType>(propagatedType)) {
                    auto& value = integer.getValue();
                    
                    if (!canHold(integer, *integerType)) {
                        llvm::SmallVector<char, 32> numberString;
                        integer.getValue().toStringSigned(numberString, 10);
                        Diagnostic::error(integer, "Integer value " + std::string{numberString.data(), numberString.size()} + " overflows when stored into " + integerType->makeName());
                        return {};
                    }

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
                return TypeConstraint::Numeric;
            } else {
                return TypeConstraint::Numeric;
            }
        },
        [&](const FloatingPointLiteral& floating) -> TypeResult {
            if (propagatedType) {
                if (auto fpType = dyn_cast<FPType>(propagatedType)) {
                    literal.setType(propagatedType);
                    return propagatedType;
                } else if (auto optionalType = dyn_cast<OptionalType>(propagatedType)) {
                    auto rootType = optionalType->removeImplicitWrapperTypes();
                    if (auto type = visitLiteral(literal, rootType); type.isType()) {
                        literal.setType(type.asType());
                        return type;
                    }
                }
            }
            return TypeConstraint::Floating;
        },
        [&](const CharacterLiteral& character) -> TypeResult {
            Diagnostic::error(literal, "Character literals are currently not supported.");
            return {};
        },
        [&](const StringLiteral& string) -> TypeResult {
            Diagnostic::error(literal, "String literals are currently not supported.");
            return {};
        },
    });
}

TypeResult ExpressionTypeChecker::visitIdentifier(AST::Identifier& identifier, Type *declaredType) {
    auto resolution = scopeManager.getResolution(identifier.getName());

    if (resolution) {
        identifier.setResolution(resolution);
    } else {
        Diagnostic::error(identifier, "Unable to resolve identifier.");
        return {};
    }

    auto nested = [this, resolution] () -> TypeResult {
        switch (resolution.getKind()) {
            case IdentifierResolution::Kind::UNRESOLVED:
                llvm_unreachable("UNRESOLVED resolution in type check.");
                break;
            case IdentifierResolution::Kind::Global: {
                auto *binding = resolution.as.global.binding;
                if (binding->hasType() || (globalHandler && (*globalHandler)(resolution.as.global.bindingIndex).ok())) {
                    return TypeResult{binding->getType(), binding->getIsMutable()};
                } else {
                    return {};
                }
            }
            case IdentifierResolution::Kind::Function: {
                Function *function = resolution.as.function.function;
                return function->type;
            }
            case IdentifierResolution::Kind::Parameter: {
                return resolution.as.parameter.function->type->getParameter(resolution.as.parameter.index);
            }
            case IdentifierResolution::Kind::Local: {
                auto *binding = resolution.as.local.binding;
                return TypeResult{binding->getType(), binding->getIsMutable()};
            }
            case IdentifierResolution::Kind::Type: {
                // TODO: We need a metatype
                llvm_unreachable("Implement metatype.");
                break;
            }
        }
    };

    TypeResult result = nested();
    if (auto type = result.asType()) {
        identifier.setType(type);
    }
    return result;
}
