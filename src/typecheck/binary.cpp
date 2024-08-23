#include "typecheck.h"
#include "typecheck/expression.h"
#include "typecheck/unify.h"

#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

using llvm::isa;
using llvm::dyn_cast;
using llvm::TypeSwitch;

TypeConstraint *unifyConstraints(TypeConstraint *left, AST::Expression& leftChild, TypeConstraint *right, AST::Expression& rightChild) {
    switch (left->getKind()) {
        case TypeConstraintKind::Numeric:
            switch (right->getKind()) {
            case TypeConstraintKind::Numeric:
                return TypeConstraint::Numeric;
            case TypeConstraintKind::Floating:
                return TypeConstraint::Floating;
            case TypeConstraintKind::Optional:
                Diagnostic::error(rightChild, "'nil' literal requires contextual type.");
                return nullptr;
            }
            break;
        case TypeConstraintKind::Floating:
            switch (right->getKind()) {
            case TypeConstraintKind::Numeric:
            case TypeConstraintKind::Floating:
                return TypeConstraint::Floating;
            case TypeConstraintKind::Optional:
                Diagnostic::error(rightChild, "'nil' literal requires contextual type.");
                return nullptr;
            }
        case TypeConstraintKind::Optional:
            Diagnostic::error(leftChild, "'nil' literal requires contextual type.");
            return nullptr;
    }
}

Type *ExpressionTypeChecker::defaultTypeFromTypeConstraints(TypeConstraint *left, AST::Expression& leftChild, TypeConstraint *right, AST::Expression& rightChild) {
    if (auto unified = unifyConstraints(left, leftChild, right, rightChild)) {
        switch (unified->getKind()) {
            case TypeConstraintKind::Numeric:
                return typeResolver.defaultIntegerType();
            case TypeConstraintKind::Floating:
                return typeResolver.defaultFPType();
            case TypeConstraintKind::Optional:
                Diagnostic::error(leftChild, "'nil' literal requires contextual type.");
                return nullptr;
        }
    } else {
        return {};
    }
}

std::pair<TypeResult, TypeResult> ExpressionTypeChecker::typeCheckBinaryOperands(AST::Expression& leftChild, AST::Expression& rightChild, Type *propagatedType) {
    auto left = typeCheckExpression(leftChild);
    auto right = typeCheckExpression(rightChild);

    if (!left || !right) {
        return {left, right};
    }

    if (left.isConstraint()) {
        if (right.isConstraint()) {
            if (propagatedType) {
                left = typeCheckExpression(leftChild, propagatedType);
                right = typeCheckExpression(rightChild, propagatedType);
            }
            return {left, right};
        } else {
            left = typeCheckExpression(leftChild, right.asType());
            return {left, right};
        }
    } else {
        if (right.isConstraint()) {
            right = typeCheckExpression(rightChild, left.asType());
            return {left, right};
        } else {
            return {left, right};
        }
    }
}

Type *ExpressionTypeChecker::typeCheckLogicalOperator(AST::BinaryExpression& binary) {
    auto boolean = typeResolver.booleanType();
    auto left = typeCheckExpression(binary.getLeft(), boolean);
    auto right = typeCheckExpression(binary.getRight(), boolean);

    
    if (left != boolean) {
        Diagnostic::error(binary.getLeft(), "Expected boolean in logical operator.");
        return nullptr;
    } else if (right != boolean) {
        Diagnostic::error(binary.getLeft(), "Expected boolean in logical operator.");
        return nullptr;
    }
    return boolean;
}

bool checkEquatability(Type *type) {
    return TypeSwitch<Type *, bool>(type)
        .Case([](IntegerType *_) {
            return true;
        })
        .Case([](FPType *_) {
            return true;
        })
        .Case([](BooleanType *_) {
            return true;
        })
        .Case([](OptionalType *optionalType) {
            return checkEquatability(optionalType->getContained());
        })
        .Default([](Type *type) {
            return false;
        });
}

Type *ExpressionTypeChecker::typeCheckEquality(AST::BinaryExpression& binary) {
    auto [left, right] = typeCheckBinaryOperands(binary.getLeft(), binary.getRight(), nullptr);

    if (!left || !right) {
        return {};
    }

    if (left.isConstraint()) {
        Diagnostic::error(binary.getLeft(), "Unable to determine type of expression.");
        return {};
    } else if (right.isConstraint()) {
        Diagnostic::error(binary.getLeft(), "Unable to determine type of expression.");
        return {};
    } else {
        Type *leftType = left;
        Type *rightType = right;

        auto boolean = typeResolver.booleanType();
       
        Type *unified;

        if (leftType == rightType) {
            unified = leftType;
        } else {
            unified = unifyTypesForEquality(binary, *leftType, *rightType);
            if (!unified) {
                return nullptr;
            }
        }
        if (checkEquatability(unified)) {
            return boolean;
        } else {
            Diagnostic::error(binary, "Cannot test instances of " + unified->makeName() + " for equality.");
            return nullptr;
        }
    }
}

bool checkComparability(Type *type) {
    return TypeSwitch<Type *, bool>(type)
        .Case([](IntegerType *_) {
            return true;
        })
        .Case([](FPType *_) {
            return true;
        })
        .Case([](BooleanType *_) {
            return true;
        })
        .Case([](OptionalType *optionalType) {
            return false;
        })
        .Default([](Type *type) {
            return false;
        });
}

Type *ExpressionTypeChecker::typeCheckComparison(AST::BinaryExpression& binary) {
    auto [left, right] = typeCheckBinaryOperands(binary.getLeft(), binary.getRight(), nullptr);

    if (!left || !right) {
        return {};
    }

    if (left.isConstraint()) {
        Diagnostic::error(binary.getLeft(), "Unable to determine type of expression.");
        return {};
    } else if (right.isConstraint()) {
        Diagnostic::error(binary.getRight(), "Unable to determine type of expression.");
        return {};
    } else {
        Type *leftType = left;
        Type *rightType = right;

        auto boolean = typeResolver.booleanType();

        Type *unified;
       
        if (leftType == rightType) {
            unified = leftType;
        } else {
            unified = unifyTypesForComparison(binary, *leftType, *rightType);
            if (!unified) {
                return nullptr;
            }
        }
        if (checkComparability(unified)) {
            return boolean;
        } else {
            Diagnostic::error(binary, "Cannot compare instances of type " + unified->makeName() + ".");
            return nullptr;
        }
    }
}
    
Type *ExpressionTypeChecker::typeCheckRangeOperator(AST::BinaryExpression& binary) {
    auto [left, right] = typeCheckBinaryOperands(binary.getLeft(), binary.getRight(), nullptr);

    if (!left || !right) {
        return {};
    }

    auto handleTypes = [&](Type *leftType, Type *rightType) -> Type * {
        if (leftType == rightType) {
            if (auto integerType = dyn_cast<IntegerType>(leftType)) {
                if (binary.getOp() == AST::BinaryOperator::OpenRange) {
                    return integerType->getOpenRangeType();
                } else if (binary.getOp() == AST::BinaryOperator::ClosedRange) {
                    return integerType->getClosedRangeType();
                }
                llvm_unreachable("Unknown range type op.");
            } else {
                Diagnostic::error(binary, "Cannot create range with non-integer type.");
                return {};
            }
        } else {
            Diagnostic::error(binary, "TODO: Implement binary unification in range typecheck.");
            return {};
        }
    };

    if (left.isConstraint() && right.isConstraint()) {
        auto type = defaultTypeFromTypeConstraints(left, binary.getLeft(), right, binary.getRight());

        if (!type) {
            return {};
        }

        Type *leftType;
        if (!(leftType = typeCheckToTypeOrError(binary.getLeft(), type))) {
            return {};
        }
        Type *rightType;
        if (!(rightType = typeCheckToTypeOrError(binary.getRight(), type))) {
            return {};
        }

        return handleTypes(leftType, rightType);
    } else if (left.isType() && right.isType()) {
        return handleTypes(left, right);
    } else {
        if (left.isConstraint()) {
            Diagnostic::error(binary.getLeft(), "Unable to determine type of expression.");
            return {};
        } else {
            Diagnostic::error(binary.getRight(), "Unable to determine type of expression.");
            return {};
        }
    }
}

TypeResult ExpressionTypeChecker::typeCheckBitwise(AST::BinaryExpression& binary, Type *propagatedType) {
    auto [left, right] = typeCheckBinaryOperands(binary.getLeft(), binary.getRight(), propagatedType);
    
    if (left.isConstraint() && right.isConstraint()) {
        return unifyConstraints(left, binary.getLeft(), right, binary.getRight());
    } else if (left.isType() && right.isType()) {
        Type *leftType = left;
        Type *rightType = right;
            
        Type *unified;
        if (leftType == rightType) {
            unified = leftType;
        } else {
            unified = unifyTypesForBitwiseArithmetic(binary, *leftType, *rightType, propagatedType);
            if (!unified) {
                return {};
            }
        }

        return TypeSwitch<Type *, Type *>(unified)
            .Case([&](IntegerType *integerType) -> Type * {
                return integerType;
            })
            .Case([&](BooleanType *boolean) -> Type * {
                return boolean;
            })
            .Default([&](Type *type) -> Type * {
                Diagnostic::error(binary, "Cannot apply bitwise operator to operands of type " + type->makeName());
                return nullptr;
            });
    } else {
        if (left.isConstraint()) {
            Diagnostic::error(binary.getLeft(), "Unable to determine type of expression.");
        } else {
            Diagnostic::error(binary.getRight(), "Unable to determine type of expression.");
        }
        return {};
    }
}

TypeResult ExpressionTypeChecker::typeCheckShift(AST::BinaryExpression& binary, Type *propagatedType) {
    // TODO: We may not want to have shift operands apply any "type pressure" to one another,
    // as we allow any integer type combination in shifts.
    // We may want to at least block the right operand determining the type of the left.
    auto [left, right] = typeCheckBinaryOperands(binary.getLeft(), binary.getRight(), propagatedType);

    if (left.isConstraint() && right.isConstraint()) {
        return unifyConstraints(left, binary.getLeft(), right, binary.getRight());
    } else if (left.isType() && right.isType()) {
        if (!isa<IntegerType>(left.asType())) {
            Diagnostic::error(binary.getLeft(), "Shift operand must be an integer type.");
            return {};
        }
        if (!isa<IntegerType>(right.asType())) {
            Diagnostic::error(binary.getLeft(), "Shift amount must be an integer type.");
            return {};
        }
        return left;
    } else {
        if (left.isConstraint()) {
            Diagnostic::error(binary.getLeft(), "Unable to determine type of expression.");
        } else {
            Diagnostic::error(binary.getRight(), "Unable to determine type of expression.");
        }
        return {};
    }
}

TypeResult ExpressionTypeChecker::typeCheckArithmetic(AST::BinaryExpression& binary, Type *propagatedType) {
    auto [left, right] = typeCheckBinaryOperands(binary.getLeft(), binary.getRight(), propagatedType);

    if (!left || !right) {
        return {};
    }

    if (left.isConstraint() && right.isConstraint()) {
        return unifyConstraints(left, binary.getLeft(), right, binary.getRight());
    } else if (left.isType() && right.isType()) {
        Type *leftType = left;
        Type *rightType = right;

        if (leftType == rightType) {
            return leftType;
        } else {
            return unifyTypesForArithmetic(binary, *leftType, *rightType);
        }
    } else {
        if (left.isConstraint()) {
            Diagnostic::error(binary.getLeft(), "Unable to determine type of expression.");
        } else {
            Diagnostic::error(binary.getRight(), "Unable to determine type of expression.");
        }
        return {};
    }
}
