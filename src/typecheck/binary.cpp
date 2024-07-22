#include "typecheck.h"
#include "typecheck/expression.h"

#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

using llvm::isa;
using llvm::dyn_cast;
using llvm::TypeSwitch;

/* TODO: We need an "unbound" type for ambiguous literals.
 */

Type *ExpressionTypeChecker::typeCheckLogicalOperator(AST::BinaryExpression& binary, Type *left, Type *right) {
    bool error = false;
    if (!isa<BooleanType>(left)) {
        Diagnostic::error(binary.getLeft(), "Non-boolean type in logical operator.");
        error = true;
    }
    if (!isa<BooleanType>(right)) {
        Diagnostic::error(binary.getRight(), "Non-boolean type in logical operator.");
        error = true;
    }
    if (error) {
        return {};
    }
    return left;
}

Type *ExpressionTypeChecker::typeCheckArithmetic(AST::BinaryExpression& binary, Type *left, Type *right, Type *propagatedType) {
    if (left == right) {
        if (isa<NumericType>(left)) {
            return left;
        } else {
            Diagnostic::error(binary.getLeft(), "Cannot apply arithmetic operator to non-numeric type.");
            return {};
        }
    }

    NumericType *leftNumeric = dyn_cast<NumericType>(left);
    if (!leftNumeric) {
        Diagnostic::error(binary.getLeft(), "Cannot apply arithmetic operator to non-numeric type.");
        return {};
    }
    NumericType *rightNumeric = dyn_cast<NumericType>(right);
    if (!rightNumeric) {
        Diagnostic::error(binary.getRight(), "Cannot apply arithmetic operator to non-numeric type.");
        return {};
    }

    if (leftNumeric == rightNumeric) {
        return leftNumeric;
    }

    llvm_unreachable("TODO: Implement safe numeric coercion.");


    // FIXME
    if (left != signed64) {
        Diagnostic::error(binary, "Attempting to perform arithmetic on non-integer types.");
        return {};
    } else {
        return signed64;
    }
}

Type *ExpressionTypeChecker::typeCheckBitwise(AST::BinaryExpression& binary, Type *left, Type *right) {
    IntegerType *leftInteger = dyn_cast<IntegerType>(left);
    if (!leftInteger) {
        Diagnostic::error(binary.getLeft(), "Cannot apply bitwise operator to non-integer type.");
        return {};
    }
    IntegerType *rightInteger = dyn_cast<IntegerType>(right);
    if (!rightInteger) {
        Diagnostic::error(binary.getRight(), "Cannot apply bitwise operator to non-integer type.");
        return {};
    }
    if (leftInteger->getIsSigned() != rightInteger->getIsSigned()) {
        // TODO: Do we do anything here?
    }

    unsigned leftWidth = leftInteger->bitWidth;
    unsigned rightWidth = rightInteger->bitWidth;

    if (leftWidth == rightWidth) {
        if (leftInteger->isSigned) {
            return rightInteger;
        } else {
            return leftInteger;
        }
    } else if (leftWidth > rightWidth) {
        return leftInteger;
    } else {
        return rightInteger;
    }
}

Type *ExpressionTypeChecker::typeCheckEquality(AST::BinaryExpression& binary, Type *left, Type *right) {
    if (left == right) {
        return TypeSwitch<Type *, Type *>(left)
            .Case([](IntegerType *_) {
                return boolean;
            })
            .Case([](FPType *_) {
                return boolean;
            })
            .Case([](BooleanType *_) {
                return boolean;
            })
            .Default([](Type *type) -> Type * {
                // TODO: Diagnostic.
                return nullptr;
            });
    }
    
    assert(false);
    
}

Type *ExpressionTypeChecker::typeCheckComparison(AST::BinaryExpression& binary, Type *left, Type *right) {
    if (left == right) {
        return TypeSwitch<Type *, Type *>(left)
            .Case([](IntegerType *_) {
                return boolean;
            })
            .Case([](FPType *_) {
                return boolean;
            })
            .Case([](BooleanType *_) {
                return boolean;
            })
            .Default([](Type *type) -> Type * {
                // TODO: Diagnostic.
                return nullptr;
            });

    }
    if (left->getKind() != right->getKind()) {
        Diagnostic::error(binary, "Attempting to compare different types.");
        return {};
    }

    // TODO: Unify types.
    
    assert(false && "TODO: Unify types.");
}

Type *ExpressionTypeChecker::typeCheckRangeOperator(AST::BinaryExpression& binary, Type *left, Type *right) {
    if (left == right) {
        if (auto integerType = dyn_cast<IntegerType>(left)) {
            if (binary.getOp() == AST::BinaryOperator::OpenRange) {
                return integerType->getOpenRangeType();
            } else if (binary.getOp() == AST::BinaryOperator::ClosedRange) {
                return integerType->getClosedRangeType();
            }
            llvm_unreachable("Unknown range type op.");
        } else {
            Diagnostic::error(binary, "Cannot create-range with non-integer types.");
            return {};
        }
    }

    if (left->getKind() != right->getKind()) {
        Diagnostic::error(binary, "Attempting to create range with different types.");
        return {};
    }

    // TODO: Unify types.
    
    assert(false && "TODO: Unify types.");
}
