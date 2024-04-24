#include "typecheck.h"
#include "typecheck/expression.h"

#include "llvm/Support/Casting.h"

using llvm::isa;
using llvm::dyn_cast;

/* TODO: We need an "unbound" type for ambiguous literals.
 *
 *
 *
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

Type *ExpressionTypeChecker::typeCheckComparison(AST::BinaryExpression& binary, Type *left, Type *right) {
    if (left->getKind() != right->getKind()) {
        Diagnostic::error(binary, "Attempting to compare different types.");
        return {};
    }
   
    // FIXME
    if (left != signed64) {
        Diagnostic::error(binary, "Attempting to compare non-integer types.");
        return {};
    } else {
        return boolean;
    }
}

Type *ExpressionTypeChecker::typeCheckArithmetic(AST::BinaryExpression& binary, Type *left, Type *right, Type *propagatedType) {
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

    unsigned leftWidth = leftInteger->getBitWidth();
    unsigned rightWidth = rightInteger->getBitWidth();

    if (leftWidth == rightWidth) {
        if (leftInteger->getIsSigned()) {
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
    
}
