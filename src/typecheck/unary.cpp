#include "typecheck.h"
#include "typecheck/expression.h"

#include "llvm/Support/Casting.h"

using llvm::dyn_cast;

Type *ExpressionTypeChecker::typeCheckBooleanNegationOperator(AST::UnaryExpression& unary) {
    auto target = typeCheckExpression(unary.getTarget(), boolean);
    if (!target) {
        return {};
    }
    if (target.isConstraint()) {
        Diagnostic::error(unary, "'not' operator expects a boolean value.");
        return {};
    }
    if (target.asType() == boolean) {
        return boolean;
    }

    Diagnostic::error(unary, "Cannot apply 'not' operator to non-boolean value");
    return {};
}

TypeResult ExpressionTypeChecker::typeCheckNegationOperator(AST::UnaryExpression& unary, Type *propagatedType) {
    auto target = typeCheckExpression(unary, propagatedType);
    if (!target) {
        return {};
    }
    if (target.isConstraint()) {
        auto constraint = target.asConstraint();
        if (isNumericConstraint(constraint)) {
            return constraint;
        } else {
            Diagnostic::error(unary, "Cannot negate non-numeric value.");
            return {};
        }
    }
    Type *targetType = target.asType();
    if (auto numericType = dyn_cast<NumericType>(targetType)) {
        return numericType;
    }
    Diagnostic::error(unary, "Cannot negate non-numeric value.");

    return {};
}

Type *getPointeeTypeOrNull(Type *type) {
    if (!type) {
        return {};
    }
    if (auto optional = dyn_cast<OptionalType>(type)) {
        if (auto pointer = dyn_cast<PointerType>(optional->getContained())) {
            return pointer->getPointeeType();
        }
    }
    if (auto pointer = dyn_cast<PointerType>(type)) {
        return pointer->getPointeeType();
    }
    return {};
}

Type *ExpressionTypeChecker::typeCheckAddressOfOperator(AST::UnaryExpression& unary, Type *propagatedType) {
    // This may not be necessary, as we should have an assignable value.
    Type *pointee = getPointeeTypeOrNull(propagatedType);

    TypeResult target;
    if (auto pointee = getPointeeTypeOrNull(propagatedType)) {
        target = typeCheckExpression(unary.getTarget(), pointee);
    } else {
        target = typeCheckExpression(unary.getTarget());
    }

    if (!target) {
        return {};
    }
    if (target.isConstraint()) {
        // NOTE: This assumes that this is an r-value. Which unbound types should be
        Diagnostic::error(unary.getTarget(), "Cannot get address of r-value. (Unbound type)");
        return {};
    }

    if (!target.canAssign()) {
        // TODO: Diagnose invalid assignment target with AddressOf
        Diagnostic::error(unary.getTarget(), "Cannot get address of r-value.");
        return {};
    }

    return target.asType()->getPointerType();
}

Type *ExpressionTypeChecker::typeCheckDereferenceOperator(AST::UnaryExpression& unary) {
    auto target = typeCheckExpression(unary.getTarget());

    if (!target) {
        return {};
    }

    if (target.isConstraint()) {
        Diagnostic::error(unary, "Cannot dereference literal.");
    }

    Type *targetType = target.asType();
    if (auto pointerType = dyn_cast<PointerType>(targetType)) {
        return pointerType->getPointeeType();
    }

    if (auto optionalType = dyn_cast<OptionalType>(targetType)) {
        if (auto optionalPointer = dyn_cast<OptionalType>(optionalType->getContained())) {
            Diagnostic::error(unary, "Cannot dereference optional pointer value. Pointer should be unwrapped first.");
            return {};
        }
    }

    Diagnostic::error(unary, "Cannot dereference non-pointer type.");

    return {};
}
