#include "typecheck.h"
#include "typecheck/expression.h"

#include "llvm/Support/Casting.h"

using llvm::dyn_cast;

Type *ExpressionTypeChecker::typeCheckBooleanNegationOperator(AST::UnaryExpression& unary) {
    auto target = typeCheckExpression(unary.getTarget(), boolean);
    if (!target) {
        return {};
    }

    if (target.type == boolean) {
        return boolean;
    }

    Diagnostic::error(unary, "Cannot apply 'not' operator to non-boolean value");

    return {};
}

Type *ExpressionTypeChecker::typeCheckNegationOperator(AST::UnaryExpression& unary, Type *propagatedType) {
    auto target = typeCheckExpression(unary, propagatedType);
    if (!target) {
        return {};
    }
    if (auto numericType = dyn_cast<NumericType>(target.type)) {
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

    if (!target.canAssign) {
        // TODO: Diagnose invalid assignment target with AddressOf
        Diagnostic::error(unary.getTarget(), "Cannot get address of r-value.");
        return {};
    }

    return target.type->getPointerType();
}

Type *ExpressionTypeChecker::typeCheckDereferenceOperator(AST::UnaryExpression& unary) {
    auto target = typeCheckExpression(unary.getTarget());

    if (!target) {
        return {};
    }

    if (auto pointerType = dyn_cast<PointerType>(target.type)) {
        return pointerType->getPointeeType();
    }

    if (auto optionalType = dyn_cast<OptionalType>(target.type)) {
        if (auto optionalPointer = dyn_cast<OptionalType>(optionalType->getContained())) {
            Diagnostic::error(unary, "Cannot dereference optional pointer value. Pointer should be unwrapped first.");
            return {};
        }
    }

    Diagnostic::error(unary, "Cannot dereference non-pointer type.");

    return {};

    


}
