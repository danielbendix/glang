#include "typecheck.h"
#include "typecheck/expression.h"

#include "llvm/Support/Casting.h"

using llvm::dyn_cast;
using llvm::isa;

Type *ExpressionTypeChecker::typeCheckBooleanNegationOperator(AST::UnaryExpression& unary) {
    auto *boolean = typeResolver.booleanType();
    auto target = typeCheckExpression(unary.getTarget(), boolean);
    if (!target) {
        return {};
    }
    if (target.isMetatype()) {
        Diagnostic::error(unary, "'not' operator expects a boolean value.");
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
    auto target = unary.getTarget().acceptVisitor(*this, propagatedType);
    if (!target) {
        return {};
    }
    if (target.isMetatype()) {
        Diagnostic::error(unary, "Cannot negate type.");
        return {};
    }
    if (target.isConstraint()) {
        auto *constraint = target.asConstraint();
        if (isNumericConstraint(constraint)) {
            return target.clone();
        } else {
            Diagnostic::error(unary, "Cannot negate non-numeric value.");
            return {};
        }
    }
    Type *targetType = target.asType();
    if (auto *numericType = dyn_cast<NumericType>(targetType)) {
        return target.clone();
    }
    Diagnostic::error(unary, "Cannot negate non-numeric value.");

    return {};
}

TypeResult ExpressionTypeChecker::typeCheckBitwiseNegationOperator(AST::UnaryExpression& unary, Type *propagatedType) {
    auto target = unary.getTarget().acceptVisitor(*this, propagatedType);

    if (!target) {
        return {};
    }
    if (target.isMetatype()) {
        Diagnostic::error(unary, "Cannot bitwise negate type.");
        return {};
    }
    if (target.isConstraint()) {
        auto *constraint = target.asConstraint();
        if (isIntegralConstraint(constraint)) {
            return target.clone();
        } else {
            Diagnostic::error(unary, "Cannot bitwise negate non-integral value.");
            return {};
        }
    }
    if (isa<IntegerType>(target.asType())) {
        return target.clone();
    }
    Diagnostic::error(unary, "Cannot bitwise negate non-integral value.");

    return {};
}

Type *getPointeeTypeOrNull(Type *type) {
    if (!type) {
        return {};
    }
    if (auto *optional = dyn_cast<OptionalType>(type)) {
        if (auto *pointer = dyn_cast<PointerType>(optional->getContained())) {
            return pointer->getPointeeType();
        }
    }
    if (auto *pointer = dyn_cast<PointerType>(type)) {
        return pointer->getPointeeType();
    }
    return {};
}

Type *ExpressionTypeChecker::typeCheckAddressOfOperator(AST::UnaryExpression& unary, Type *propagatedType) {
    ExpressionLValueTypeChecker lvalueTypeChecker{LValueKind::AddressOf, scopeManager, typeResolver};

    LValueTypeResult target;
    // This may not be necessary, as we should have an assignable value.
    if (auto *pointeeType = getPointeeTypeOrNull(propagatedType)) {
        target = lvalueTypeChecker.typeCheckExpression(unary.getTarget(), pointeeType);
    } else {
        target = lvalueTypeChecker.typeCheckExpression(unary.getTarget());
    }

    if (!target) {
        return {};
    }

    return target.type->getPointerType();
}

Type *ExpressionTypeChecker::typeCheckDereferenceOperator(AST::UnaryExpression& unary) {
    auto target = typeCheckExpression(unary.getTarget());

    if (!target) {
        return {};
    }
    if (target.isMetatype()) {
        Diagnostic::error(unary, "Cannot dereference type.");
        return {};
    }
    if (target.isConstraint()) {
        Diagnostic::error(unary, "Cannot dereference literal.");
        return {};
    }

    Type *targetType = target.asType();
    if (auto *pointerType = dyn_cast<PointerType>(targetType)) {
        // TODO: This is a lot of indirection. We might want to compare to a cached pointer to the right type.
        if (pointerType->getPointeeType()->isVoid()) {
            Diagnostic::error(unary, "Cannot dereference void pointer value. Cast to a concrete type before derferencing.");
            return {};
        } else {
            return pointerType->getPointeeType();
        }
    }

    if (auto *optionalType = dyn_cast<OptionalType>(targetType)) {
        if (auto *optionalPointer = dyn_cast<OptionalType>(optionalType->getContained())) {
            Diagnostic::error(unary, "Cannot dereference optional pointer value. Pointer must be unwrapped first.");
            return {};
        }
    }

    Diagnostic::error(unary, "Cannot dereference non-pointer type.");

    return {};
}

// TODO: This could potentially just return a `Type *` if we keep everything with assignment in 
// the lvalue type checker.
TypeResult ExpressionTypeChecker::typeCheckForceUnwrapOperator(AST::UnaryExpression& unary) {
    if (isa<AST::NilLiteral>(unary.getTarget())) {
        Diagnostic::error(unary, "Cannot force unwrap nil literal.");
        return {};
    }

    TypeResult target = typeCheckExpression(unary.getTarget());

    if (!target) {
        return {};
    }
    if (target.isMetatype()) {
        Diagnostic::error(unary, "Unable to force unwrap type.");
        return {};
    }
    if (target.isConstraint()) {
        Diagnostic::error(unary, "Unable to determine type of force unwrap target.");
        return {};
    }

    if (auto *optionalType = dyn_cast<OptionalType>(target.asType())) {
        return TypeResult::type(optionalType->getContained());
    } else {
        Diagnostic::error(unary, "Cannot force unwrap non-optional value.");
        return {};
    }
}
