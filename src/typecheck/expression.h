#ifndef LANG_typecheck_expression_h
#define LANG_typecheck_expression_h

#include "typecheck.h"
#include "typecheck/scope.h"
#include "typecheck/internal.h"
#include "typecheck/resolver.h"

#include "sema/fold.h"

using Result = PassResult;

struct TypeCheckResult {
    llvm::PointerIntPair<Type *, 1> _type;
    AST::Expression *_folded;

    TypeCheckResult(Type *type, AST::Expression *folded, bool canAssign) 
        : _type{type, canAssign}, _folded{folded} {}
    TypeCheckResult(std::nullptr_t, AST::Expression *folded) 
        : _type{nullptr, false}, _folded{folded} {}
    TypeCheckResult(TypeResult typeResult, AST::Expression *folded) 
        : _type{typeResult.type(), typeResult.canAssign()}, _folded{folded} {}

    Type *type() const {
        return _type.getPointer();
    }

    AST::Expression *folded() const {
        return _folded;
    }

    bool canAssign() const {
        return _type.getInt();
    }
};

class ExpressionTypeChecker : public AST::ExpressionVisitorT<ExpressionTypeChecker, TypeResult, Type *> {
public:
    using GlobalHandler = std::function<Result(AST::IdentifierBinding&)>;
private:
    ScopeManager& scopeManager;
    TypeResolver& typeResolver;
    GlobalHandler *globalHandler = nullptr;
public:
    ExpressionTypeChecker(ScopeManager& scopeManager, TypeResolver& typeResolver) 
        : scopeManager{scopeManager}, typeResolver{typeResolver} {}
    ExpressionTypeChecker(ScopeManager& scopeManager, TypeResolver& typeResolver, GlobalHandler& globalHandler) 
        : scopeManager{scopeManager}, typeResolver{typeResolver}, globalHandler{&globalHandler} {}

    TypeCheckResult typeCheckExpressionRequiringInferredType(AST::Expression *NONNULL expression) {
        if (auto folded = foldConstantsUntyped(*expression)) {
            expression = folded;
        }

        TypeResult typeResult = typeCheckExpression(*expression);
        if (typeResult && typeResult.isConstraint()) {
            Diagnostic::error(*expression, "Cannot determine type of expression.");
            return TypeCheckResult(nullptr, expression);
        }
        return TypeCheckResult(typeResult, expression);
    }

    TypeCheckResult typeCheckExpressionUsingDeclaredType(AST::Expression *NONNULL expression, Type *NONNULL declaredType) {
        if (auto folded = foldConstantsUntyped(*expression)) {
            expression = folded;
        }

        TypeResult typeResult = typeCheckExpression(*expression, declaredType);
        if (typeResult && typeResult.isConstraint()) {
            Diagnostic::error(*expression, "Cannot determine type of expression.");
            return TypeCheckResult(nullptr, expression);
        }
        return TypeCheckResult(typeResult, expression);
    }

    TypeCheckResult typeCheckExpressionUsingDeclaredOrDefaultType(AST::Expression *NONNULL expression, Type *NULLABLE declaredType) {
        if (auto folded = foldConstantsUntyped(*expression)) {
            expression = folded;
        }

        if (declaredType) {
            return TypeCheckResult(typeCheckExpression(*expression, declaredType), expression);
        } else {
            TypeResult typeResult = typeCheckExpression(*expression);
            if (typeResult && typeResult.isConstraint()) {
                Type *defaultType = typeResolver.defaultTypeFromTypeConstraint(typeResult.constraint());
                if (defaultType) {
                    if (auto typeResult= typeCheckExpression(*expression, defaultType); typeResult.isType()) {
                        return TypeCheckResult(typeResult, expression);
                    }
                }

                Diagnostic::error(*expression, "Cannot determine type of expression.");
                return TypeCheckResult(nullptr, expression);
            }
            return TypeCheckResult(typeResult, expression);
        }
    }

    Type *typeCheckExpressionUsingDeclaredOrDefaultType(AST::Expression& expression, Type *declaredType) {
        if (declaredType) {
            return typeCheckExpression(expression, declaredType);
        } else {
            TypeResult typeResult = typeCheckExpression(expression);
            if (typeResult && typeResult.isConstraint()) {
                Type *defaultType = typeResolver.defaultTypeFromTypeConstraint(typeResult.constraint());
                if (defaultType) {
                    if (auto type = typeCheckExpression(expression, defaultType)) {
                        return type;
                    }
                }

                Diagnostic::error(expression, "Cannot determine type of expression.");
                return nullptr;
            }
            return typeResult;
        }
    }

    TypeResult typeCheckExpression(AST::Expression& expression) {
        return expression.acceptVisitor(*this, {});
    }

    TypeResult typeCheckExpression(AST::Expression& expression, Type *declaredType) {
        assert(declaredType);
        return expression.acceptVisitor(*this, declaredType);
    }

    Type *typeCheckToTypeOrError(AST::Expression& expression, Type *declaredType) {
        TypeResult result = typeCheckExpression(expression, declaredType);

        if (!result) {
            return {};
        }

        if (result.isConstraint()) {
            Diagnostic::error(expression, "Unable to determine type of expression.");
            return {};
        }

        return result;
    }

    Type *typeCheckBooleanNegationOperator(AST::UnaryExpression& unary);
    TypeResult typeCheckNegationOperator(AST::UnaryExpression& unary, Type *propagatedType);
    TypeResult typeCheckBitwiseNegationOperator(AST::UnaryExpression& unary, Type *propagatedType);
    Type *typeCheckAddressOfOperator(AST::UnaryExpression& unary, Type *propagatedType);
    Type *typeCheckDereferenceOperator(AST::UnaryExpression& unary);
    TypeResult typeCheckForceUnwrapOperator(AST::UnaryExpression& unary);
    TypeResult visitUnaryExpression(AST::UnaryExpression& unary, Type *propagatedType);

    Type *defaultTypeFromTypeConstraints(TypeConstraint *left, AST::Expression& leftChild, TypeConstraint *right, AST::Expression& rightChild);
    std::pair<TypeResult, TypeResult> typeCheckBinaryOperands(AST::Expression& leftChild, AST::Expression& rightChild, Type *propagatedType);

    Type *typeCheckLogicalOperator(AST::BinaryExpression& binary);
    Type *typeCheckEquality(AST::BinaryExpression& binary);
    Type *typeCheckComparison(AST::BinaryExpression& binary);
    Type *typeCheckRangeOperator(AST::BinaryExpression& binary);
    TypeResult typeCheckBitwise(AST::BinaryExpression& binary, Type *propagatedType);
    TypeResult typeCheckShift(AST::BinaryExpression& binary, Type *propagatedType);
    TypeResult typeCheckArithmetic(AST::BinaryExpression& binary, Type *propagatedType);
    TypeResult visitBinaryExpression(AST::BinaryExpression& binary, Type *declaredType);

    Type *typeCheckTruncateIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType);
    Type *typeCheckPrintIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType);
    Type *typeCheckAssertIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType);
    Type *typeCheckBitcastIntrinsic(AST::IntrinsicExpression& intrinsic, Type *declaredType);
    TypeResult visitIntrinsicExpression(AST::IntrinsicExpression& intrinsic, Type *declaredType);

    TypeResult visitCallExpression(AST::CallExpression& call, Type *declaredType);
    TypeResult visitSubscriptExpression(AST::SubscriptExpression& subscript, Type *declaredType);

    TypeResult visitInitializerExpression(AST::InitializerExpression& initializer, Type *declaredType);

    TypeResult visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess, Type *declaredType);

    TypeResult visitInferredMemberAccessExpression(AST::InferredMemberAccessExpression& inferredMemberAccess, Type *declaredType);

    TypeResult visitLiteral(AST::Literal& literal, Type *declaredType);

    TypeResult visitIdentifier(AST::Identifier& identifier, Type *declaredType);
};

#endif // LANG_typecheck_expression_h
