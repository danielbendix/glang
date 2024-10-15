#ifndef LANG_typecheck_expression_h
#define LANG_typecheck_expression_h

#include "typecheck.h"
#include "typecheck/internal.h"
#include "typecheck/resolver.h"

extern VoidType *void_;
extern BooleanType *boolean;
extern IntegerType *signed64;

using Result = PassResult;

class ExpressionTypeChecker : public AST::ExpressionVisitorT<ExpressionTypeChecker, TypeResult, Type *> {
public:
    using GlobalHandler = std::function<Result(AST::IdentifierBinding&)>;
private:
    TypeResolver& typeResolver;
    GlobalHandler *globalHandler = nullptr;
public:
    ExpressionTypeChecker(TypeResolver& typeResolver) : typeResolver{typeResolver} {}
    ExpressionTypeChecker(TypeResolver& typeResolver, GlobalHandler& globalHandler) : typeResolver{typeResolver}, globalHandler{&globalHandler} {}

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
