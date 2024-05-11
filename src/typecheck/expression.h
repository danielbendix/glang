#ifndef LANG_typecheck_expression_h
#define LANG_typecheck_expression_h

#include "typecheck.h"
#include "typecheck/internal.h"

extern VoidType *void_;
extern BooleanType *boolean;
extern IntegerType *signed64;

class ExpressionTypeChecker : public AST::ExpressionVisitorT<ExpressionTypeChecker, TypeResult, Type *> {

public:
    ExpressionTypeChecker() {}

    TypeResult typeCheckExpression(AST::Expression& expression) {
        return expression.acceptVisitor(*this, {});
    }

    TypeResult typeCheckExpression(AST::Expression& expression, Type *declaredType) {
        assert(declaredType);
        return expression.acceptVisitor(*this, declaredType);
    }

    Type *typeCheckBooleanNegationOperator(AST::UnaryExpression& unary);
    TypeResult typeCheckNegationOperator(AST::UnaryExpression& unary, Type *propagatedType);
    Type *typeCheckAddressOfOperator(AST::UnaryExpression& unary, Type *propagatedType);
    Type *typeCheckDereferenceOperator(AST::UnaryExpression& unary);
    TypeResult visitUnaryExpression(AST::UnaryExpression& unary, Type *propagatedType);

    Type *typeCheckLogicalOperator(AST::BinaryExpression& binary, Type *left, Type *right);
    Type *typeCheckComparison(AST::BinaryExpression& binary, Type *left, Type *right);
    Type *typeCheckBitwise(AST::BinaryExpression& binary, Type *left, Type *right);
    Type *typeCheckShift(AST::BinaryExpression& binary, Type *left, Type *right);
    Type *typeCheckEquality(AST::BinaryExpression& binary, Type *left, Type *right);
    Type *typeCheckArithmetic(AST::BinaryExpression& binary, Type *left, Type *right, Type *propagatedType);
    TypeResult visitBinaryExpression(AST::BinaryExpression& binary, Type *declaredType);

    TypeResult visitCallExpression(AST::CallExpression& call, Type *declaredType);

    TypeResult visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess, Type *declaredType);

    TypeResult visitInferredMemberAccessExpression(AST::InferredMemberAccessExpression& inferredMemberAccess, Type *declaredType);

    TypeResult visitLiteral(AST::Literal& literal, Type *declaredType);

    TypeResult visitIdentifier(AST::Identifier& identifier, Type *declaredType);
};

#endif // LANG_typecheck_expression_h