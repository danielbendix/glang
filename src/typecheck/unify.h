#ifndef LANG_typecheck_unify_h
#define LANG_typecheck_unify_h

#include "AST.h"
#include "common.h"

using Result = PassResult;

Type *NULLABLE unifyTypesForComparison(AST::BinaryExpression& binary, Type& left, Type& right);
Type *NULLABLE unifyTypesForEquality(AST::BinaryExpression& binary, Type& left, Type& right);
Type *NULLABLE unifyTypesForArithmetic(AST::BinaryExpression& binary, Type& left, Type& right);
Type *NULLABLE unifyTypesForBitwiseArithmetic(AST::BinaryExpression& binary, Type& left, Type& right, Type *propagatedType);


std::pair<Result, AST::Expression *NULLABLE> unifyTypesForBitwiseArithmetic(Type& left, Type& right);



#endif
