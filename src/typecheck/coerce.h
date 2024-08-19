#ifndef LANG_typecheck_coerce_h
#define LANG_typecheck_coerce_h

#include "AST.h"
#include "common.h"

using Result = PassResult;

std::pair<Result, AST::Expression *> coerceType(Type& to, Type& from, AST::Expression& expression);

std::pair<Result, AST::Expression *> coerceCompoundAssignmentOperand(Type& to, Type& from, AST::BinaryOperator op, AST::Expression& expression);

#endif // LANG_typecheck_expression_h
