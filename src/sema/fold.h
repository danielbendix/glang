#ifndef LANG_sema_fold
#define LANG_sema_fold

#include "AST.h"
#include "AST_Visitor.h"

AST::Literal *NULLABLE foldLiteralUnary(AST::UnaryOperator op, AST::Literal& operand);
AST::Literal *NULLABLE foldLiteralsBinary(AST::BinaryOperator op, AST::Literal& left, AST::Literal& right);

/// Recursively fold all constant expression that can be resolved without types.
AST::Expression *NULLABLE foldConstantsUntyped(AST::Expression& expression);

/// Recursively fold all constants within expression. This assumes that the entire expression is type checked.
/// Returns an error if any calculated values exceed their type bounds.
//std::pair<Result, AST::Expression *NULLABLE> foldConstantsTyped(AST::Expression& expression);

#endif // LANG_sema_fold
