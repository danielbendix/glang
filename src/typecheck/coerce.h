#ifndef LANG_typecheck_coerce_h
#define LANG_typecheck_coerce_h

#include "AST.h"
#include "common.h"

using Result = PassResult;

std::pair<Result, unique_ptr_t<AST::Expression>> coerceType(Type& to, Type& from, AST::Expression& expression) noexcept;

#endif // LANG_typecheck_expression_h
