#ifndef LANG_typecheck_unify_h
#define LANG_typecheck_unify_h

#include "AST.h"
#include "common.h"

using Result = PassResult;

std::pair<Result, unique_ptr_t<AST::Expression>> unifyTypes(Type& destination, Type& source, AST::Expression& expression) noexcept;

#endif // LANG_typecheck_expression_h
