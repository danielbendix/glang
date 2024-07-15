#ifndef LANG_typecheck_unify_h
#define LANG_typecheck_unify_h

#include "AST.h"
#include "common.h"

using Result = PassResult;

void unifyTypesForArithmetic(Type& left, Type& right);


std::pair<Result, unique_ptr_t<AST::Expression>> unifyTypesForBitwiseArithmetic(Type& left, Type& right);



#endif
