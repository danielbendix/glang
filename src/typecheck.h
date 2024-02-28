#ifndef LANG_typecheck_h
#define LANG_typecheck_h

#include "AST.h"

bool typeCheckDeclarations(std::vector<AST::unique_ptr<AST::Declaration>>& declarations, llvm::StringMap<AST::Declaration *>& globals);
Type *typeCheckCondition(AST::Expression *expression);
Type *typeCheckExpression(AST::Expression *condition);

#endif // LANG_typecheck_h
