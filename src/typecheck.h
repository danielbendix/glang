#ifndef LANG_typecheck_h
#define LANG_typecheck_h

#include "AST.h"

Type *typeCheckDeclaration(AST::Declaration& declaration);
Type *typeCheckCondition(AST::Expression *expression);
Type *typeCheckExpression(AST::Expression *condition);

#endif // LANG_typecheck_h
