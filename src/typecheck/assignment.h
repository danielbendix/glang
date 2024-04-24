#ifndef LANG_typecheck_assignment_h
#define LANG_typecheck_assignment_h

#include "AST.h"

void diagnoseInvalidAssignmentTarget(const AST::Expression& expression, const AST::AssignmentStatement& assignment);

#endif // LANG_typecheck_assignment_h
