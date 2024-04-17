#ifndef LANG_control_h
#define LANG_control_h

#include "common.h"

#include "AST.h"
#include "namespace.h"

/**
 * Analyze control flow of code, doing the following:
 * - Remove strictly unreachable code, e.g. not variables that are never used.
 * - Detect if a non-void function can reach the end of execution without returning a value.
 */
PassResult analyzeControlFlow(ModuleDef& moduleDefinition);
PassResult analyzeControlFlow(std::vector<AST::unique_ptr<AST::Declaration>>& declarations);

#endif // LANG_control_h
