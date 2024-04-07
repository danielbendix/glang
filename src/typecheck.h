#ifndef LANG_typecheck_h
#define LANG_typecheck_h

#include "common.h"

#include "AST.h"
#include "diagnostic.h"

PassResult typeCheckDeclarations(std::vector<AST::unique_ptr<AST::Declaration>>& declarations, llvm::StringMap<AST::Declaration *>& globals, DiagnosticWriter& diagnostic);

#endif // LANG_typecheck_h
