#ifndef LANG_codegen_h
#define LANG_codegen_h

#include "AST.h"
#include "llvm/IR/Module.h"
#include "namespace.h"

std::unique_ptr<llvm::Module> generateCode(Module& module);

#endif // LANG_codegen_h
