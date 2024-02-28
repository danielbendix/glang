#ifndef LANG_codegen_h
#define LANG_codegen_h

#import "AST.h"
#import "llvm/IR/Module.h"

std::unique_ptr<llvm::Module> generateCode(std::vector<AST::unique_ptr<AST::Declaration>>& declarations);

#endif // LANG_codegen_h
