#ifndef LANG_namespace_h
#define LANG_namespace_h

#include "AST.h"
#include "AST_Visitor.h"

#include "containers/symbol_map.h"
#include "diagnostic.h"

#include "type.h"
#include "type/struct.h"
#include "type/enum.h"

#include "llvm/ADT/PointerUnion.h"

struct ModuleDef {
    using Definition = llvm::PointerUnion<AST::FunctionDeclaration *, AST::VariableDeclaration *, Type *>;
    SymbolMap<Definition> all;

    SymbolMap<AST::Declaration *NONNULL> definitions;
    SymbolMap<Type *NONNULL> types;

    // We could potentially do all of these as one vector, with tagged pointers.
    std::vector<StructType *NONNULL> structs;
    std::vector<EnumType *NONNULL> enums;

    std::vector<AST::VariableDeclaration *NONNULL> globals;
    std::vector<AST::FunctionDeclaration *NONNULL> functions;
};

std::unique_ptr<ModuleDef> createModuleDefinition(std::vector<AST::Declaration *>& declarations);

PassResult resolveNamesInModuleDefinition(ModuleDef& moduleDefinition);

#endif // LANG_namespace_h
