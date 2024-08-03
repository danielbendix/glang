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

    SymbolMap<AST::Declaration *> definitions;
    SymbolMap<Type *> types;

    std::vector<unique_ptr_t<StructType>> structs;
    std::vector<unique_ptr_t<EnumType>> enums;

    std::vector<unique_ptr_t<Type>> _types;
    std::vector<unique_ptr_t<AST::VariableDeclaration>> _globals;
    std::vector<unique_ptr_t<AST::FunctionDeclaration>> _functions;

    // TODO: Do not rely on these, but save a source file anchor or equivalent.
    std::vector<unique_ptr_t<AST::Declaration>> saved;
};

std::unique_ptr<ModuleDef> createModuleDefinition(std::vector<AST::Declaration *>& declarations);

PassResult resolveNamesInModuleDefinition(ModuleDef& moduleDefinition);

#endif // LANG_namespace_h
