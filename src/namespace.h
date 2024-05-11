#ifndef LANG_namespace_h
#define LANG_namespace_h

#include "AST.h"
#include "AST_Visitor.h"

#include "containers/string_map.h"
#include "diagnostic.h"

#include "type.h"
#include "type/struct.h"
#include "type/enum.h"

struct ModuleDef {
    StringMap<AST::Declaration *> all;

    StringMap<AST::Declaration *> definitions;
    StringMap<Type *> types;

    std::vector<unique_ptr_t<StructType>> structs;
    std::vector<unique_ptr_t<EnumType>> enums;

    std::vector<unique_ptr_t<Type>> _types;
    std::vector<unique_ptr_t<AST::VariableDeclaration>> globals;
    std::vector<unique_ptr_t<AST::FunctionDeclaration>> functions;

    // TODO: Do not rely on these, but save a source file anchor or equivalent.
    std::vector<unique_ptr_t<AST::Declaration>> saved;
};

std::unique_ptr<ModuleDef> createModuleDefinition(std::vector<AST::unique_ptr<AST::Declaration>>& declarations);

PassResult resolveNamesInModuleDefinition(ModuleDef& moduleDefinition);

#endif // LANG_namespace_h
