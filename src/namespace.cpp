#include "namespace.h"
#include "resolution/identifier.h"


#include "llvm/Support/Casting.h"

#include "namespace/struct.h"
#include "namespace/enum.h"

using Result = PassResult;
using enum PassResultKind;

using llvm::cast;


class NamespaceBuilder {
    ModuleDef& names;
    SymbolMap<AST::Node *> declarations;
public:
    NamespaceBuilder(ModuleDef& names) : names{names} {}

    void diagnoseDuplicateDeclaration(const Symbol& name, AST::Node& duplicate) {
        Diagnostic::error(duplicate, "Duplicate declaration.");
        auto& existing = *declarations[name];
        Diagnostic::note(existing, "Previously declared here.");
    }

    // TODO: Clean up all these methods with proper names.

    Result addGlobalFunction(const Symbol& name, AST::FunctionDeclaration& declaration) {
        if (!names.all.insert(name, &declaration)) {
            diagnoseDuplicateDeclaration(name, declaration);
            return ERROR;
        }
        declarations.insert(name, &declaration);
        return OK;
    }

    Result addGlobal(const Symbol& name, AST::IdentifierBinding& binding) {
        if (!names.all.insert(name, &binding)) {
            diagnoseDuplicateDeclaration(name, binding);
            return ERROR;
        }
        declarations.insert(name, &binding);
        return OK;
    }

    Result addType(const Symbol& name, Type& type, AST::Declaration& declaration) {
        if (!names.all.insert(name, &type)) {
            diagnoseDuplicateDeclaration(name, declaration);
            return ERROR;
        }
        names.types.insert(name, &type);
        declarations.insert(name, &declaration);
        return OK;
    }

    Result addVariable(AST::IdentifierBinding& binding, AST::VariableDeclaration& variable) {
        const Symbol& name = binding.getIdentifier();
        Result result = OK;
        result |= addGlobal(name, binding);
        // TODO: Create an ambiguous value on error
        assert(names.definitions.insert(name, &variable));
        names.globals.push_back(&variable);
        return result;
    }

    Result addFunction(const Symbol& name, AST::FunctionDeclaration& function) {
        Result result = OK;
        result |= addGlobalFunction(name, function);
        // TODO: Create an ambiguous value on error
        assert(names.definitions.insert(name, &function));
        names.functions.push_back(&function);
        return result;
    }

    Result addStructType(const Symbol& name, StructType *type, AST::StructDeclaration& declaration) {
        Result result = OK;
        result |= addType(name, *type, declaration);
        names.structs.push_back(std::move(type));
        return result;
    }

    Result addEnumType(const Symbol& name, EnumType *enumType, AST::EnumDeclaration& declaration) {
        Result result = addType(name, *enumType, declaration);
        names.enums.push_back(std::move(enumType));
        return result;
    }
};

/// Create map for global namespace, and ensure that no duplicate declarations exist.
class DeclarationTableVisitor : public AST::DeclarationVisitorT<DeclarationTableVisitor, Result> {
    ModuleDef& names;
    NamespaceBuilder builder;
    
public:
    DeclarationTableVisitor(ModuleDef& names) : names{names}, builder{names} {}

    Result takeDeclaration(AST::Declaration *declaration) {
        return declaration->acceptVisitor(*this);
    }

    Result visitVariableDeclaration(AST::VariableDeclaration& variable) {
        auto& binding = cast<AST::IdentifierBinding>(variable.getBinding());
        return builder.addVariable(binding, variable);
    }

    Result visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        return builder.addFunction(function.getName(), function);
    }

    Result visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        auto structType = resolveStructType(structDeclaration);
        return builder.addStructType(structDeclaration.getName(), structType, structDeclaration);
    }

    Result visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        auto enumType = resolveEnumType(enumDeclaration);
        return builder.addEnumType(enumDeclaration.getName(), std::move(enumType), enumDeclaration);
        Diagnostic::error(enumDeclaration, "Enums are not currently supported.");
        return ERROR;
    }

    Result visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        Diagnostic::error(protocolDeclaration, "Protocols are not currently supported.");
        AST::Node::deleteValue(&protocolDeclaration);
        return ERROR;
    }

    Result visitStatementDeclaration(AST::StatementDeclaration& statement) {
        Diagnostic::error(statement, "Top-level statements are not allowed.");
        AST::Node::deleteValue(&statement);
        return ERROR;
    }
};

std::unique_ptr<ModuleDef> createModuleDefinition(std::vector<AST::Declaration *>& declarations)
{
    auto names = std::make_unique<ModuleDef>();

    DeclarationTableVisitor visitor{*names};

    for (auto&& declaration : declarations) {
        visitor.takeDeclaration(declaration);
    }

    return names;
}
