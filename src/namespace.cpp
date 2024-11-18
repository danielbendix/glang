#include "namespace/struct.h"
#include "namespace/enum.h"

#include "AST.h"

#include "llvm/Support/Casting.h"

using Result = PassResult;
using enum PassResultKind;

using llvm::cast;

struct ModuleInserter : public AST::DeclarationVisitorT<ModuleInserter, Result> {
    uint32_t currentFile;
    Module& module;

    AST::Node *getNodeFromDefinition(Definition definition) {
        auto kind = definition.kind();
        auto index = definition.index();
        switch (kind) {
            case Definition::Kind::Function:
                return module.functionDeclarations[index];
            case Definition::Kind::Global:
                return module.globalBindings[index].binding;
            case Definition::Kind::Struct:
                return module.structDeclarations[index];
            case Definition::Kind::Enum:
                return module.enumDeclarations[index];
        }
    }

    void diagnoseDuplicateDeclaration(const Symbol& name, AST::Node& duplicate) {
        Diagnostic::error(duplicate, "Duplicate declaration.");
        auto existing = module.all[name];
        auto declaration = getNodeFromDefinition(existing);
        Diagnostic::note(*declaration, "Previously declared here.");
    }

    Result addGlobal(AST::IdentifierBinding& binding, AST::VariableDeclaration& variable) {
        uint32_t bindingIndex = module.globalBindings.size();
        uint32_t declarationIndex = module.globalDeclarations.size();

        module.globalBindings.emplace_back(&binding, declarationIndex);
        module.globalDeclarations.emplace_back(&variable, bindingIndex, 1);

        const Symbol& name = binding.getIdentifier();
        auto definition = Definition::fromGlobalIndex(bindingIndex);
        if (!module.all.insert(name, definition)) {
            diagnoseDuplicateDeclaration(name, binding);
            return ERROR;
        }

        return OK;
    }

    Result addFunction(const Symbol& name, AST::FunctionDeclaration& functionDeclaration) {
        Result result = OK;

        auto index = module.functions.size();
        module.functions.push_back({});
        module.functionDeclarations.push_back(&functionDeclaration);
        assert(module.functions.size() == module.functionDeclarations.size());
        auto& function = module.functions.back();
        function.file = currentFile;
        function.parameterCount = functionDeclaration.getParameterCount();

        auto definition = Definition::fromFunctionIndex(index);
        if (!module.all.insert(name, definition)) {
            diagnoseDuplicateDeclaration(name, functionDeclaration);
            return ERROR;
        }

        return result;
    }

    Result addStructType(const Symbol& name, StructType *type, AST::StructDeclaration& declaration) {
        auto index = module.structs.size();

        module.structs.push_back(std::move(type));
        module.structDeclarations.push_back(&declaration);
        assert(module.structs.size() == module.structDeclarations.size());

        auto definition = Definition::fromStructIndex(index);
        if (!module.all.insert(name, definition)) {
            diagnoseDuplicateDeclaration(name, declaration);
            return ERROR;
        }

        module.types.insert(name, type);

        return OK;
    }

    Result addEnumType(const Symbol& name, EnumType *enumType, AST::EnumDeclaration& declaration) {
        auto index = module.enums.size();

        module.enums.push_back(std::move(enumType));
        module.enumDeclarations.push_back(&declaration);
        assert(module.enums.size() == module.enumDeclarations.size());

        if (!module.all.insert(name, Definition::fromEnumIndex(index))) {
            diagnoseDuplicateDeclaration(name, declaration);
            return ERROR;
        }

        module.types.insert(name, enumType);

        return OK;
    }

    // - Visitor
    
    Result visitVariableDeclaration(AST::VariableDeclaration& variable) {
        auto& binding = cast<AST::IdentifierBinding>(variable.getBinding());
        return addGlobal(binding, variable);
    }

    Result visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        return addFunction(function.getName(), function);
    }

    Result visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        auto structType = resolveStructType(structDeclaration);
        return addStructType(structDeclaration.getName(), structType, structDeclaration);
    }

    Result visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        auto enumType = resolveEnumType(enumDeclaration);
        return addEnumType(enumDeclaration.getName(), std::move(enumType), enumDeclaration);
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
    ModuleInserter(Module& module, uint32_t file) : module{module} {}

    static Result insertDeclarationsIntoModule(std::span<AST::Declaration *NONNULL> declarations, uint32_t file, Module& module) {
        ModuleInserter inserter{module, file};

        Result result = OK;
        for (auto declaration : declarations) {
            result |= inserter.visit(*declaration);
        }

        return result;
    }
};

void ModuleBuilder::addDeclarations(std::span<AST::Declaration *NONNULL> declarations, uint32_t file) {
    assert(module);
    result |= ModuleInserter::insertDeclarationsIntoModule(declarations, file, *module);
}
