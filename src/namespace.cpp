#include "namespace/struct.h"
#include "namespace/enum.h"

#include "AST.h"

#include "llvm/Support/Casting.h"

using Result = PassResult;
using enum PassResultKind;

using llvm::cast;

struct ModuleInserter : public AST::DeclarationVisitorT<ModuleInserter, Result> {
    const u32 currentFile;
    Module& module;
    Symbol& mainSymbol;

    std::pair<AST::Node *NONNULL, u32> getNodeAndFileFromDefinition(Definition definition) {
        auto kind = definition.kind();
        auto index = definition.index();
        switch (kind) {
            case Definition::Kind::Function:
                return {module.functionDeclarations[index], module.functions[index].file};
            case Definition::Kind::Global: {
                auto& binding = module.globalBindings[index];
                return {binding.binding, module.globalDeclarations[binding.declarationIndex].file};
            }
            case Definition::Kind::Struct:
                return {module.structDeclarations[index], module.structs[index]->file};
            case Definition::Kind::Enum:
                return {module.enumDeclarations[index], module.enums[index]->file};
        }
    }

    void diagnoseDuplicateDeclaration(const Symbol& name, AST::Node& duplicate) {
        auto existing = module.all[name];

        u32 offendingFile = currentFile;
        auto [original, originalFile] = getNodeAndFileFromDefinition(existing);

        Diagnostic::error(duplicate, "Duplicate declaration.", offendingFile);
        Diagnostic::note(*original, "Previously declared here.", offendingFile, originalFile, duplicate.getFileLocation().offset);
    }

    Result addGlobal(AST::IdentifierBinding& binding, AST::VariableDeclaration& variable) {
        u32 bindingIndex = module.globalBindings.size();
        u32 declarationIndex = module.globalDeclarations.size();

        module.globalBindings.emplace_back(&binding, declarationIndex);
        module.globalDeclarations.emplace_back(&variable, bindingIndex, 1, currentFile);

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
        module.functions.push_back({u16(functionDeclaration.getParameterCount()), currentFile});
        module.functionDeclarations.push_back(&functionDeclaration);
        assert(module.functions.size() == module.functionDeclarations.size());

        if (name == mainSymbol) {
            if (module.mainFunction) {
                u32 offendingFile = currentFile;
                Diagnostic::error(functionDeclaration, "Duplicate main function declaration.", offendingFile);
                u32 previousMainFile = module.functions[*module.mainFunction].file;
                auto *previousMainDeclaration = module.functionDeclarations[*module.mainFunction];
                Diagnostic::note(*previousMainDeclaration, "main function previously declared here.", offendingFile, previousMainFile, functionDeclaration.offset);
                return ERROR;
            } else {
                // TODO: main will not be callable from other functions, which is probably desired.
                // But there should be a different declaration for main to have a syntactical distinction.
                module.mainFunction = index;
            }
        }

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
        auto structType = createStructType(structDeclaration, currentFile);
        return addStructType(structDeclaration.getName(), structType, structDeclaration);
    }

    Result visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        auto enumType = createEnumType(enumDeclaration, currentFile);
        return addEnumType(enumDeclaration.getName(), std::move(enumType), enumDeclaration);
        Diagnostic::error(enumDeclaration, "Enums are not currently supported.");
        return ERROR;
    }

    Result visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        Diagnostic::error(protocolDeclaration, "Protocols are not currently supported.");
        return ERROR;
    }

    Result visitStatementDeclaration(AST::StatementDeclaration& statement) {
        Diagnostic::error(statement, "Top-level statements are not allowed.");
        return ERROR;
    }

    ModuleInserter(Module& module, u32 file) 
        : module{module}
        , currentFile{file} 
        , mainSymbol{ThreadContext::get()->symbols->getSymbol("main")}
        {}

    static Result insertDeclarationsIntoModule(std::span<AST::Declaration *NONNULL> declarations, u32 file, Module& module) {
        ModuleInserter inserter{module, file};

        Result result = OK;
        for (auto declaration : declarations) {
            result |= inserter.visit(*declaration);
        }

        return result;
    }
};

void ModuleBuilder::addDeclarations(std::span<AST::Declaration *NONNULL> declarations, u32 file) {
    assert(module);
    result |= ModuleInserter::insertDeclarationsIntoModule(declarations, file, *module);
}
