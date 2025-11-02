#include "struct.h"
#include "AST.h"
#include "AST_Visitor.h"

using Result = PassResult;
using enum PassResultKind;

using Modifier = AST::Modifiers::Modifier;

struct StructVisitor : public AST::DeclarationVisitorT<StructVisitor, Result> {
    using Property = llvm::PointerUnion<AST::VariableDeclaration *, AST::FunctionDeclaration *>;
    AST::StructDeclaration& visiting;
    SymbolMap<Property> properties;
    std::vector<AST::VariableDeclaration *> fields;
    std::vector<AST::FunctionDeclaration *> methods;
    std::vector<AST::VariableDeclaration *> staticFields;
    std::vector<AST::FunctionDeclaration *> staticMethods;
    const FileID file;

    StructVisitor(AST::StructDeclaration& visiting, FileID file)
        : visiting{visiting}, file{file} {}

    // Declaration visitor

    Result visitVariableDeclaration(AST::VariableDeclaration& variable) {
        auto *identifierBinding = llvm::dyn_cast<AST::IdentifierBinding>(&variable.getBinding());
        if (!identifierBinding) {
            Diagnostic::error(variable.getBinding(), "Only identifier bindings are allowed in struct fields.");
            return ERROR;
        }
        auto& binding = *identifierBinding;
        if (!properties.insert(binding.getIdentifier(), &variable)) {
            auto fileLocation = binding.getFileLocation();

            Diagnostic::error(binding, "Duplicate declaration of struct field.", file);
            auto& existing = *static_cast<AST::Node *>(properties[binding.getIdentifier()].getOpaqueValue());
            Diagnostic::note(existing, "Previously declared here.", file, fileLocation.offset);
            return ERROR;
        }

        if (variable.getModifiers().has(AST::Modifier::Static)) {
            staticFields.push_back(&variable);
        } else {
            fields.push_back(&variable);
        }

        return OK;
    }

    Result visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        if (!properties.insert(function.getName(), &function)) {
            auto fileLocation = function.getFileLocation();

            Diagnostic::error(function, "Duplicate declaration in struct.", file);
            auto& existing = *static_cast<AST::Node *>(properties[function.getName()].getOpaqueValue());
            Diagnostic::note(existing, "Previously declared here.", file, fileLocation.offset);
            return ERROR;
        }
        if (function.getModifiers().has(AST::Modifier::Static)) {

        } else {
            methods.push_back(&function);
        }

        return OK;
    }

    Result visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        Diagnostic::error(structDeclaration, "Nested struct declarations are not supported.");
        return ERROR;
    }

    Result visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        Diagnostic::error(enumDeclaration, "Nested enum declarations are not supported.");
        return ERROR;
    }

    Result visitProtocolDeclaration(AST::ProtocolDeclaration& protocol) {
        Diagnostic::error(protocol, "Nested protocol declarations are not supported.");
        return ERROR;
    }

    Result visitStatementDeclaration(AST::StatementDeclaration& statement) {
        Diagnostic::error(statement, "Statements are not allowed inside structs.");
        return ERROR;
    }
};

StructType *createStructType(AST::StructDeclaration& structDeclaration, FileID file) {
    StructVisitor visitor{structDeclaration, file};

    Result result = OK;

    for (auto& declaration : structDeclaration) {
        result |= declaration.acceptVisitor(visitor);
    }

    auto modifiers = structDeclaration.getModifiers();
    bool isCompact = modifiers.has(AST::Modifier::Compact);
    bool isUnpadded = modifiers.has(AST::Modifier::Unpadded);
    
    return StructType::create(
        structDeclaration.getName(), 
        file,
        result.ok(),
        isCompact,
        isUnpadded,
        std::move(visitor.properties), 
        std::move(visitor.fields), 
        std::move(visitor.methods)
    );
}
