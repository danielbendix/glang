#include "struct.h"
#include "AST.h"
#include "../AST_Visitor.h"

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

    StructVisitor(AST::StructDeclaration& visiting) : visiting{visiting} {}

    // Declaration visitor

    Result visitVariableDeclaration(AST::VariableDeclaration& variable) {
        auto *identifierBinding = llvm::dyn_cast<AST::IdentifierBinding>(&variable.getBinding());
        if (!identifierBinding) {
            Diagnostic::error(variable.getBinding(), "Only identifier bindings are allowed in struct fields.");
            return ERROR;
        }
        auto& binding = *identifierBinding;
        if (!properties.insert(binding.getIdentifier(), &variable)) {
            Diagnostic::error(binding, "Duplicate declaration of struct field.");
            auto& existing = *static_cast<AST::Node *>(properties[binding.getIdentifier()].getOpaqueValue());
            Diagnostic::note(existing, "Previously declared here.");
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
            Diagnostic::error(function, "Duplicate declaration in struct.");
            auto& existing = *static_cast<AST::Node *>(properties[function.getName()].getOpaqueValue());
            Diagnostic::note(existing, "Previously declared here.");
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

StructType *resolveStructType(AST::StructDeclaration& structDeclaration) {
    StructVisitor visitor{structDeclaration};

    Result result = OK;

    for (auto& declaration : structDeclaration) {
        result |= declaration.acceptVisitor(visitor);
    }

    auto modifiers = structDeclaration.getModifiers();
    bool isCompact = modifiers.has(AST::Modifier::Compact);
    bool isUnpadded = modifiers.has(AST::Modifier::Unpadded);
    
    return StructType::create(
        structDeclaration.getName(), 
        result.ok(),
        isCompact,
        isUnpadded,
        std::move(visitor.properties), 
        std::move(visitor.fields), 
        std::move(visitor.methods)
    );
}
