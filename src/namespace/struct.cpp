#include "struct.h"
#include "AST.h"
#include "../AST_Visitor.h"

using Result = PassResult;
using enum PassResultKind;

struct StructVisitor : public AST::DeclarationVisitorT<StructVisitor, Result> {
    using Property = llvm::PointerUnion<AST::VariableDeclaration *, AST::FunctionDeclaration *>;
    AST::StructDeclaration& visiting;
    StringMap<Property> properties;
    std::vector<AST::VariableDeclaration *> fields;
    std::vector<AST::FunctionDeclaration *> methods;

    StructVisitor(AST::StructDeclaration& visiting) : visiting{visiting} {}

    // Declaration visitor

    Result visitVariableDeclaration(AST::VariableDeclaration& variable) {
        if (!properties.insert(variable.getName(), &variable)) {
            Diagnostic::error(variable, "Duplicate declaration of struct field.");
            auto& existing = *static_cast<AST::Node *>(properties[variable.getName()].getOpaqueValue());
            Diagnostic::note(existing, "Previously declared here.");
            return ERROR;
        }
        fields.push_back(&variable);

        return OK;
    }

    Result visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        if (!properties.insert(function.getName(), &function)) {
            Diagnostic::error(function, "Duplicate declaration in struct.");
            auto& existing = *static_cast<AST::Node *>(properties[function.getName()].getOpaqueValue());
            Diagnostic::note(existing, "Previously declared here.");
            return ERROR;
        }
        methods.push_back(&function);

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

unique_ptr_t<StructType> resolveStructType(AST::StructDeclaration& structDeclaration) {
    StructVisitor visitor{structDeclaration};

    Result result = OK;

    for (auto& declaration : structDeclaration) {
        result |= declaration.acceptVisitor(visitor);
    }
    
    return StructType::create(structDeclaration.getName(), result.ok(), std::move(visitor.properties), std::move(visitor.fields), std::move(visitor.methods));
}
