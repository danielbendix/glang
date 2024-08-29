#include "struct.h"
#include "AST.h"
#include "../AST_Visitor.h"

using Result = PassResult;
using enum PassResultKind;

struct StructVisitor : public AST::DeclarationVisitorT<StructVisitor, Result> {
    using Property = llvm::PointerUnion<AST::VariableDeclaration *, AST::FunctionDeclaration *>;
    AST::StructDeclaration& visiting;
    SymbolMap<Property> properties;
    std::vector<AST::VariableDeclaration *> fields;
    std::vector<AST::FunctionDeclaration *> methods;

    // Bitmap for storing which fields have a default initial value.
    uint64_t currentInitialized = 0;
    llvm::SmallVector<uint64_t, 1> areInitialized;

    StructVisitor(AST::StructDeclaration& visiting) : visiting{visiting} {}

    void finalize() {
        if (fields.size() & 0x3F) {
            areInitialized.push_back(currentInitialized);
        }
    }

    // Declaration visitor

    Result visitVariableDeclaration(AST::VariableDeclaration& variable) {
        // TODO: To keep structs simple, we need to error on any bindings other than IdentifierBinding here.
        auto& binding = llvm::cast<AST::IdentifierBinding>(variable.getBinding());
        if (!properties.insert(binding.getIdentifier(), &variable)) {
            Diagnostic::error(binding, "Duplicate declaration of struct field.");
            auto& existing = *static_cast<AST::Node *>(properties[binding.getIdentifier()].getOpaqueValue());
            Diagnostic::note(existing, "Previously declared here.");
            return ERROR;
        }

        if (variable.getInitialValue()) {
            currentInitialized |= 1 << (fields.size() & 0x3F);
        }

        fields.push_back(&variable);

        if ((fields.size() & 0x3F) == 0) {
            areInitialized.push_back(currentInitialized);
            currentInitialized = 0;
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

StructType *resolveStructType(AST::StructDeclaration& structDeclaration) {
    StructVisitor visitor{structDeclaration};

    Result result = OK;

    for (auto& declaration : structDeclaration) {
        result |= declaration.acceptVisitor(visitor);
    }

    visitor.finalize();
    
    return StructType::create(structDeclaration.getName(), result.ok(), std::move(visitor.properties), std::move(visitor.fields), std::move(visitor.methods), visitor.areInitialized);
}
