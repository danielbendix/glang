#include "namespace.h"


std::unique_ptr<llvm::StringMap<AST::Declaration *>> globalTable(std::vector<AST::unique_ptr<AST::Declaration>>& declarations)
{
    auto table = std::make_unique<llvm::StringMap<AST::Declaration *>>();

    DeclarationTableVisitor visitor{*table};

    for (const auto& declaration : declarations) {
        declaration->acceptVisitor(visitor);
    }

    for (const auto& value : *table) {
        std::cout << ((std::string_view) value.first()) << ":\n";
        std::cout << *value.second;
    }

    return table;
}

void DeclarationTableVisitor::visitVariableDeclaration(AST::VariableDeclaration& variable) {
    addDeclaration(variable.getName(), variable);
}

void DeclarationTableVisitor::visitFunctionDeclaration(AST::FunctionDeclaration& function) {
    addDeclaration(function.getName(), function);
}

void DeclarationTableVisitor::visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
    addDeclaration(structDeclaration.getName(), structDeclaration);
}

void DeclarationTableVisitor::visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
    
}

void DeclarationTableVisitor::visitClassDeclaration(AST::ClassDeclaration& classDeclaration) {

}

void DeclarationTableVisitor::visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {

}

void DeclarationTableVisitor::visitStatementDeclaration(AST::StatementDeclaration& statement) {

}
