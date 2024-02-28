#include "AST.h"
#include "AST_Visitor.h"

std::unique_ptr<llvm::StringMap<AST::Declaration *>> globalTable(std::vector<AST::unique_ptr<AST::Declaration>>& declarations);

class DeclarationTableVisitor : public AST::DeclarationVisitorT<DeclarationTableVisitor, void> {

    llvm::StringMap<AST::Declaration *>& table;
    // TODO: Should we store duplicates here?

    void addDeclaration(const std::string& name, AST::Declaration& declaration) {
        auto it = table.insert(std::make_pair(name, &declaration));

        if (!it.second) {
            duplicateDetected = true;

            // EMIT diagnostic
            // Global namespace conflict
            // show initial and attempt to replace.
        }
    }
public:
    void visitVariableDeclaration(AST::VariableDeclaration& variable);
    void visitFunctionDeclaration(AST::FunctionDeclaration& function);
    void visitStructDeclaration(AST::StructDeclaration& structDeclaration);
    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration);
    void visitClassDeclaration(AST::ClassDeclaration& classDeclaration);
    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration);
    void visitStatementDeclaration(AST::StatementDeclaration& statement);
    bool duplicateDetected = false;
    DeclarationTableVisitor(llvm::StringMap<AST::Declaration *>& table) : table{table} {}
};
