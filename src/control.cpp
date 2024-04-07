#include "control.h"

#include "AST_Visitor.h"

class FunctionBodyAnalyzer : public AST::DeclarationVisitorT<FunctionBodyAnalyzer, bool>,
                             public AST::StatementVisitorT<FunctionBodyAnalyzer, bool> {
    DiagnosticWriter& diagnostic;

    bool error = false;
    bool isVoid;
public:
    FunctionBodyAnalyzer(DiagnosticWriter& diagnostic) : diagnostic{diagnostic} {}

    bool analyzeFunction(AST::FunctionDeclaration& function) {
        isVoid = function.getType()->getReturnType()->isVoid();

        bool returns = visitBlock(function.getCode());

        if (!isVoid && !returns) {
            diagnostic.error(function, "Control reaches end of non-void function.");
            error = true;
        }

        return error;
    }

    /// Returns true iff the declaration as a whole will return.
    bool willReturn(AST::Declaration& declaration) {
        return declaration.acceptVisitor(*this);
    }

    bool visitBlock(AST::Block& block) {
        for (int i = 0; i < block.size(); ++i) {
            if (willReturn(block[i])) {
                if (i + 1 < block.size()) {
                    diagnostic.warning(block[i + 1], "Code after [[GET STATEMENT NAME]] will not be executed");
                    block.resize(i + 1);
                }
                return true;
            }
        }
        return false;
    }

    // Declarations

    bool visitVariableDeclaration(AST::VariableDeclaration& variable) { return false; }
    bool visitFunctionDeclaration(AST::FunctionDeclaration& function) { return false; }
    bool visitStructDeclaration(AST::StructDeclaration& structDeclaration) { return false; }
    bool visitProtocolDeclaration(AST::ProtocolDeclaration& protocol) { return false; }

    bool visitStatementDeclaration(AST::StatementDeclaration& statement) {
        return statement.getStatement().acceptVisitor(*this);
    }

    // Statements

    bool visitReturnStatement(AST::ReturnStatement& returnStatement) {
        return true;
    }

    bool visitIfStatement(AST::IfStatement& ifStatement) {
        bool allReturn = true;

        for (size_t i = 0; i < ifStatement.getConditionCount(); ++i) {
            auto& branch = ifStatement.getCondition(i);

            bool returns = visitBlock(branch.getBlock());

            allReturn = allReturn && returns;
        }
        if (auto *fallback = ifStatement.getFallback()) {
            bool returns = visitBlock(*fallback);

            allReturn = allReturn && returns;
        } else {
            allReturn = false;
        }

        return allReturn;
    }

    bool visitWhileStatement(AST::WhileStatement& whileStatement) {
        // TODO: This can be evaluated differently if condition is the literal 'true'
        visitBlock(whileStatement.getBlock());
        return false;
    }

    bool visitForStatement(AST::ForStatement& forStatement) { 
        visitBlock(forStatement.getBlock());
        return false;
    }

    bool visitAssignmentStatement(AST::AssignmentStatement& assignment) { return false; }
    bool visitExpressionStatement(AST::ExpressionStatement& expression) { return false; }

};

class GlobalDeclarationAnalyzer : public AST::DeclarationVisitorT<GlobalDeclarationAnalyzer, bool> {
    DiagnosticWriter& diagnostic;

public:
    GlobalDeclarationAnalyzer(DiagnosticWriter& diagnostic) : diagnostic{diagnostic} {}

    bool visitProtocolDeclaration(AST::ProtocolDeclaration& protocol) { return false; }
    bool visitVariableDeclaration(AST::VariableDeclaration& variable) { return false; }
    bool visitStatementDeclaration(AST::StatementDeclaration& statement) { assert(false); }

    bool visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        // TODO: Visit methods
        return false;
    }

    bool visitFunctionDeclaration(AST::FunctionDeclaration& function) { 
        FunctionBodyAnalyzer analyzer{diagnostic};

        return analyzer.analyzeFunction(function);
    }
};

bool analyzeControlFlow(std::vector<AST::unique_ptr<AST::Declaration>>& declarations, DiagnosticWriter& diagnostic)
{
    GlobalDeclarationAnalyzer analyzer{diagnostic};

    bool error = false;
    for (auto& declaration : declarations) {
        error = declaration->acceptVisitor(analyzer) || error;
    }

    return error;
}
