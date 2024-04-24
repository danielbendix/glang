#include "control.h"

#include "AST_Visitor.h"

#include "diagnostic.h"

using Result = PassResult;
using enum PassResultKind;

/// Returns true iff the statement as a whole will return on all paths.
class FunctionBodyAnalyzer : public AST::DeclarationVisitorT<FunctionBodyAnalyzer, bool>,
                             public AST::StatementVisitorT<FunctionBodyAnalyzer, bool> {
    Result result = OK;
    bool isVoid;
public:
    FunctionBodyAnalyzer() {}

    Result analyzeFunction(AST::FunctionDeclaration& function) {
        isVoid = function.getType()->getReturnType()->isVoid();

        bool returns = visitBlock(function.getCode());

        if (!isVoid && !returns) {
            Diagnostic::error(function, "Control reaches end of non-void function.");
            result = ERROR;
        }

        return result;
    }

    /// Returns true iff the declaration as a whole will return.
    bool willReturn(AST::Declaration& declaration) {
        return declaration.acceptVisitor(*this);
    }

    bool visitBlock(AST::Block& block) {
        for (int i = 0; i < block.size(); ++i) {
            if (willReturn(block[i])) {
                if (i + 1 < block.size()) {
                    Diagnostic::warning(block[i + 1], "Code after [[GET STATEMENT NAME]] will not be executed");
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

    bool visitGuardStatement(AST::GuardStatement& guardStatement) {
        bool returns = visitBlock(guardStatement.getBlock());

        if (!returns) {
            Diagnostic::error(guardStatement, "Guard statement body must return.");
            result = ERROR;
        }

        return false;
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
    bool visitCompoundAssignmentStatement(AST::CompoundAssignmentStatement& assignment) { return false; }
    bool visitExpressionStatement(AST::ExpressionStatement& expression) { return false; }

};

class GlobalDeclarationAnalyzer : public AST::DeclarationVisitorT<GlobalDeclarationAnalyzer, Result> {
public:
    GlobalDeclarationAnalyzer() {}

    Result visitProtocolDeclaration(AST::ProtocolDeclaration& protocol) { return OK; }
    Result visitVariableDeclaration(AST::VariableDeclaration& variable) { return OK; }
    Result visitStatementDeclaration(AST::StatementDeclaration& statement) { llvm_unreachable("Should not exist at this point"); }

    Result visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        // TODO: Visit methods
        return OK;
    }

    Result visitFunctionDeclaration(AST::FunctionDeclaration& function) { 
        FunctionBodyAnalyzer analyzer;

        return analyzer.analyzeFunction(function);
    }
};

PassResult analyzeControlFlow(ModuleDef& moduleDefinition)
{
    GlobalDeclarationAnalyzer analyzer;

    Result result = OK;
    for (auto& declaration : moduleDefinition.functions) {
        result |= declaration->acceptVisitor(analyzer);
    }

    return result;
}
