#include "control.h"

#include "AST_Visitor.h"

#include "diagnostic.h"

using Result = PassResult;
using enum PassResultKind;

enum class ControlFlowEffect {
    Continues,
    EndsBlock,
    EndsFunction,
};

using enum ControlFlowEffect;

enum class LoopType {
    None,
    While,
    For,
};

/// Returns true iff the statement as a whole will return on all paths.
class FunctionBodyAnalyzer : public AST::DeclarationVisitorT<FunctionBodyAnalyzer, ControlFlowEffect>,
                             public AST::StatementVisitorT<FunctionBodyAnalyzer, ControlFlowEffect> {
    Result result = OK;
    bool isVoid;
    LoopType loop = LoopType::None;
public:
    FunctionBodyAnalyzer() {}

    Result analyzeFunction(Function& function, AST::FunctionDeclaration *declaration) {
        isVoid = function.type->getReturnType()->isVoid();

        auto effect = visitBlock(declaration->getCode());

        if (!isVoid && effect != EndsFunction) {
            Diagnostic::error(declaration->getClosingBracketLocation(), "Control reaches end of non-void function.");
            result = ERROR;
        }

        return result;
    }

    /// Returns true iff the declaration as a whole will return.
    ControlFlowEffect willReturn(AST::Declaration& declaration) {
        return declaration.acceptVisitor(*this);
    }

    ControlFlowEffect visitBlock(AST::Block& block) {
        for (int i = 0; i < block.size(); ++i) {
            auto effect = block[i].acceptVisitor(*this);
            if (effect > Continues) {
                if (i + 1 < block.size()) {
                    Diagnostic::warning(block[i + 1], "Code after [[GET STATEMENT NAME]] will not be executed");
                    block.shrinkTo(i + 1);
                }
                return effect;
            }
        }
        return Continues;
    }

    // Declarations

    ControlFlowEffect visitVariableDeclaration(AST::VariableDeclaration& variable) { return Continues; }
    ControlFlowEffect visitFunctionDeclaration(AST::FunctionDeclaration& function) { return Continues; }
    ControlFlowEffect visitStructDeclaration(AST::StructDeclaration& structDeclaration) { return Continues; }
    ControlFlowEffect visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) { return Continues; }
    ControlFlowEffect visitProtocolDeclaration(AST::ProtocolDeclaration& protocol) { return Continues; }

    ControlFlowEffect visitStatementDeclaration(AST::StatementDeclaration& statement) {
        return statement.getStatement().acceptVisitor(*this);
    }

    // Statements

    ControlFlowEffect visitReturnStatement(AST::ReturnStatement& returnStatement) {
        return EndsFunction;
    }

    ControlFlowEffect visitIfStatement(AST::IfStatement& ifStatement) {
        auto minEffect = EndsFunction;

        for (size_t i = 0; i < ifStatement.getConditionCount(); ++i) {
            auto& branch = ifStatement.getCondition(i);

            auto effect = visitBlock(branch.getBlock());

            minEffect = std::min(minEffect, effect);
        }
        if (auto *fallback = ifStatement.getFallback()) {
            auto effect = visitBlock(*fallback);

            minEffect = std::min(minEffect, effect);
        } else {
            minEffect = Continues;
        }

        return minEffect;
    }

    ControlFlowEffect visitGuardStatement(AST::GuardStatement& guardStatement) {
        auto effect = visitBlock(guardStatement.getBlock());

        if (effect != EndsFunction) {
            Diagnostic::error(guardStatement, "Guard statement body must return.");
            result = ERROR;
        }

        return Continues;
    }

    ControlFlowEffect visitWhileStatement(AST::WhileStatement& whileStatement) {
        auto savedLoop = loop;
        loop = LoopType::While;
        // TODO: This can be evaluated differently if condition is the literal 'true'
        visitBlock(whileStatement.getBlock());
        loop = savedLoop;
        return Continues;
    }

    ControlFlowEffect visitForStatement(AST::ForStatement& forStatement) { 
        auto savedLoop = loop;
        loop = LoopType::For;
        visitBlock(forStatement.getBlock());
        loop = savedLoop;
        return Continues;
    }

    ControlFlowEffect visitBreakStatement(AST::BreakStatement& breakStatement) {
        if (loop == LoopType::None) {
            Diagnostic::error(breakStatement, "'break' statement not in loop.");
            result |= ERROR;
            return Continues;
        }
        return EndsBlock;
    }

    ControlFlowEffect visitContinueStatement(AST::ContinueStatement& continueStatement) {
        if (loop == LoopType::None) {
            Diagnostic::error(continueStatement, "'continue' statement not in loop.");
            result |= ERROR;
            return Continues;
        }
        return EndsBlock;
    }

    ControlFlowEffect visitDeferStatement(AST::DeferStatement& deferStatement) {
        llvm_unreachable("TODO: Handle defer in control flow analysis");
    }
    
    ControlFlowEffect visitAssignmentStatement(AST::AssignmentStatement& assignment) { return Continues; }
    ControlFlowEffect visitCompoundAssignmentStatement(AST::CompoundAssignmentStatement& assignment) { return Continues; }
    ControlFlowEffect visitExpressionStatement(AST::ExpressionStatement& expression) { return Continues; }

};

Result analyzeFunction(Function& function, AST::FunctionDeclaration *declaration) {
    FunctionBodyAnalyzer analyzer;

    return analyzer.analyzeFunction(function, declaration);
}

PassResult analyzeControlFlow(Module& module)
{
    Result result = OK;
    for (auto [function, declaration] : llvm::zip(module.functions, module.functionDeclarations)) {
        result |= analyzeFunction(function, declaration);
    }

    return result;
}
