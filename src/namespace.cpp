#include "namespace.h"
#include "resolution.h"

#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

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

void DeclarationTableVisitor::visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {

}

void DeclarationTableVisitor::visitStatementDeclaration(AST::StatementDeclaration& statement) {
    // TODO: Emit error
}



class ScopeManager {
    
    struct Local {
        const std::string& identifier;
        const int scopeLevel;
        bool isParameter;
        union {
            struct {
                AST::FunctionDeclaration *function;
                int index;
            } parameter;
            AST::VariableDeclaration *variableDeclaration;
        } as;

        Local(const std::string& identifier, int scopeLevel, AST::FunctionDeclaration& function, int index) : identifier{identifier}, scopeLevel{scopeLevel}, isParameter{true}, as{.parameter = {.function=&function, index = index}} {}

        Local(const std::string& identifier, int scopeLevel, AST::VariableDeclaration& variableDeclaration) : identifier{identifier}, scopeLevel{scopeLevel}, isParameter{false}, as{.variableDeclaration = &variableDeclaration} {}
    };

    int scopeLevel = -1;
    std::vector<Local> locals = {};

    llvm::StringMap<AST::Declaration *>& globals;

public:

    ScopeManager(llvm::StringMap<AST::Declaration *>& globals) : globals{globals} {}

    void reset() {
        locals.clear();
    }

    void pushOuterScope() {

    }

    void popOuterScope() {

    }

    void pushInnerScope() {
        scopeLevel++;
    }

    void popInnerScope() {
        // TODO: Use resize()
        for (int i = locals.size() - 1; i >= 0; --i) {
            if (locals[i].scopeLevel == scopeLevel) {
                locals.pop_back();
            }
        }
        scopeLevel--;
    }

    bool pushParameter(std::string& identifier, AST::FunctionDeclaration& function, int index) {
        locals.emplace_back(identifier, scopeLevel, function, index);

        return true;
    }

    void pushDeclaration(const std::string& identifier, AST::VariableDeclaration& declaration) {
        locals.emplace_back(identifier, scopeLevel, declaration);
    }

    std::unique_ptr<IdentifierResolution, Deleter<IdentifierResolution>> getResolution(const std::string& identifier) {
        for (int i = locals.size() - 1; i >= 0; --i) {
            Local& local = locals[i];
            if (local.identifier == identifier) {
                if (local.isParameter) {
                    return FunctionParameterResolution::create(local.as.parameter.function, local.as.parameter.index);
                } else {
                    return LocalResolution::create(*local.as.variableDeclaration);
                }
            }
        }
        
        auto it = globals.find(identifier);
        if (it != globals.end()) {
            auto result =llvm::TypeSwitch<AST::Declaration *, unique_ptr_t<IdentifierResolution>>(it->second)
                .Case<AST::FunctionDeclaration>([](auto function) {
                    return FunctionResolution::create(*function);
                })
                .Case<AST::VariableDeclaration>([](auto variable) {
                    return GlobalResolution::create(*variable, false);
                })
                .Default([](auto any) {
                    return nullptr;
                })
            ;
            if (result) {
                return result;
            }
        }

        return nullptr;
    }

};

class FunctionBodyVisitor 
    : public AST::DeclarationVisitorT<FunctionBodyVisitor, void>
    , public AST::StatementVisitorT<FunctionBodyVisitor, void>
    , public AST::ExpressionVisitorT<FunctionBodyVisitor, void>
{
    ScopeManager manager;

    bool hadError = false;

    void visitBlock(const AST::Block& block) {
        manager.pushInnerScope();
        for (int bi = 0; bi < block.size(); bi++) {
            block[bi].acceptVisitor(*this);
        }
        manager.popOuterScope();
    }

public:
    
    FunctionBodyVisitor(llvm::StringMap<AST::Declaration *>& globals) : manager{globals} {}

    bool resolveScopeInFunction(AST::FunctionDeclaration *function) {
        manager.reset();
        hadError = false;
        manager.pushOuterScope();

        for (int pi = 0; pi < function->getParameterCount(); ++pi) {
            auto& parameter = function->getParameter(pi);
            manager.pushParameter(parameter.name, *function, pi);
        }
        manager.pushInnerScope();

        auto& block = function->getCode();
        for (int di = 0; di < block.size(); ++di) {
            auto& declaration = block[di];
            declaration.acceptVisitor(*this);
        }

        manager.popInnerScope();
        manager.popOuterScope();

        return hadError;
    }

    // Declarations

    void visitVariableDeclaration(AST::VariableDeclaration& variable) {
        manager.pushDeclaration(variable.getName(), variable);

        if (variable.getInitialValue()) {
            variable.getInitialValue()->acceptVisitor(*this);
        }
    }

    void visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        // TODO: Error: Functions are not allowed in functions.
    }

    void visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        // TODO: Error: Structs are not allowed in functions.
    }

    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        // TODO: Error: Enums are not allowed in functions.
    }

    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        // TODO: Error: Protocols are not allowed in functions.
    }

    void visitStatementDeclaration(AST::StatementDeclaration& statement) {
        statement.getStatement().acceptVisitor(*this);
    }

    // Statements

    void visitAssignmentStatement(AST::AssignmentStatement& assignment) {
        assignment.getTarget().acceptVisitor(*this);
        assignment.getValue().acceptVisitor(*this);
    }

    void visitIfStatement(AST::IfStatement& ifStatement) {
        for (int ci = 0; ci < ifStatement.getConditionCount(); ++ci) {
            auto& branch = ifStatement.getCondition(ci);
            branch.getCondition().acceptVisitor(*this);
            visitBlock(branch.getBlock());
        }
        if (auto* fallback = ifStatement.getFallback()) {
            visitBlock(*fallback);
        }
    }

    void visitReturnStatement(AST::ReturnStatement& returnStatement) {
        if (auto* value = returnStatement.getValue()) {
            value->acceptVisitor(*this);
        }
    }

    void visitWhileStatement(AST::WhileStatement& whileStatement) {
        whileStatement.getCondition().acceptVisitor(*this);
        visitBlock(whileStatement.getBlock());
    }

    void visitForStatement(AST::ForStatement& forStatement) {
        // TODO: Implement this.
    }

    void visitExpressionStatement(AST::ExpressionStatement& expression) {
        expression.getExpression().acceptVisitor(*this);
    }

    // Expressions
    
    void visitIdentifier(AST::Identifier& identifier) {
        unique_ptr_t<IdentifierResolution> resolution = manager.getResolution(identifier.getName());

        if (resolution) {
            identifier.setResolution(std::move(resolution));
        } else {
            hadError = true;

            std::cout << "Unable to resolveidentifier with name " << identifier << "\n";
            std::cout << resolution << "\n";
            // TODO: Emit error
        }
    }

    void visitLiteral(AST::Literal&) {}

    void visitUnaryExpression(AST::UnaryExpression& unary) {
        unary.getTarget().acceptVisitor(*this);
    }

    void visitBinaryExpression(AST::BinaryExpression& binary) {
        binary.getLeft().acceptVisitor(*this);
        binary.getRight().acceptVisitor(*this);
    }

    void visitCallExpression(AST::CallExpression& call) {
        call.getTarget().acceptVisitor(*this);

        for (int ai = 0; ai < call.argumentCount(); ++ai) {
            call.getArgument(ai).acceptVisitor(*this);
        }
    }
};

bool resolveNames(std::vector<AST::unique_ptr<AST::Declaration>>& declarations, llvm::StringMap<AST::Declaration *> *globals)
{
    FunctionBodyVisitor visitor{*globals};

    bool hadError = false;

    for (auto& declaration : declarations) {
        if (auto *function = llvm::dyn_cast<AST::FunctionDeclaration>(declaration.get())) {
            hadError = visitor.resolveScopeInFunction(function) || hadError;
        }
    }
    
    return hadError;
}
