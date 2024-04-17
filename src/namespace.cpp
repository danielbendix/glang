#include "namespace.h"
#include "resolution/identifier.h"


#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

#include "namespace/struct.h"

using Result = PassResult;
using enum PassResultKind;


class NamespaceBuilder {
    ModuleDef& names;
public:
    NamespaceBuilder(ModuleDef& names) : names{names} {}
   
    Result addDeclaration(const std::string& name, AST::Declaration &declaration) {
        if (!names.all.insert(name, &declaration)) {
            Diagnostic::error(declaration, "Duplicate declaration.");
            auto& existing = *names.all[name];
            Diagnostic::note(existing, "Previously declared here.");
            return ERROR;
        }

        return OK;
    }

    Result addVariable(const std::string& name, unique_ptr_t<AST::VariableDeclaration>&& variable) {
        Result result = OK;
        result |= addDeclaration(name, *variable);
        // TODO: Create an ambiguous value
        assert(names.definitions.insert(name, *variable));
        names.globals.push_back(std::move(variable));
        return result;
    }

    Result addFunction(const std::string& name, unique_ptr_t<AST::FunctionDeclaration>&& function) {
        Result result = OK;
        result |= addDeclaration(name, *function);
        // TODO: Create an ambiguous value
        assert(names.definitions.insert(name, *function));
        names.functions.push_back(std::move(function));
        return result;
    }

    Result addType(const std::string& name, unique_ptr_t<Type>&& type) {
        // TODO: Create an ambiguous value
        assert(names.types.insert(name, type.get()));
        names._types.push_back(std::move(type));
        return OK;
    }

    Result addStructType(const std::string& name, unique_ptr_t<StructType>&& structType, unique_ptr_t<AST::StructDeclaration>&& declaration) {
        Result result = OK;
        result |= addDeclaration(name, *declaration);
        result |= addType(name, std::move(structType));
        names.saved.push_back(std::move(declaration));
        return result;
    }
};


/// Create map for global namespace, and ensure that no duplicate declarations exist.
class DeclarationTableVisitor : public AST::DeclarationVisitorT<DeclarationTableVisitor, Result> {
    ModuleDef& names;
    NamespaceBuilder builder;
    
    Result addDeclaration(const std::string& name, AST::Declaration& declaration) {
        if (!names.all.insert(name, declaration)) {
            Diagnostic::error(declaration, "Duplicate declaration.");
            auto& existing = *names.all[name];
            Diagnostic::note(existing, "Previously declared here.");
            return ERROR;
        }
        return OK;
    }
public:
    DeclarationTableVisitor(ModuleDef& names) : names{names}, builder{names} {}

    Result takeDeclaration(unique_ptr_t<AST::Declaration>&& declaration) {
        return declaration.release()->acceptVisitor(*this);
    }

    Result visitVariableDeclaration(AST::VariableDeclaration& variable) {
        return builder.addVariable(variable.getName(), unique_ptr_t<AST::VariableDeclaration>{&variable});
    }

    Result visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        return builder.addFunction(function.getName(), unique_ptr_t<AST::FunctionDeclaration>{&function});
    }

    Result visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        auto structType = resolveStructType(structDeclaration);
        return builder.addStructType(structDeclaration.getName(), std::move(structType), unique_ptr_t<AST::StructDeclaration>{&structDeclaration});
    }

    Result visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        Diagnostic::error(enumDeclaration, "Enums are not currently supported.");
        AST::Node::deleteValue(&enumDeclaration);
        return ERROR;
    }

    Result visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        Diagnostic::error(protocolDeclaration, "Protocols are not currently supported.");
        AST::Node::deleteValue(&protocolDeclaration);
        return ERROR;
    }

    Result visitStatementDeclaration(AST::StatementDeclaration& statement) {
        Diagnostic::error(statement, "Top-level statements are not allowed.");
        return ERROR;
    }
};

std::unique_ptr<ModuleDef> createModuleDefinition(std::vector<AST::unique_ptr<AST::Declaration>>& declarations)
{
    auto names = std::make_unique<ModuleDef>();

    DeclarationTableVisitor visitor{*names};

    for (auto&& declaration : declarations) {
        visitor.takeDeclaration(std::move(declaration));
    }

    return names;
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

    ModuleDef& moduleDefinition;

public:

    ScopeManager(ModuleDef& moduleDefinition) : moduleDefinition{moduleDefinition} {}

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
        
        if (auto declaration = moduleDefinition.definitions.lookup(identifier)) {
            auto result =llvm::TypeSwitch<AST::Declaration *, unique_ptr_t<IdentifierResolution>>(*declaration)
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

class CodeVisitor 
    : public AST::DeclarationVisitorT<CodeVisitor, void>
    , public AST::StatementVisitorT<CodeVisitor, void>
    , public AST::ExpressionVisitorT<CodeVisitor, void>
{
    ScopeManager manager;

    Result result = OK;

    void visitBlock(AST::Block& block) {
        manager.pushInnerScope();
        for (auto& declaration : block) {
            declaration.acceptVisitor(*this);
        }
        manager.popOuterScope();
    }

public:
    
    CodeVisitor(ModuleDef& moduleDefinition) : manager{moduleDefinition} {}

    Result resolveScopeInGlobal(AST::VariableDeclaration& global) {
        manager.reset();
        result = OK;

        if (auto *initial = global.getInitialValue()) {
            initial->acceptVisitor(*this);
        }

        return result;
    }

    Result resolveScopeInFunction(AST::FunctionDeclaration& function) {
        manager.reset();
        result = OK;
        manager.pushOuterScope();

        for (int pi = 0; pi < function.getParameterCount(); ++pi) {
            auto& parameter = function.getParameter(pi);
            manager.pushParameter(parameter.name, function, pi);
        }
        manager.pushInnerScope();

        auto& block = function.getCode();
        for (auto& declaration : function.getCode()) {
            declaration.acceptVisitor(*this);
        }

        manager.popInnerScope();
        manager.popOuterScope();

        return result;
    }

    // Declarations

    void visitVariableDeclaration(AST::VariableDeclaration& variable) {
        manager.pushDeclaration(variable.getName(), variable);

        if (variable.getInitialValue()) {
            variable.getInitialValue()->acceptVisitor(*this);
        }
    }

    void visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        result = ERROR;
        // TODO: Error: Functions are not allowed in functions.
    }

    void visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        result = ERROR;
        // TODO: Error: Structs are not allowed in functions.
    }

    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        result = ERROR;
        // TODO: Error: Enums are not allowed in functions.
    }

    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        result = ERROR;
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

    void visitGuardStatement(AST::GuardStatement& guardStatement) {
        guardStatement.getCondition().acceptVisitor(*this);
        visitBlock(guardStatement.getBlock());
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
        assert(false);
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
            result = ERROR;

            Diagnostic::error(identifier, "Unable to resolve identifier");
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

    void visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess) {
        memberAccess.getTarget().acceptVisitor(*this);
    }
};

PassResult resolveNamesInModuleDefinitiion(ModuleDef& moduleDefinition) 
{
    CodeVisitor visitor{moduleDefinition};

    PassResult result = OK;

    for (auto& global : moduleDefinition.globals) {
        //result |= 
    }

    for (auto& function : moduleDefinition.functions) {
        result |= visitor.resolveScopeInFunction(*function);
    }

    return result;
}
