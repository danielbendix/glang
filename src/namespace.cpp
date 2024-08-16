#include "namespace.h"
#include "resolution/identifier.h"


#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

#include "namespace/struct.h"
#include "namespace/enum.h"

using Result = PassResult;
using enum PassResultKind;

using llvm::TypeSwitch;


class NamespaceBuilder {
    ModuleDef& names;
    SymbolMap<AST::Declaration *> declarations;
public:
    NamespaceBuilder(ModuleDef& names) : names{names} {}

    void diagnoseDuplicateDeclaration(const Symbol& name, AST::Declaration& duplicate) {
        Diagnostic::error(duplicate, "Duplicate declaration.");
        auto& existing = *declarations[name];
        Diagnostic::note(existing, "Previously declared here.");
    }

    Result addFunction(const Symbol& name, AST::FunctionDeclaration& declaration) {
        if (!names.all.insert(name, &declaration)) {
            diagnoseDuplicateDeclaration(name, declaration);
            return ERROR;
        }
        declarations.insert(name, &declaration);
        return OK;
    }

    Result addGlobal(const Symbol& name, AST::VariableDeclaration& declaration) {
        if (!names.all.insert(name, &declaration)) {
            diagnoseDuplicateDeclaration(name, declaration);
            return ERROR;
        }
        declarations.insert(name, &declaration);
        return OK;
    }

    Result addType(const Symbol& name, Type& type, AST::Declaration& declaration) {
        if (!names.all.insert(name, &type)) {
            diagnoseDuplicateDeclaration(name, declaration);
            return ERROR;
        }
        names.types.insert(name, &type);
        declarations.insert(name, &declaration);
        return OK;
    }

    Result addVariable(const Symbol& name, unique_ptr_t<AST::VariableDeclaration>&& variable) {
        Result result = OK;
        result |= addGlobal(name, *variable);
        // TODO: Create an ambiguous value
        assert(names.definitions.insert(name, variable.get()));
        names._globals.push_back(std::move(variable));
        return result;
    }

    Result addFunction(const Symbol& name, unique_ptr_t<AST::FunctionDeclaration>&& function) {
        Result result = OK;
        result |= addFunction(name, *function);
        // TODO: Create an ambiguous value
        assert(names.definitions.insert(name, function.get()));
        names._functions.push_back(std::move(function));
        return result;
    }

    Result addStructType(const Symbol& name, unique_ptr_t<StructType>&& type, unique_ptr_t<AST::StructDeclaration>&& declaration) {
        Result result = OK;
        result |= addType(name, *type, *declaration);
        names.structs.push_back(std::move(type));
        names.saved.push_back(std::move(declaration));
        return result;
    }

    Result addEnumType(const Symbol& name, unique_ptr_t<EnumType>&& enumType, unique_ptr_t<AST::EnumDeclaration>&& declaration) {
        Result result = addType(name, *enumType, *declaration);
        names.enums.push_back(std::move(enumType));
        names.saved.push_back(std::move(declaration));
        return result;
    }
};

/// Create map for global namespace, and ensure that no duplicate declarations exist.
class DeclarationTableVisitor : public AST::DeclarationVisitorT<DeclarationTableVisitor, Result> {
    ModuleDef& names;
    NamespaceBuilder builder;
    
public:
    DeclarationTableVisitor(ModuleDef& names) : names{names}, builder{names} {}

    Result takeDeclaration(AST::Declaration *declaration) {
        return declaration->acceptVisitor(*this);
    }

    Result visitVariableDeclaration(AST::VariableDeclaration& variable) {
        assert(false && "TODO: Implement global variables.");

        //return builder.addVariable(variable.getName(), unique_ptr_t<AST::VariableDeclaration>{&variable});
    }

    Result visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        return builder.addFunction(function.getName(), unique_ptr_t<AST::FunctionDeclaration>{&function});
    }

    Result visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        auto structType = resolveStructType(structDeclaration);
        return builder.addStructType(structDeclaration.getName(), std::move(structType), unique_ptr_t<AST::StructDeclaration>{&structDeclaration});
    }

    Result visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        auto enumType = resolveEnumType(enumDeclaration);
        return builder.addEnumType(enumDeclaration.getName(), std::move(enumType), unique_ptr_t<AST::EnumDeclaration>{&enumDeclaration});
        Diagnostic::error(enumDeclaration, "Enums are not currently supported.");
        return ERROR;
    }

    Result visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        Diagnostic::error(protocolDeclaration, "Protocols are not currently supported.");
        AST::Node::deleteValue(&protocolDeclaration);
        return ERROR;
    }

    Result visitStatementDeclaration(AST::StatementDeclaration& statement) {
        Diagnostic::error(statement, "Top-level statements are not allowed.");
        AST::Node::deleteValue(&statement);
        return ERROR;
    }
};

std::unique_ptr<ModuleDef> createModuleDefinition(std::vector<AST::Declaration *>& declarations)
{
    auto names = std::make_unique<ModuleDef>();

    DeclarationTableVisitor visitor{*names};

    for (auto&& declaration : declarations) {
        visitor.takeDeclaration(declaration);
    }

    return names;
}

class ScopeManager {
    
    struct Local {
        const Symbol& identifier;
        const int scopeLevel;
        bool isParameter;
        union {
            struct {
                AST::FunctionDeclaration *function;
                int index;
            } parameter;
            AST::IdentifierBinding *binding;
        } as;

        Local(const Symbol& identifier, int scopeLevel, AST::FunctionDeclaration& function, int index) : identifier{identifier}, scopeLevel{scopeLevel}, isParameter{true}, as{.parameter = {.function=&function, index = index}} {}

        Local(const Symbol& identifier, int scopeLevel, AST::IdentifierBinding& binding) : identifier{identifier}, scopeLevel{scopeLevel}, isParameter{false}, as{.binding = &binding} {}
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

    Result pushParameter(const Symbol& identifier, AST::FunctionDeclaration& function, int index) {
        for (int i = locals.size() - 1; i >= 0 && locals[i].scopeLevel == scopeLevel; --i) {
            if (locals[i].identifier == identifier) {
                Diagnostic::error(function, "Invalid redeclaration of parameter " + identifier.string());
                return ERROR;
            }
        }
        locals.emplace_back(identifier, scopeLevel, function, index);

        return OK;
    }

    Result pushBinding(const Symbol& identifier, AST::IdentifierBinding& binding) {
        for (int i = locals.size() - 1; i >= 0 && locals[i].scopeLevel == scopeLevel; --i) {
            if (locals[i].identifier == identifier) {
                Diagnostic::error(binding, "Invalid redeclaration of " + identifier.string());
                Diagnostic::note(*locals[i].as.binding, identifier.string() + " previously declared here.");
                return ERROR;
            }
        }
        locals.emplace_back(identifier, scopeLevel, binding);
        return OK;
    }

    std::unique_ptr<IdentifierResolution, Deleter<IdentifierResolution>> getResolution(const Symbol& identifier) {
        for (int i = locals.size() - 1; i >= 0; --i) {
            Local& local = locals[i];
            if (local.identifier == identifier) {
                if (local.isParameter) {
                    return FunctionParameterResolution::create(local.as.parameter.function, local.as.parameter.index);
                } else {
                    return LocalResolution::create(*local.as.binding);
                }
            }
        }
        
        if (auto declaration = moduleDefinition.all.lookup(identifier)) {
            auto result = TypeSwitch<ModuleDef::Definition, unique_ptr_t<IdentifierResolution>>(*declaration)
                .Case<AST::FunctionDeclaration *>([](auto function) {
                    return FunctionResolution::create(*function);
                })
                .Case<AST::VariableDeclaration *>([](auto variable) {
                    assert(false);
                    return nullptr;
                    //return GlobalResolution::create(variable->getBinding(), false);
                })
                .Case<Type *>([](auto type) {
                    return IdentifierTypeResolution::create(type);
                })
                .Default([](auto any) {
                    assert(false);
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
        manager.popInnerScope();
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
            result |= manager.pushParameter(parameter.name, function, pi);
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
        if (auto initial = variable.getInitialValue()) {
            initial->acceptVisitor(*this);
        }
        // FIXME: Support other binding types.
        auto& identifierBinding = llvm::cast<AST::IdentifierBinding>(variable.getBinding());
        result |= manager.pushBinding(identifierBinding.getIdentifier(), identifierBinding);
    }

    void visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        Diagnostic::error(function, "Nested function declarations are not allowed in functions.");
        result = ERROR;
    }

    void visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        Diagnostic::error(structDeclaration, "Nested structs are not allowed in functions.");
        result = ERROR;
    }

    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        Diagnostic::error(enumDeclaration, "Nested enums are not allowed in functions.");
        result = ERROR;
    }

    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        Diagnostic::error(protocolDeclaration, "Nested protocols are not allowed in functions.");
        result = ERROR;
    }

    void visitStatementDeclaration(AST::StatementDeclaration& statement) {
        statement.getStatement().acceptVisitor(*this);
    }

    // Statements

    void visitAssignmentStatement(AST::AssignmentStatement& assignment) {
        assignment.getTarget().acceptVisitor(*this);
        assignment.getValue().acceptVisitor(*this);
    }

    void visitCompoundAssignmentStatement(AST::CompoundAssignmentStatement& assignment) {
        assignment.getTarget().acceptVisitor(*this);
        assignment.getOperand().acceptVisitor(*this);
    }

    void handleConditionalBinding(AST::VariableDeclaration& variable) {
        if (auto initial = variable.getInitialValue()) {
            initial->acceptVisitor(*this);
        } else {
            Diagnostic::error(variable, "Conditional binding must have a value specified.");
            result = ERROR;
        }
        // FIXME: Support other binding types.
        auto& identifierBinding = llvm::cast<AST::IdentifierBinding>(variable.getBinding());
        result |= manager.pushBinding(identifierBinding.getIdentifier(), identifierBinding);
    }

    void handleCondition(AST::Condition condition) {
        TypeSwitch<AST::Condition>(condition)
            .Case<AST::VariableDeclaration *>([&](AST::VariableDeclaration *variable) {
                handleConditionalBinding(*variable);
            })
            .Case<AST::Expression *>([&](AST::Expression *expression) {
                expression->acceptVisitor(*this);
            });
    }

    void visitIfStatement(AST::IfStatement& ifStatement) {
        for (int bi = 0; bi < ifStatement.getConditionCount(); ++bi) {
            manager.pushInnerScope();
            auto& branch = ifStatement.getCondition(bi);
            for (auto condition : branch.getConditions()) {
                handleCondition(condition);
            }
            visitBlock(branch.getBlock());
            manager.popInnerScope();
        }
        if (auto* fallback = ifStatement.getFallback()) {
            visitBlock(*fallback);
        }
    }

    void visitGuardStatement(AST::GuardStatement& guardStatement) {
        visitBlock(guardStatement.getBlock());
        for (auto condition : guardStatement.getConditions()) {
            handleCondition(condition);
        }
    }

    void visitReturnStatement(AST::ReturnStatement& returnStatement) {
        if (auto* value = returnStatement.getValue()) {
            value->acceptVisitor(*this);
        }
    }

    void visitWhileStatement(AST::WhileStatement& whileStatement) {
        manager.pushInnerScope();
        for (auto condition : whileStatement.getConditions()) {
            handleCondition(condition);
        }
        visitBlock(whileStatement.getBlock());
        manager.popInnerScope();
    }

    void visitForStatement(AST::ForStatement& forStatement) {
        forStatement.getIterable().acceptVisitor(*this);
        manager.pushInnerScope();

        // FIXME: Support other binding types.
        auto& identifierBinding = llvm::cast<AST::IdentifierBinding>(forStatement.getBinding());
        manager.pushBinding(identifierBinding.getIdentifier(), identifierBinding);

        visitBlock(forStatement.getBlock());
        manager.popInnerScope();
    }

    void visitBreakStatement(AST::BreakStatement&) {}
    void visitContinueStatement(AST::ContinueStatement&) {}

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

    void visitSubscriptExpression(AST::SubscriptExpression& subscript) {
        subscript.getTarget().acceptVisitor(*this);
        subscript.getIndex().acceptVisitor(*this);
    }

    void visitInitializerExpression(AST::InitializerExpression& initializer) {
        for (size_t i = 0; i < initializer.getNumberOfPairs(); ++i) {
            auto& pair = initializer.getPair(i);
            pair.first->acceptVisitor(*this);
            pair.second->acceptVisitor(*this);
        }
    }

    void visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess) {
        memberAccess.getTarget().acceptVisitor(*this);
    }

    void visitInferredMemberAccessExpression(AST::InferredMemberAccessExpression& inferredMemberAccess) {}
};

PassResult resolveNamesInModuleDefinition(ModuleDef& moduleDefinition) 
{
    CodeVisitor visitor{moduleDefinition};

    PassResult result = OK;

    for (auto& global : moduleDefinition._globals) {
        //result |= 
    }

    for (auto& function : moduleDefinition._functions) {
        result |= visitor.resolveScopeInFunction(*function);
    }

    return result;
}
