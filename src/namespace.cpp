#include "namespace.h"
#include "resolution/identifier.h"


#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

#include "namespace/struct.h"
#include "namespace/enum.h"

using Result = PassResult;
using enum PassResultKind;

using llvm::TypeSwitch;
using llvm::cast;


class NamespaceBuilder {
    ModuleDef& names;
    SymbolMap<AST::Node *> declarations;
public:
    NamespaceBuilder(ModuleDef& names) : names{names} {}

    void diagnoseDuplicateDeclaration(const Symbol& name, AST::Node& duplicate) {
        Diagnostic::error(duplicate, "Duplicate declaration.");
        auto& existing = *declarations[name];
        Diagnostic::note(existing, "Previously declared here.");
    }

    // TODO: Clean up all these methods with proper names.

    Result addGlobalFunction(const Symbol& name, AST::FunctionDeclaration& declaration) {
        if (!names.all.insert(name, &declaration)) {
            diagnoseDuplicateDeclaration(name, declaration);
            return ERROR;
        }
        declarations.insert(name, &declaration);
        return OK;
    }

    Result addGlobal(const Symbol& name, AST::IdentifierBinding& binding) {
        if (!names.all.insert(name, &binding)) {
            diagnoseDuplicateDeclaration(name, binding);
            return ERROR;
        }
        declarations.insert(name, &binding);
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

    Result addVariable(AST::IdentifierBinding& binding, AST::VariableDeclaration& variable) {
        const Symbol& name = binding.getIdentifier();
        Result result = OK;
        result |= addGlobal(name, binding);
        // TODO: Create an ambiguous value on error
        assert(names.definitions.insert(name, &variable));
        names.globals.push_back(&variable);
        return result;
    }

    Result addFunction(const Symbol& name, AST::FunctionDeclaration& function) {
        Result result = OK;
        result |= addGlobalFunction(name, function);
        // TODO: Create an ambiguous value on error
        assert(names.definitions.insert(name, &function));
        names.functions.push_back(&function);
        return result;
    }

    Result addStructType(const Symbol& name, StructType *type, AST::StructDeclaration& declaration) {
        Result result = OK;
        result |= addType(name, *type, declaration);
        names.structs.push_back(std::move(type));
        return result;
    }

    Result addEnumType(const Symbol& name, EnumType *enumType, AST::EnumDeclaration& declaration) {
        Result result = addType(name, *enumType, declaration);
        names.enums.push_back(std::move(enumType));
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
        auto& binding = cast<AST::IdentifierBinding>(variable.getBinding());
        return builder.addVariable(binding, variable);
    }

    Result visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        return builder.addFunction(function.getName(), function);
    }

    Result visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        auto structType = resolveStructType(structDeclaration);
        return builder.addStructType(structDeclaration.getName(), structType, structDeclaration);
    }

    Result visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        auto enumType = resolveEnumType(enumDeclaration);
        return builder.addEnumType(enumDeclaration.getName(), std::move(enumType), enumDeclaration);
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
        const Symbol* identifier;
        bool isParameter;
        union {
            struct {
                AST::FunctionDeclaration *function;
                int index;
            } parameter;
            AST::IdentifierBinding *binding;
        } as;

        [[noreturn]]
        Local() {
            llvm_unreachable("Programmer error: Default constructor of Local should never be called.");
        }

        Local(const Symbol& identifier, AST::FunctionDeclaration& function, int index) : identifier{&identifier}, isParameter{true}, as{.parameter = {.function=&function, index = index}} {}

        Local(const Symbol& identifier, AST::IdentifierBinding& binding) : identifier{&identifier}, isParameter{false}, as{.binding = &binding} {}
    };

    static_assert(sizeof(Local) <= 32);

    std::vector<Local> locals = {};
    std::vector<uint32_t> scopes = {0};

    ModuleDef& moduleDefinition;

public:

    ScopeManager(ModuleDef& moduleDefinition) : moduleDefinition{moduleDefinition} {}

    void reset() {
        locals.clear();
        scopes.clear();
        scopes.push_back(0);
    }

    void pushOuterScope() {

    }

    void popOuterScope() {

    }

    void pushInnerScope() {
        scopes.push_back(locals.size());
    }

    void popInnerScope() {
        uint32_t scope = scopes.back();
        scopes.pop_back();

        locals.resize(scope);
    }

    Result pushParameter(const Symbol& identifier, AST::FunctionDeclaration& function, int index) {
        assert(!scopes.empty());
        int64_t maxIndex = scopes.back();
        
        for (int i = locals.size() - 1; i >= maxIndex; --i) {
            if (*locals[i].identifier == identifier) {
                Diagnostic::error(function, "Invalid redeclaration of parameter " + identifier.string());
                return ERROR;
            }
        }
        locals.emplace_back(identifier, function, index);
        return OK;
    }

    Result pushBinding(const Symbol& identifier, AST::IdentifierBinding& binding) {
        assert(!scopes.empty());
        int64_t maxIndex = scopes.back();

        for (int i = locals.size() - 1; i >= maxIndex; --i) {
            if (*locals[i].identifier == identifier) {
                Diagnostic::error(binding, "Invalid redeclaration of " + identifier.string());
                Diagnostic::note(*locals[i].as.binding, identifier.string() + " previously declared here.");
                return ERROR;
            }
        }
        locals.emplace_back(identifier, binding);
        return OK;
    }

    IdentifierResolution getResolution(const Symbol& identifier) {
        for (int i = locals.size() - 1; i >= 0; --i) {
            Local& local = locals[i];
            if (*local.identifier == identifier) {
                if (local.isParameter) {
                    return IdentifierResolution::parameter(local.as.parameter.function, local.as.parameter.index);
                } else {
                    return IdentifierResolution::local(local.as.binding);
                }
            }
        }
        
        if (auto declaration = moduleDefinition.all.lookup(identifier)) {
            auto result = TypeSwitch<ModuleDef::Definition, IdentifierResolution>(*declaration)
                .Case<AST::FunctionDeclaration *>([](auto function) {
                    return IdentifierResolution::function(function);
                })
                .Case<AST::VariableDeclaration *>([](auto variable) {
                    return IdentifierResolution::global(cast<AST::IdentifierBinding>(&variable->getBinding()), false);
                })
                .Case<Type *>([](auto type) {
                    return IdentifierResolution::type(type);
                })
                .Case<AST::IdentifierBinding *>([](auto binding) {
                    return IdentifierResolution::global(binding, false);
                })
                .Default([](auto any) {
                    assert(false);
                    return IdentifierResolution::unresolved();
                })
            ;
            if (result) {
                return result;
            }
        }

        return IdentifierResolution::unresolved();
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
        IdentifierResolution resolution = manager.getResolution(identifier.getName());

        if (resolution) {
            identifier.setResolution(resolution);
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

    void visitIntrinsicExpression(AST::IntrinsicExpression& intrinsic) {
        for (auto argument : intrinsic.getArguments()) {
            argument->acceptVisitor(*this);
        }
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

    for (auto global : moduleDefinition.globals) {
        result |= visitor.resolveScopeInGlobal(*global);
    }

    for (auto function : moduleDefinition.functions) {
        result |= visitor.resolveScopeInFunction(*function);
    }

    return result;
}
