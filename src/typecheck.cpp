#include "AST.h"
#include "AST_Visitor.h"
#include "typecheck.h"
#include "type.h"

using llvm::dyn_cast;

VoidType void_;
BooleanType boolean;
IntegerType signed64{64, true};

Type *resolveType(AST::TypeNode& typeNode) {
    if (AST::TypeLiteral* literal = dyn_cast<AST::TypeLiteral>(&typeNode)) {
        if (literal->getName() == "int64") {
            return &signed64;
        } else if (literal->getName() == "int") {
            return &signed64;
        } else if (literal->getName() == "bool") {
            return &boolean;
        }
        std::cout << "Unknown type literal: " << literal->getName() << "\n";
    }
    return &signed64;
}

/* For now, we implement type checking with the caveat that declarations must be in order,
 * and not rely on types of later declarations for inference.
 *
 *
 */

template <typename T>
struct Global {
    bool isExternal;

    T meta;

    T& get() {
        return meta;
    }
};

template <typename Metadata>
struct Local {
    const std::string& name;
    int scope;
    int depth;
    int shadows;

    Metadata meta;

    Metadata& get() {
        return meta;
    }

    Local(const std::string& name, int scope, int depth, int shadows, Metadata meta) 
        : name{name}
        , scope{scope}
        , depth{depth}
        , shadows{shadows}
        , meta{meta} {}
};

enum class ScopedVariableType {
    Global,
    Local,
};

template <typename Metadata>
class ScopedVariable {
    Metadata metadata;

    Metadata& get() {
        return metadata;
    }
    virtual const Metadata& get() const {
        return metadata;
    }
};

template <typename Metadata>
class ScopeManager2 {


};

template <typename Metadata>
class ScopeManager {
    int currentScope;
    int currentDepth;

    llvm::StringMap<int> symbols;
    std::vector<Local<Metadata>> locals;

public:
    void pushScope() {
        currentDepth += 1;
    }

    void popScope() {
        for (int i = locals.size() - 1; i >= 0; --i) {
            if (locals[i].depth == currentDepth) {
                int shadows = locals[i].shadows;
                if (shadows != -1) {
                    symbols.insert_or_assign(locals[i].name, shadows);
                }
                locals.pop_back();
            }
        }
    }

    void addLocal(const std::string& name, Metadata meta) {
        auto it = symbols.find(name);

        if (it != symbols.end()) {
            int shadows = it->getValue();
            it->setValue(locals.size());
            locals.emplace_back(name, currentScope, currentDepth, shadows, meta);
        } else {
            symbols.insert(std::make_pair(name, locals.size()));
            locals.emplace_back(name, currentScope, currentDepth, -1, meta);
        }
    }

    void addLocal(const std::string& name) {
        addLocal(name, Metadata{});
    }

    // TODO: Figure out how this works
    template <typename M = Metadata, typename = std::enable_if_t<!std::is_pointer_v<M>>>
    Metadata& resolve(const std::string& name) {
        auto it = symbols.find(name);

        if (it != symbols.end()) {
            int index = it->getValue();
            if (index >= 0) {
                return locals[index].meta;
            } else {

            }
        } else {
            return Metadata{};
        }
    }

    template <typename M = Metadata, typename = std::enable_if_t<std::is_pointer_v<M>>>
    Metadata resolve(const std::string& name) {
        auto it = symbols.find(name);

        if (it != symbols.end()) {
            int index = it->getValue();
            if (index >= 0) {
                return locals[index].meta;
            } else {
                // TODO: return global
                return nullptr;
            }
        } else {
            return nullptr;
        }
    }
};

class ExpressionTypeChecker : public AST::ExpressionVisitorT<ExpressionTypeChecker, Type *> {

    ScopeManager<Type *>& scopeManager;
    const Type * const declaredType;
public:

    ExpressionTypeChecker(ScopeManager<Type *>& scopeManager, Type *declaredType) : scopeManager{scopeManager}, declaredType{declaredType} {}

    Type *typeCheckExpression(AST::Expression *expression) {
        return expression->acceptVisitor(*this);
    }

    Type *visitUnaryExpression(AST::UnaryExpression& unary) {
        Type *targetType = unary.getTarget().acceptVisitor(*this);

        using enum AST::UnaryOperator;
        switch (unary.getOp()) {
            case Not:
                if (targetType == &boolean) {
                    return &boolean;
                } else {
                    return nullptr;
                }
            case Negate:
                // Fixme ensure that type is numeric.
                return targetType;
        }
    }

    Type *visitBinaryExpression(AST::BinaryExpression& binary) {
        Type *leftType = binary.getLeft().acceptVisitor(*this);
        Type *rightType = binary.getRight().acceptVisitor(*this);


        /* TODO: type checking integer and floating point types should 
         * perhaps have an extra type for unconstrained types for literals. */

        if (leftType != rightType) {
            // TODO: Error
            return nullptr;
        }

        using enum AST::BinaryOperator;
        switch (binary.getOp()) {

            case LogicalOr:
            case LogicalAnd:
                if (leftType != &boolean) {
                    // TODO: Error
                } else {
                    return &boolean;
                }
            case Less:
            case LessEqual:
            case Greater:
            case GreaterEqual:
                break;

            case Equal:
            case NotEqual:
                break;

            case Add:
                break;
            case Subtract:
            case Multiply:
            case Divide:
            case Modulo:
                break;
        }

        binary.setType(leftType);
        return leftType;
    }

    Type *visitCallExpression(AST::CallExpression& expression) {
        // Verify that function exists, and call matches arity.
        // Return return type of function.
        auto type = expression.getTarget().acceptVisitor(*this);
        if (FunctionType *functionType = dyn_cast<FunctionType>(type)) {
            if (functionType->parameterCount() != expression.argumentCount()) {
                return nullptr;
            }

            for (int i = 0; i < functionType->parameterCount(); ++i) {
                Type *argumentType = expression.getArgument(i).acceptVisitor(*this);
                Type *parameterType = functionType->getParameter(i);
                if (argumentType != parameterType) {
                    return nullptr;
                }
            }

            Type *type = functionType->getReturnType();
            expression.setType(type);
            return type;
        } else {
            // TODO: Error
            return nullptr;
        }
    }

    Type *visitLiteral(AST::Literal& literal) {
        using enum AST::Literal::Type;
        // TODO: Validate against declaration type.
        switch (literal.getLiteralType()) {
            case Boolean:
                return &boolean;
            case Integer:
                // FIXME: Check declared integer type
                // Can be converted to smaller integer types or a floating point value.
                literal.setType(&signed64);
                return &signed64;
            case Double:
                std::cout << "Unsupported literal type: double";
                exit(-1);
                break;
            case String:
                std::cout << "Unsupported literal type: string";
                exit(-1);
                break;
        }
    }

    Type *visitIdentifier(AST::Identifier& identifier) {
        return scopeManager.resolve(identifier.getName());
    }
};

// TODO: Make one combined declaration type checker, that has get and set variables, push and pop scopes

class GlobalDeclarationTypeChecker : public AST::DeclarationVisitorT<GlobalDeclarationTypeChecker, bool> {

    llvm::StringMap<AST::Declaration *>& globals;

public:
    GlobalDeclarationTypeChecker(llvm::StringMap<AST::Declaration *>& globals) : globals{globals} {}

    bool visitVariableDeclaration(AST::VariableDeclaration& variable) {
        Type *declaredType = nullptr;
        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            declaredType = resolveType(*typeDeclaration);
        }

        Type *initialType = nullptr;
        if (AST::Expression *initial = variable.getInitialValue()) {
            // TODO: Verify that declaration can be without value.
        }

        if (declaredType) {
            variable.setType(*declaredType);
            return true;
        } else {
            return false;
        }
    }

    bool visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        bool success = true;
        if (auto returnTypeNode = function.getReturnTypeDeclaration()) {
            Type *returnType = resolveType(*returnTypeNode);
            if (!returnType) success = false;
            function.setReturnType(*returnType);
        } else {
            function.setReturnType(void_);
        }

        for (int i = 0; i < function.getParameterCount(); ++i) {
            auto& parameter = function.getParameter(i);
            Type *type = resolveType(*parameter.typeDeclaration);
            if (!type) success = false;
            parameter.type = type;
        }
        return success;
    }

    bool visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        // Store type as struct type.
    }

    bool visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        // Store type as enum type.
    }

    bool visitClassDeclaration(AST::ClassDeclaration& classDeclaration) {
        // Store type as class type.
    }

    bool visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        // Store type as protocol type.
    }

    bool visitStatementDeclaration(AST::StatementDeclaration& statement) {
        // Should we allow top-level statements?
    }
};

class DeclarationTypeChecker : public AST::DeclarationVisitorT<DeclarationTypeChecker, void> {

    ScopeManager<Type *> scopeManager;

    void visitVariableDeclaration(AST::VariableDeclaration& variable) {
        Type *declaredType = nullptr;
        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            declaredType = resolveType(*typeDeclaration);
        }

        Type *initialType = nullptr;
        if (AST::Expression *initial = variable.getInitialValue()) {
            // TODO: Verify that declaration can be without value.
            ExpressionTypeChecker checker{scopeManager, declaredType};
            initialType = initial->acceptVisitor(checker);
        }
    }

    void visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        if (auto returnTypeNode = function.getReturnTypeDeclaration()) {
            Type *returnType = resolveType(*returnTypeNode);
            function.setReturnType(*returnType);
        } else {
            function.setReturnType(void_);
        }

        for (int i = 0; i < function.getParameterCount(); ++i) {
            auto& parameter = function.getParameter(i);
            Type *type = resolveType(*parameter.typeDeclaration);
            parameter.type = type;
        }
        // Resolve type names, and create function type.
    }

    void visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        // Store type as struct type.
    }

    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        // Store type as enum type.
    }

    void visitClassDeclaration(AST::ClassDeclaration& classDeclaration) {
        // Store type as class type.
    }

    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        // Store type as protocol type.
    }

    void visitStatementDeclaration(AST::StatementDeclaration& statement) {
        // Should we allow top-level statements?
    }
};


class FunctionTypeChecker {
    ScopeManager<Type *> scopeManager;

    void visitVariableDeclaration(AST::VariableDeclaration& variable) {
        scopeManager.addLocal(variable.getName(), nullptr);

        Type *declaredType = nullptr;
        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            resolveType(*typeDeclaration);
        }

        Type *initialType = nullptr;
        if (AST::Expression *initial = variable.getInitialValue()) {
            ExpressionTypeChecker checker{scopeManager, declaredType};
            initialType = initial->acceptVisitor(checker);
        }
    }
};

template <class T>
concept TypeResolver = requires(T t, std::string& s)
{
    { t(s) } -> std::same_as<Type*>;
    { t.getIdentifier(s) } -> std::same_as<Type *>;
};

bool typeCheckDeclaration(AST::Declaration& declaration, llvm::StringMap<AST::Declaration *> globals)
{
    return true;
}

bool typeCheckDeclarations(std::vector<AST::unique_ptr<AST::Declaration>>& declarations, llvm::StringMap<AST::Declaration *>& globals)
{
    GlobalDeclarationTypeChecker typeChecker{globals};
    
    bool success = true;
    for (const auto& declaration : declarations) {
        if (!declaration->acceptVisitor(typeChecker)) {
            success = false;
        }
    }

    return success;
}

bool typeCheckBodies(std::vector<AST::unique_ptr<AST::Declaration>>& declarations, llvm::StringMap<AST::Declaration *>& globals)
{
    
}


Type *typeCheckCondition(AST::Expression *expression)
{
    // Verify that condition is boolean variable or comparison binary operator
    return &boolean;
}

Type *typeCheckExpression(AST::Expression *condition)
{
    return nullptr;
}
