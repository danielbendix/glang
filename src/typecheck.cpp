#include "typecheck.h"
#include "type.h"
#include "AST.h"

BooleanType boolean;
IntegerType signed64{64, true};

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



Type *resolveType(AST::TypeNode& typeNode) {
    if (AST::TypeLiteral* literal = dynamic_cast<AST::TypeLiteral*>(&typeNode)) {
        if (literal->getName() == "int64") {
            return &signed64;
        } else if (literal->getName() == "bool") {
            return &boolean;
        }
    }
    return &signed64;
}

class DeclarationTypeChecker : public AST::DeclarationVisitor {


};

class ExpressionTypeChecker : public AST::ExpressionVisitor {

    ScopeManager<Type *>& scopeManager;

    ExpressionTypeChecker(ScopeManager<Type *>& scopeManager, Type *declaredType) : scopeManager{scopeManager}, declaredType{declaredType} {}

    const Type * const declaredType;
    Type *returnValue;
    Type *typeCheckExpression(AST::Expression *expression) {
        expression->acceptVisitor(*this);
        return returnValue;
    }

    virtual void visitUnaryExpression(AST::UnaryExpression& unaryExpression) override {

    }

    virtual void visitBinaryExpression(AST::BinaryExpression& expression) override {

    }

    virtual void visitCallExpression(AST::CallExpression& expression) override {

    }

    virtual void visitLiteral(AST::Literal& literal) override {
        using enum AST::Literal::Type;
        // TODO: Validate against declaration type.
        switch (literal.getLiteralType()) {
            case Boolean:
                returnValue = &boolean;
                break;
            case Integer:
                // TODO: Check declared integer type
                // Can be converted to smaller integer types or a floating point value.
                returnValue = &signed64;
                break;
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

    virtual void visitIdentifier(AST::Identifier& identifier) override {
        returnValue = scopeManager.resolve(identifier.getName());
    }

};

class FunctionTypeChecker : public AST::DeclarationVisitor, public AST::StatementVisitor, public AST::ExpressionVisitor {
    ScopeManager<Type *> scopeManager;

    virtual void visitVariableDeclaration(AST::VariableDeclaration& variable) override {
        scopeManager.addLocal(variable.getName(), nullptr);

        Type *declaredType = nullptr;
        if (AST::TypeNode *typeDeclaration = variable.getTypeDeclaration()) {
            // Resolve type
        }


        if (AST::Expression *initial = variable.getInitialValue()) {
            // Typecheck initial value

        }

    }


};

template <class T>
concept TypeResolver = requires(T t, std::string& s)
{
    { t(s) } -> std::same_as<Type*>;
    { t.getIdentifier(s) } -> std::same_as<Type *>;
};

template <TypeResolver T>
class ExpressionChecker : public AST::ExpressionVisitor {

    T& resolver;

    virtual void visitUnaryExpression(AST::UnaryExpression& unaryExpression) override {

    }

    virtual void visitBinaryExpression(AST::BinaryExpression& expression) override {

    }

    virtual void visitCallExpression(AST::CallExpression& expression) override {

    }

    virtual void visitLiteral(AST::Literal& literal) override {

    }

    virtual void visitIdentifier(AST::Identifier& identifier) override {
        Type *type = resolver(identifier.getName());



    }
};







Type * typeCheckDeclaration(AST::Declaration& declaration)
{
    return nullptr;
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
