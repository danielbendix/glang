#ifndef LANG_ast_h
#define LANG_ast_h

#include "scanner.h" // Token, TokenType
#include "unique_ref.h"
#include "type.h"

#include <string>
#include <vector>
#include <cassert>

#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/Casting.h"

#include <iostream>
/* The declaration of all AST nodes could be done in a simpler, declarative language.
 * That defines all the properties, and if they're public or accessed via getter/setter;
 * This would ensure consistency, and ease of changing conventions.
 */

/*
 * Node:
 *  Declaration:
 *  Statement:
 *  Expression:
 *  Type:
 */

namespace AST {
    class PrintContext;

    struct Location {
        // TODO: SourceFile reference
        int line;
        int offset;

        Location(Token token) : line{token.line}, offset{token.offset} {}
    };

    class Node {
    public:
        enum Kind {
            // Declarations
            NK_Decl_Variable,
            NK_Decl_Function,
            NK_Decl_Struct,
            NK_Decl_Enum,
            NK_Decl_Class,
            NK_Decl_Protocol,
            NK_Decl_Statement,

            // Statements
            NK_Stmt_Assignment,
            NK_Stmt_If,
            NK_Stmt_Return,
            NK_Stmt_While,
            NK_Stmt_For,
            NK_Stmt_Expression,

            // Expression
            NK_Expr_Identifier,
            NK_Expr_Literal,
            NK_Expr_Unary,
            NK_Expr_Binary,
            NK_Expr_Call,

            // Type nodes
            NK_Type_Literal,
        };

        Kind kind;
        Location location;
        Node(Kind kind, Location location) : kind{kind}, location{location} {}
    public:
        Node(const Node&) = delete;
        Node& operator=(const Node&) = delete;
        ~Node() = default;

        Kind getKind() const { return kind; }
        Location getLocation() const { return location; }

        //void acceptVisitor();

        void print(std::ostream& os) const;

        virtual void print(PrintContext& pc) const = 0;

        static void deleteNode(AST::Node *node);
    };

    struct Deleter {
        void operator()(AST::Node *node) const {
            AST::Node::deleteNode(node);
        }
    };

    template <typename T>
    using unique_ptr = std::unique_ptr<T, Deleter>;
}
std::ostream& operator<<(std::ostream& os, const AST::Node& node);
std::ostream& operator<<(std::ostream& os, AST::Node& node);

namespace AST {

    // Types

    class TypeNode : public Node {
    protected:
        using Node::Node;

        ~TypeNode() = default;
    public:
        static bool classof(const Node *node) {
            return node->getKind() >= NK_Type_Literal && node->getKind() <= NK_Type_Literal;
        }
    };

    class TypeLiteral : public TypeNode {
    protected:
        std::string identifier;

        TypeLiteral(Token name) : TypeNode{NK_Type_Literal, Location{name}}, identifier{name.chars} {}

        virtual void print(PrintContext& pc) const;

    public:
        static unique_ptr<TypeLiteral> create(Token name) {
            return unique_ptr<TypeLiteral>(new TypeLiteral(name));
        }

        const std::string& getName() const {
            return identifier;
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Type_Literal;
        }
    };

    // Utilities
    
    class Declaration;
    class Block final {
    protected:
        std::vector<unique_ptr<Declaration>> declarations;

        Block(const Block&) = delete;
        Block& operator=(const Block&) = delete;
    public:
        Block(std::vector<unique_ptr<Declaration>>&& declarations) : declarations{std::move(declarations)} {}
        Block(Block&& other) = default;
        Block& operator=(Block&& other) = default;

        static std::unique_ptr<Block> create(std::vector<unique_ptr<Declaration>>&& declarations) {
            return std::unique_ptr<Block>(new Block(std::move(declarations)));
        }

        void print(PrintContext& pc) const;

        size_t size() const {
            return declarations.size();
        }

        const Declaration& operator[](size_t index) const {
            return *declarations[index].get();
        }
    };

    // Expressions
    
    template <typename Subclass, typename ReturnType>
    class ExpressionVisitorT;

    class Expression : public Node {
    public:
        template <typename Subclass, typename ReturnType>
        ReturnType acceptVisitor(ExpressionVisitorT<Subclass, ReturnType>& visitor);

        Type *getType() const {
            return type;
        }

        void setType(Type *type) {
            this->type = type;
        }
    protected:
        using Node::Node;

        Type *type = nullptr;

        static bool classof(const Node *node) {
            return node->getKind() >= NK_Expr_Identifier && node->getKind() <= NK_Expr_Call;
        }
    };

    class Identifier : public Expression {
    protected:
        std::string name;

        Identifier(Token token) : Expression{NK_Expr_Identifier, token}, name{token.chars} {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<Identifier> create(Token token) {
            return unique_ptr<Identifier>{new Identifier(token)};
        }

        const std::string& getName() const {
            return name;
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Expr_Identifier;
        }
    };

    class Literal : public Expression {
    public:
        enum class Type {
            Boolean,
            Integer,
            Double,
            String,
        };
    protected:
        // Maybe use std::variant
        // TODO: Implement this using APInt from llvm
        union Internal {
            bool boolean;
            uint64_t integer;
            double double_;
            std::string* string;
        };

        Type type;
        Internal internal;

        virtual void print(PrintContext& pc) const override;
    public:
        explicit Literal(Token token, bool boolean) : Expression{NK_Expr_Literal, Location{token}}, type{Type::Boolean}, internal{.boolean=boolean} {}
        explicit Literal(Token token, uint64_t integer) : Expression{NK_Expr_Literal, Location{token}}, type{Type::Integer}, internal{.integer=integer} {}
        explicit Literal(Token token, double double_) : Expression{NK_Expr_Literal, Location{token}}, type{Type::Double}, internal{.double_=double_} {}
        explicit Literal(Token token, std::unique_ptr<std::string>&& string) : Expression{NK_Expr_Literal, Location{token}}, type{Type::String}, internal{.string=string.release()} {}

        template <typename T>
        static unique_ptr<Literal> create(Token token, T value) {
            return unique_ptr<Literal>{new Literal(token, value)};
        }

        static unique_ptr<Literal> create(Token token, std::unique_ptr<std::string> value) {
            return unique_ptr<Literal>{new Literal(token, std::move(value))};
        }

        Literal::Type getLiteralType() const {
            return type;
        }

        bool getBoolean() const {
            return internal.boolean;
        }

        uint64_t getInteger() const {
            return internal.integer;
        }

        double getDouble() const {
            return internal.double_;
        }

        std::string *getString() const {
            return internal.string;
        }

        ~Literal() {
            if (type == Type::String) {
                delete internal.string;
            }
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Expr_Literal;
        }
    };

    class CallExpression : public Expression {
        unique_ptr<Expression> target;
        std::vector<unique_ptr<Expression>> arguments;

    protected:
        CallExpression(Token token, unique_ptr<Expression>&& target, std::vector<unique_ptr<Expression>>&& arguments) 
            : Expression{NK_Expr_Call, Location{token}}
            , target{std::move(target)}
            , arguments{std::move(arguments)} {}

        virtual void print(PrintContext& pc) const override;
        CallExpression(const CallExpression&) = delete;
        CallExpression& operator=(const CallExpression&) = delete;
    public:
        static unique_ptr<CallExpression> create(Token token, unique_ptr<Expression>&& target, std::vector<unique_ptr<Expression>>&& arguments) {
            return unique_ptr<CallExpression>{new CallExpression(token, std::move(target), std::move(arguments))};
        }

        Expression& getTarget() const {
            return *target;
        }

        int argumentCount() const {
            return arguments.size();
        }

        Expression& getArgument(size_t i) const {
            return *arguments[i];
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Expr_Call;
        }
    };

    enum class UnaryOperator {
        Negate,
        Not,
    };

    class UnaryExpression : public Expression {
    protected:
        UnaryOperator op;
        unique_ptr<Expression> target;

        UnaryExpression(Token token, UnaryOperator op, unique_ptr<Expression>&& target) 
            : Expression{NK_Expr_Unary, token}, op{op}, target{std::move(target)}
        {}

        void print(PrintContext& pc) const override;
    public:
        static unique_ptr<UnaryExpression> create(Token token, UnaryOperator op, unique_ptr<Expression>&& target) {
            return unique_ptr<UnaryExpression>{new UnaryExpression(token, op, std::move(target))};
        }

        UnaryOperator getOp() const {
            return op;
        }

        Expression &getTarget() const {
            return *target;
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Expr_Unary;
        }
    };

    enum class BinaryOperator {
        Add,
        Subtract,
        Multiply,
        Divide,
        Modulo,

        Equal,
        NotEqual,
        Less,
        LessEqual,
        Greater,
        GreaterEqual,

        LogicalAnd,
        LogicalOr,
    };

    class BinaryExpression : public Expression {
    protected:
        BinaryOperator op;
        unique_ptr<Expression> left;
        unique_ptr<Expression> right;
        BinaryExpression(Token token, BinaryOperator op, unique_ptr<Expression> left, unique_ptr<Expression> right) 
            : Expression{NK_Expr_Binary, Location{token}}, op{op}, left{std::move(left)}, right{std::move(right)} {}

        void print(PrintContext& pc) const override;
    public:
        static unique_ptr<BinaryExpression> create(Token token, BinaryOperator op, unique_ptr<Expression>&& left, unique_ptr<Expression>&& right) {
            return unique_ptr<BinaryExpression>{new BinaryExpression(token, op, std::move(left), std::move(right))};
        }

        BinaryOperator getOp() const {
            return op;
        }
        
        Expression& getLeft() const {
            return *left;
        }
        void setLeft(Expression& left) {
            this->left.reset(&left);
        }

        Expression& getRight() const {
            return *right;
        }
        void setRight(Expression& right) {
            this->right.reset(&right);
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Expr_Binary;
        }
    };

    // Statements
    template <typename Subclass, typename ReturnType>
    class StatementVisitorT;

    class Statement : public Node {
    public:
        template <typename Subclass, typename ReturnType>
        ReturnType acceptVisitor(StatementVisitorT<Subclass, ReturnType>& visitor);
    protected:
        using Node::Node;
    };

    enum class AssignmentOperator {
        Assign,
        AssignAdd,
        AssignSub,
        AssignMultiply,
        AssignDivide,
    };

    class AssignmentStatement : public Statement {
    protected:
        AssignmentOperator op;
        unique_ptr<Expression> target;
        unique_ptr<Expression> value;

        AssignmentStatement(Token token, AssignmentOperator op, unique_ptr<Expression> target, unique_ptr<Expression> value)
            : Statement{NK_Stmt_Assignment, Location{token}}
            , op{op}
            , target{std::move(target)}
            , value{std::move(value)} 
        {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<AssignmentStatement> create(Token token, AssignmentOperator op, unique_ptr<Expression> target, unique_ptr<Expression> value) {
            return unique_ptr<AssignmentStatement>{new AssignmentStatement(token, op, std::move(target), std::move(value))};
        }
    };

    class IfStatement : public Statement {
    public:
        class Branch {
            unique_ptr<Expression> condition;
            Block block;

        public:
            Branch(unique_ptr<Expression>&& condition, Block&& block) : condition{std::move(condition)}, block{std::move(block)} {}

            friend class IfStatement;
        };
    protected:
        std::vector<Branch> conditionals;
        std::optional<Block> fallback;

        // NOTE: this would be safer if it took a conditional, and a vector of subsequent ones, but initialization becomes more troublesome that way.
        IfStatement(Token token, std::vector<Branch>&& conditionals, std::optional<Block>&& fallback) 
            : Statement{NK_Stmt_If, token}
            , conditionals{std::move(conditionals)}
            , fallback{std::move(fallback)}
        {
            assert(this->conditionals.size() > 0 && "if statement must have at least one condition.");
        }

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<IfStatement> create(Token token, std::vector<Branch>&& conditionals, std::optional<Block>&& fallback) {
            return unique_ptr<IfStatement>{new IfStatement(token, std::move(conditionals), std::move(fallback))};
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Stmt_If;
        }
    };

    class ReturnStatement : public Statement {
    protected:
        unique_ptr<Expression> expression;

        ReturnStatement(Token token, unique_ptr<Expression>&& expression) : Statement{NK_Stmt_Return, token}, expression{std::move(expression)} {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<ReturnStatement> create(Token token, unique_ptr<Expression> expression) {
            return unique_ptr<ReturnStatement>{new ReturnStatement(token, std::move(expression))};
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Stmt_Return;
        }
    };

    class WhileStatement : public Statement {
    protected:
        unique_ptr<Expression> condition;
        Block code;

        WhileStatement(Token token, unique_ptr<Expression>&& condition, Block&& code) 
            : Statement{NK_Stmt_While, token}
            , condition{std::move(condition)}
            , code{std::move(code)} 
        {}
    
        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<WhileStatement> create(Token token, unique_ptr<Expression>&& condition, Block&& code) {
            return unique_ptr<WhileStatement>{new WhileStatement(token, std::move(condition), std::move(code))};
        }

        const Expression& getCondition() const {
            return *condition;
        }

        const Block& getBlock() const {
            return code;
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Stmt_While;
        }
    };

    class ForStatement : public Statement {
    protected:
        unique_ptr<Declaration> initialization;
        unique_ptr<Expression> condition;
        unique_ptr<Statement> increment;
        Block code;

        ForStatement(Token token, unique_ptr<Declaration> initialization, unique_ptr<Expression> condition, unique_ptr<Statement> increment, Block&& code)
            : Statement{NK_Stmt_For, token}
            , initialization{std::move(initialization)}
            , condition{std::move(condition)}
            , increment{std::move(increment)}
            , code{std::move(code)}
        {}

        virtual void print(PrintContext& pc) const override;

    public:
        static unique_ptr<ForStatement> create(Token token, unique_ptr<Declaration> initialization, unique_ptr<Expression> condition, unique_ptr<Statement> increment, Block&& code) {
            return unique_ptr<ForStatement>{new ForStatement(token, std::move(initialization), std::move(condition), std::move(increment), std::move(code))};
        }

        const Declaration *getInitialization() const {
            return initialization.get();
        }

        const Expression& getCondition() const {
            return *condition;
        }

        const Statement *getIncrement() const {
            return increment.get();
        }
    };

    class ExpressionStatement : public Statement {
    protected:
        unique_ptr<Expression> expression;

        ExpressionStatement(unique_ptr<Expression>&& expression) : Statement{NK_Stmt_Expression, expression->getLocation()}, expression{std::move(expression)} {}

        virtual void print(PrintContext& pc) const override { expression->print(pc); }
    public:
        static unique_ptr<ExpressionStatement> create(unique_ptr<Expression>&& expression) {
            return unique_ptr<ExpressionStatement>{new ExpressionStatement{std::move(expression)}};
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Stmt_Expression;
        }
    };

    // Declarations
    
    template <typename Subclass, typename ReturnType>
    class DeclarationVisitorT;

    class Declaration : public Node {
    public:
        template <typename Subclass, typename ReturnType>
        ReturnType acceptVisitor(DeclarationVisitorT<Subclass, ReturnType>& visitor);
    protected:
        using Node::Node;

        static bool classof(const Node *node) {
            // TODO: Ensure starting kind is correct
            return node->getKind() >= NK_Decl_Variable && node->getKind() <= NK_Decl_Statement;
        }
    };

    class VariableDeclaration : public Declaration {
    protected:
        std::string identifier;
        Type* type = nullptr;
        unique_ptr<TypeNode> typeDeclaration;
        unique_ptr<Expression> initial;
        bool isMutable;

        VariableDeclaration(
            Token token, 
            bool isMutable,
            std::string&& identifier, 
            unique_ptr<TypeNode>&& type, 
            unique_ptr<Expression>&& initial
        ) : Declaration{NK_Decl_Variable, token}, isMutable{isMutable}, identifier{std::move(identifier)}, typeDeclaration{std::move(type)}, initial{std::move(initial)} {}
        
        virtual void print(PrintContext& pc) const override;

    public:
        static unique_ptr<VariableDeclaration> create(
            Token token, 
            bool isMutable,
            std::string&& identifier, 
            unique_ptr<TypeNode>&& type, 
            unique_ptr<Expression>&& initial
        ) {
            return unique_ptr<VariableDeclaration>(new VariableDeclaration(token, isMutable, std::move(identifier), std::move(type), std::move(initial)));
        }

        const std::string& getName() const {
            return identifier;
        }

        Expression *getInitialValue() const {
            return initial.get();
        }

        TypeNode *getTypeDeclaration() const {
            return typeDeclaration.get();
        }

        Type *getType() const {
            return type;
        }

        void setType(Type& type) {
            this->type = &type;
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Decl_Variable;
        }
    };

    class FunctionDeclaration : public Declaration {
    public:
        class Parameter {
        public:
            std::string name;
            unique_ptr<TypeNode> typeDeclaration;
            Type *type = nullptr;
            
            Parameter(std::string_view& name, unique_ptr<TypeNode>&& type) : name{name}, typeDeclaration{std::move(type)} {}
            //Parameter(Parameter&& parameter) : name{std::move(parameter.name)}, type{std::move(parameter.type)} {}
            //Parameter& operator=(Parameter&& parameter) = default;
        };
    protected:
        std::string name;
        std::vector<Parameter> parameters;
        int arity;
        unique_ptr<TypeNode> returnTypeDeclaration;
        Type *returnType;
        std::vector<unique_ptr<Declaration>> declarations;

        FunctionDeclaration(Token token, std::vector<Parameter>&& parameters, unique_ptr<TypeNode>&& returnType, std::vector<unique_ptr<Declaration>>&& declarations) 
            : Declaration{NK_Decl_Function, Location{token}}
            , name{token.chars}
            , parameters{std::move(parameters)}
            , arity{int(this->parameters.size())}
            , returnTypeDeclaration{std::move(returnType)}
            , declarations{std::move(declarations)} {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<FunctionDeclaration> create(Token token, std::vector<Parameter>&& parameters, unique_ptr<TypeNode>&& returnType, std::vector<unique_ptr<Declaration>>&& declarations) {
            return unique_ptr<FunctionDeclaration>{new FunctionDeclaration(token, std::move(parameters), std::move(returnType), std::move(declarations))};
        }

        const std::string& getName() const {
            return name;
        }

        TypeNode *getReturnTypeDeclaration() const {
            return returnTypeDeclaration.get();
        }

        void setReturnType(Type& type) {
            returnType = &type;
        }

        int getParameterCount() const {
            return parameters.size();
        }

        Parameter& getParameter(int i) {
            return parameters[i];
        }

        const Parameter& getParameter(int i) const {
            return parameters[i];
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Decl_Function;
        }
    };

    class StructDeclaration : public Declaration {
    public:
        class Field {
            std::string name;
            unique_ptr<TypeNode> typeDeclaration;
            Type *type;

            Field(std::string_view& name, unique_ptr<TypeNode>&& type) : name{name}, typeDeclaration{std::move(type)} {}
        };

    protected:
        std::string name;
        std::vector<Field> fields;
        std::vector<unique_ptr<FunctionDeclaration>> methods;

        StructDeclaration(Token token, std::string_view& name, std::vector<Field>&& fields, std::vector<unique_ptr<FunctionDeclaration>>&& methods)
            : Declaration{NK_Decl_Struct, Location{token}}
            , name{name}
            , fields{std::move(fields)}
            , methods{std::move(methods)} {}
    public:
        static unique_ptr<StructDeclaration> create(Token token, std::string_view& name, std::vector<Field>&& fields, std::vector<unique_ptr<FunctionDeclaration>>&& methods) {
            return unique_ptr<StructDeclaration>{new StructDeclaration(token, name, std::move(fields), std::move(methods))};
        }

        virtual void print(PrintContext& pc) const override;

        const std::string& getName() const {
            return name;
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Decl_Struct;
        }
    };

    class ClassDeclaration : public Declaration {
    public:
        class Field {
            std::string name;
            unique_ptr<TypeNode> typeDeclaration;
            Type *type;

            Field(std::string_view& name, unique_ptr<TypeNode>&& type) : name{name}, typeDeclaration{std::move(type)} {}
        };

        virtual void print(PrintContext& pc) const override;
    };


    class EnumDeclaration : public Declaration {
    protected:
        std::string name;

        virtual void print(PrintContext& pc) const override;
    };
    
    class StatementDeclaration : public Declaration {
    protected:
        unique_ptr<Statement> statement;

        StatementDeclaration(unique_ptr<Statement>&& statement) : Declaration{NK_Decl_Statement, statement.get()->getLocation()}, statement{std::move(statement)} {}

        virtual void print(PrintContext& pc) const override; 
    public:
        static unique_ptr<StatementDeclaration> create(unique_ptr<Statement>&& statement) {
            return unique_ptr<StatementDeclaration>(new StatementDeclaration(std::move(statement)));
        }
        
        static bool classof(const Node *node) {
            return node->getKind() == NK_Decl_Statement;
        }
    };

    class ProtocolDeclaration : public Declaration {
    protected:
        std::string name;

        virtual void print(PrintContext& pc) const override;
    public:
        const std::string& getName() { return name; }
    };
};

namespace AST {
    class PrintContext {
        std::ostream& os;
        int indentWidth;
        int indentLevel;

        PrintContext(std::ostream& os, int indentWidth = 4) : os{os}, indentWidth{indentWidth}, indentLevel{0} {}

        PrintContext(const PrintContext&) = delete;
        PrintContext& operator=(const PrintContext&) = delete;
    public:
        void indent() {
            ++indentLevel;
        }

        void outdent() {
            --indentLevel;
        }

        void startLine() {
            int nIndent = indentWidth * indentLevel;
            std::fill_n(std::ostream_iterator<char>(os), nIndent, ' ');
        }

        PrintContext& operator<<(const Node& value);

        PrintContext& operator<<(const std::string& str) {
            os << str;
            return *this;
        }

        PrintContext& operator<<(const char *str) {
            os << str;
            return *this;
        }

        template <typename T>
        PrintContext& operator<<(T value) requires std::is_arithmetic_v<T> {
            os << value;
            return *this;
        }

        friend class Node;
    };
}

#endif // LANG_ast_h
