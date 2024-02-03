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
 */

using std::unique_ptr;


namespace AST {
    class PrintContext;

    struct Location {
        // TODO: SourceFile reference
        int line;
        int offset;

        Location(Token token) : line{token.line}, offset{token.offset} {}
    };

    class Node {
    protected:
        enum Kind {
            // Declarations
            NK_Decl_Function,
            NK_Decl_Struct,
            NK_Decl_Class,
            NK_Decl_Variable,
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
        virtual ~Node() = default;

        Kind getKind() const { return kind; }
        Location getLocation() const { return location; }

        void acceptVisitor();

        void print(std::ostream& os) const;

        virtual void print(PrintContext& pc) const = 0;
    };
}

std::ostream& operator<<(std::ostream& os, const AST::Node& node);
std::ostream& operator<<(std::ostream& os, AST::Node& node);

namespace AST {

    // Types

    class TypeNode : public Node {
    protected:
        using Node::Node;
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

        virtual ~TypeLiteral() = default;

        static bool classof(const Node *node) {
            return node->getKind() == NK_Type_Literal;
        }
    };

    // TODO: Replce with PointerUnion from LLVM
    class TypeRef {
        llvm::PointerUnion<TypeNode *, Type *> internal;

    public:
        ~TypeRef() {
            if (TypeNode *p = internal.dyn_cast<TypeNode *>()) {
                delete p;
            }
        }

        TypeRef(TypeNode *node) : internal{} {
            internal = node;
        }
        TypeRef(unique_ptr<TypeNode>&& node) : internal{} {
            internal = node.release();
        }

        void setType(Type *type) {
            if (TypeNode *p = internal.dyn_cast<TypeNode *>()) {
                delete p;
            }
        }

        TypeNode *node() const {
            return internal.get<TypeNode *>();
        }

        Type *type() const {
            return internal.get<Type *>();
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

        static unique_ptr<Block> create(std::vector<unique_ptr<Declaration>>&& declarations) {
            return unique_ptr<Block>(new Block(std::move(declarations)));
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
    
    class ExpressionVisitor;

    template <typename Subclass, typename ReturnType>
    class ExpressionVisitorT;

    class Expression : public Node {
    public:
        virtual void acceptVisitor(ExpressionVisitor& visitor) = 0;
    protected:
        using Node::Node;

        template <typename Subclass, typename ReturnType>
        ReturnType acceptVisitor(ExpressionVisitorT<Subclass, ReturnType>& visitor);

        static bool classof(const Node *node) {
            return node->getKind() >= NK_Expr_Identifier && node->getKind() <= NK_Expr_Call;
        }
    };

    class Identifier : public Expression {
    protected:
        std::string name;

        Identifier(Token token) : Expression{NK_Expr_Identifier, token}, name{token.chars} {}

        virtual void print(PrintContext& pc) const override;
        virtual void acceptVisitor(ExpressionVisitor& visitor) override;
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
        virtual void acceptVisitor(ExpressionVisitor& visitor) override;

    public:
        explicit Literal(Token token, bool boolean) : Expression{NK_Expr_Literal, Location{token}}, type{Type::Boolean}, internal{.boolean=boolean} {}
        explicit Literal(Token token, uint64_t integer) : Expression{NK_Expr_Literal, Location{token}}, type{Type::Integer}, internal{.integer=integer} {}
        explicit Literal(Token token, double double_) : Expression{NK_Expr_Literal, Location{token}}, type{Type::Double}, internal{.double_=double_} {}
        explicit Literal(Token token, unique_ptr<std::string>&& string) : Expression{NK_Expr_Literal, Location{token}}, type{Type::String}, internal{.string=string.release()} {}

        template <typename T>
        static unique_ptr<Literal> create(Token token, T value) {
            return unique_ptr<Literal>{new Literal(token, value)};
        }

        static unique_ptr<Literal> create(Token token, unique_ptr<std::string> value) {
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

        virtual ~Literal() {
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
        virtual void acceptVisitor(ExpressionVisitor& visitor) override;
        CallExpression(const CallExpression&) = delete;
        CallExpression& operator=(const CallExpression&) = delete;
    public:
        static unique_ptr<CallExpression> create(Token token, unique_ptr<Expression>&& target, std::vector<unique_ptr<Expression>>&& arguments) {
            return unique_ptr<CallExpression>{new CallExpression(token, std::move(target), std::move(arguments))};
        }

        Expression *getTarget() const {
            return target.get();
        }

        Expression *getArgument(size_t index) const {
            return arguments.at(index).get();
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
        void acceptVisitor(ExpressionVisitor& visitor) override;
    public:
        static unique_ptr<UnaryExpression> create(Token token, UnaryOperator op, unique_ptr<Expression>&& target) {
            return unique_ptr<UnaryExpression>{new UnaryExpression(token, op, std::move(target))};
        }

        UnaryOperator getOp() const {
            return op;
        }

        Expression *getTarget() const {
            return target.get();
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
        void acceptVisitor(ExpressionVisitor& visitor) override;
    public:
        static unique_ptr<BinaryExpression> create(Token token, BinaryOperator op, unique_ptr<Expression>&& left, unique_ptr<Expression>&& right) {
            return unique_ptr<BinaryExpression>{new BinaryExpression(token, op, std::move(left), std::move(right))};
        }

        BinaryOperator getOp() const {
            return op;
        }
        
        const Expression& getLeft() const {
            return *left;
        }
        void setLeft(Expression& left) {
            this->left.reset(&left);
        }

        const Expression& getRight() const {
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
    class StatementVisitor;

    class Statement : public Node {
    public:
        virtual void acceptVisitor(StatementVisitor& visitor) = 0;
    protected:
        using Node::Node;
    };

    class AssignmentStatement : public Statement {
    protected:
        unique_ptr<Expression> target;
        unique_ptr<Expression> value;

        AssignmentStatement(Token token, unique_ptr<Expression> target, unique_ptr<Expression> value)
            : Statement{NK_Stmt_Assignment, Location{token}}
            , target{std::move(target)}
            , value{std::move(value)} 
        {}

        virtual void print(PrintContext& pc) const override;
        virtual void acceptVisitor(StatementVisitor& visitor) override;
    public:
        static unique_ptr<AssignmentStatement> create(Token token, unique_ptr<Expression> target, unique_ptr<Expression> value) {
            return unique_ptr<AssignmentStatement>{new AssignmentStatement(token, std::move(target), std::move(value))};
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
        virtual void acceptVisitor(StatementVisitor& visitor) override;
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
        virtual void acceptVisitor(StatementVisitor& visitor) override;
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
        virtual void acceptVisitor(StatementVisitor& visitor) override;
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

    class ExpressionStatement : public Statement {
    protected:
        unique_ptr<Expression> expression;

        ExpressionStatement(unique_ptr<Expression>&& expression) : Statement{NK_Stmt_Expression, expression->getLocation()}, expression{std::move(expression)} {}

        virtual void print(PrintContext& pc) const override { expression->print(pc); }
        virtual void acceptVisitor(StatementVisitor& visitor) override;
    public:
        static unique_ptr<ExpressionStatement> create(unique_ptr<Expression>&& expression) {
            return unique_ptr<ExpressionStatement>{new ExpressionStatement{std::move(expression)}};
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Stmt_Expression;
        }
    };

    // Declarations
    
    class DeclarationVisitor;

    class Declaration : public Node {
    public:
        virtual void acceptVisitor(DeclarationVisitor& visitor) = 0;
    protected:
        using Node::Node;

        static bool classof(const Node *node) {
            // TODO: Ensure starting kind is correct
            return node->getKind() >= NK_Decl_Function && node->getKind() <= NK_Decl_Statement;
        }
    };

    class FunctionDeclaration : public Declaration {
    public:
        class Parameter {
        public:
            std::string name;
            unique_ptr<TypeNode> type;
            
            Parameter() {}
            Parameter(std::string_view& name, unique_ptr<TypeNode>&& type) : name{name}, type{std::move(type)} {}
            //Parameter(Parameter&& parameter) = default;
            //Parameter& operator=(Parameter&& parameter) = default;
        };
    private:
        std::string name;
        std::vector<Parameter> parameters;
        int arity;
        unique_ptr<TypeNode> returnType;
        std::vector<unique_ptr<Declaration>> declarations;

        FunctionDeclaration(Token token, std::vector<Parameter>&& parameters, unique_ptr<TypeNode>&& returnType, std::vector<unique_ptr<Declaration>>&& declarations) 
            : Declaration{NK_Decl_Function, Location{token}}
            , name{token.chars}
            , parameters{std::move(parameters)}
            , arity{int(this->parameters.size())}
            , returnType{std::move(returnType)}
            , declarations{std::move(declarations)} {}

        virtual void print(PrintContext& pc) const override;
        virtual void acceptVisitor(DeclarationVisitor& visitor) override;
    public:
        static unique_ptr<FunctionDeclaration> create(Token token, std::vector<Parameter>&& parameters, unique_ptr<TypeNode>&& returnType, std::vector<unique_ptr<Declaration>>&& declarations) {
            return unique_ptr<FunctionDeclaration>{new FunctionDeclaration(token, std::move(parameters), std::move(returnType), std::move(declarations))};
        }

        const std::string& getName() const {
            return name;
        }

        int getParameterCount() const {
            return parameters.size();
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Decl_Function;
        }
    };

    class StructDeclaration : public Declaration {
    public:
        class Field {
            std::string name;
            TypeRef type;

            Field(std::string_view& name, unique_ptr<TypeNode>&& type) : name{name}, type{std::move(type)} {}
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
        virtual void acceptVisitor(DeclarationVisitor& visitor) override;

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
            TypeRef type;

            Field(std::string_view& name, unique_ptr<TypeNode>&& type) : name{name}, type{std::move(type)} {}
        };
    };


    class EnumDeclaration : public Declaration {
    protected:
        std::string identifier;

    };
    
    class VariableDeclaration : public Declaration {
    protected:
        std::string identifier;
        TypeRef type;
        unique_ptr<Expression> initial;
        bool isMutable;

        VariableDeclaration(
            Token token, 
            std::string&& identifier, 
            unique_ptr<TypeNode>&& type, 
            unique_ptr<Expression>&& initial
        ) : Declaration{NK_Decl_Variable, token}, identifier{std::move(identifier)}, type{std::move(type)}, initial{std::move(initial)} {}
        
        virtual void print(PrintContext& pc) const override;
        virtual void acceptVisitor(DeclarationVisitor& visitor) override;

    public:
        static unique_ptr<VariableDeclaration> create(
            Token token, 
            std::string&& identifier, 
            unique_ptr<TypeNode>&& type, 
            unique_ptr<Expression>&& initial
        ) {
            return unique_ptr<VariableDeclaration>(new VariableDeclaration(token, std::move(identifier), std::move(type), std::move(initial)));
        }

        const std::string& getName() const {
            return identifier;
        }

        Expression *getInitialValue() const {
            return initial.get();
        }

        TypeNode *getTypeDeclaration() const {
            return type.node();
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Decl_Variable;
        }
    };

    class StatementDeclaration : public Declaration {
    protected:
        unique_ptr<Statement> statement;

        StatementDeclaration(unique_ptr<Statement>&& statement) : Declaration{NK_Decl_Statement, statement.get()->getLocation()}, statement{std::move(statement)} {}

        virtual void print(PrintContext& pc) const override; 
        virtual void acceptVisitor(DeclarationVisitor& visitor) override;
    public:
        static unique_ptr<StatementDeclaration> create(unique_ptr<Statement>&& statement) {
            return unique_ptr<StatementDeclaration>(new StatementDeclaration(std::move(statement)));
        }
        
        static bool classof(const Node *node) {
            return node->getKind() == NK_Decl_Statement;
        }
    };

    class ProtocolDeclaration : public Declaration {

    };

    // Visitors
    
    class DeclarationVisitor {
    public:
        virtual void visitFunctionDeclaration(FunctionDeclaration& functionDeclaration) = 0;
        virtual void visitVariableDeclaration(VariableDeclaration& variableDeclaration) = 0;
        virtual void visitStatementDeclaration(StatementDeclaration& statementDeclaration) = 0;
        virtual void visitStructDeclaration(StructDeclaration& declaration) = 0;
    };

    class StatementVisitor {
    public:
        virtual void visitIfStatement(IfStatement& ifStatement) = 0;
        virtual void visitAssignmentStatement(AssignmentStatement& assignmentStatement) = 0;
        virtual void visitWhileStatement(WhileStatement& whileStatement) = 0;
        virtual void visitReturnStatement(ReturnStatement& returnStatement) = 0;
        virtual void visitExpressionStatement(ExpressionStatement& expressionStatement) = 0;
    };

    class ExpressionVisitor {
    public:
        virtual void visitUnaryExpression(UnaryExpression& unaryExpression) = 0;
        virtual void visitBinaryExpression(BinaryExpression& binaryExpression) = 0;
        virtual void visitCallExpression(CallExpression& callExpression) = 0;
        virtual void visitIdentifier(Identifier& identifier) = 0;
        virtual void visitLiteral(Literal& literal) = 0;
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
