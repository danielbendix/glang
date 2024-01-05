#ifndef LANG_ast_h
#define LANG_ast_h

#include "scanner.h" // Token, TokenType
#include "unique_ref.h"

#include <string>
#include <vector>
#include <cassert>

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
    class Visitor;
    class PrintContext;

    struct Location {
        // TODO: SourceFile reference
        int line;
        int offset;

        Location(Token token) : line{token.line}, offset{token.offset} {}
    };

    class Node {
    protected:
        Location location;
        Node(Location location) : location{location} {}
    public:
        Node(const Node&) = delete;
        Node& operator=(const Node&) = delete;
        virtual ~Node() = default;

        Location getLocation() { return location; }

        void print(std::ostream& os) const;

        virtual void acceptVisitor(Visitor& visitor) = 0;
        virtual void print(PrintContext& pc) const = 0;
    };
}

std::ostream& operator<<(std::ostream& os, const AST::Node& node);
std::ostream& operator<<(std::ostream& os, AST::Node& node);

namespace AST {

    // Types

    class Type : public Node {
    protected:
        using Node::Node;
    };

    class TypeLiteral : public Type {
    protected:
        std::string identifier;

        TypeLiteral(Token name) : Type{Location{name}}, identifier{name.chars} {}

        virtual void print(PrintContext& pc) const;
        virtual void acceptVisitor(Visitor& visitor) {}

    public:
        static unique_ptr<TypeLiteral> create(Token name) {
            return unique_ptr<TypeLiteral>(new TypeLiteral(name));
        }

        virtual ~TypeLiteral() = default;
    };

    // Utilities
    
    class Declaration;
    class Block final {
    protected:
        std::vector<unique_ptr<Declaration>> declarations;

    public:
        Block(std::vector<unique_ptr<Declaration>>&& declarations) : declarations{std::move(declarations)} {}

        Block(Block&& other) = default;
        Block& operator=(Block&& other) = default;
        Block(const Block&) = delete;
        Block& operator=(const Block&) = delete;
        static unique_ptr<Block> create(std::vector<unique_ptr<Declaration>>&& declarations) {
            return unique_ptr<Block>(new Block(std::move(declarations)));
        }

        size_t size() const {
            return declarations.size();
        }

        const Declaration& operator[](size_t index) const {
            return *declarations[index].get();
        }

        void print(PrintContext& pc) const;
    };

    // Expressions

    class Expression : public Node {
    protected:
        using Node::Node;
    };

    class Identifier : public Expression {
    protected:
        std::string name;

        Identifier(Token token) : Expression{token}, name{token.chars} {}

        virtual void print(PrintContext& pc) const;
        virtual void acceptVisitor(Visitor& visitor) {};
    public:
        static unique_ptr<Identifier> create(Token token) {
            return unique_ptr<Identifier>{new Identifier(token)};
        }
    };

    class Literal : public Expression {
        enum class Type {
            Boolean,
            Int8,
            Int16,
            Int32,
            Int64,
            UInt8,
            UInt16,
            UInt32,
            UInt64,
            Float,
            Double,
            String,
        };

        union Internal {
            bool boolean;
            int8_t int8;
            int16_t int16;
            int32_t int32;
            int64_t int64;
            uint8_t uint8;
            uint16_t uint16;
            uint32_t uint32;
            uint64_t uint64;
            float float_;
            double double_;
            std::string* string;
        };

        Type type;
        Internal internal;

        virtual void acceptVisitor(Visitor& visitor) {};
        virtual void print(PrintContext& pc) const;
    public:
        Literal(Token token, bool boolean) : Expression{Location{token}}, type{Type::Boolean}, internal{.boolean=boolean} {}
        Literal(Token token, int8_t int8) : Expression{Location{token}}, type{Type::Int8}, internal{.int8=int8} {}
        Literal(Token token, int16_t int16) : Expression{Location{token}}, type{Type::Int16}, internal{.int16=int16} {}
        Literal(Token token, int32_t int32) : Expression{Location{token}}, type{Type::Int32}, internal{.int32=int32} {}
        Literal(Token token, int64_t int64) : Expression{Location{token}}, type{Type::Int64}, internal{.int64=int64} {}
        Literal(Token token, uint8_t uint8) : Expression{Location{token}}, type{Type::UInt8}, internal{.uint8=uint8} {}
        Literal(Token token, uint16_t uint16) : Expression{Location{token}}, type{Type::UInt16}, internal{.uint16=uint16} {}
        Literal(Token token, uint32_t uint32) : Expression{Location{token}}, type{Type::UInt32}, internal{.uint32=uint32} {}
        Literal(Token token, uint64_t uint64) : Expression{Location{token}}, type{Type::UInt64}, internal{.uint64=uint64} {}
        Literal(Token token, float float_) : Expression{Location{token}}, type{Type::Float}, internal{.float_=float_} {}
        Literal(Token token, double double_) : Expression{Location{token}}, type{Type::Double}, internal{.double_=double_} {}
        Literal(Token token, std::string&& string) : Expression{Location{token}}, type{Type::String}, internal{.string=new std::string(std::move(string))} {}

        template <typename T>
        static unique_ptr<Literal> create(Token token, T value) {
            return unique_ptr<Literal>{new Literal(token, value)};
        }

        static unique_ptr<Literal> create(Token token, std::string&& value) {
            return unique_ptr<Literal>{new Literal(token, std::move(value))};
        }

        virtual ~Literal() {
            if (type == Type::String) {
                delete internal.string;
            }
        }
    };

    class CallExpression : public Expression {
        Expression *callee;
        std::vector<Expression *> arguments;

    protected:
        CallExpression(Token token, Expression& callee) : Expression{Location{token}}, callee{&callee} {}
    public:
        CallExpression(const CallExpression&) = delete;
        CallExpression& operator=(const CallExpression&) = delete;
    };

    enum class BinaryOperator {
        Add,
        Subtract,
        Multiply,
        Divide,
        Modulo,

        Equal,
        Less,
        LessEqual,
        Greater,
        GreaterEqual,
    };

    class BinaryExpression : public Expression {
    protected:
        BinaryOperator op;
        unique_ptr<Expression> left;
        unique_ptr<Expression> right;
        BinaryExpression(Token token, BinaryOperator op, unique_ptr<Expression> left, unique_ptr<Expression> right) 
            : Expression{Location{token}}, op{op}, left{std::move(left)}, right{std::move(right)} {}

        void print(PrintContext& pc) const;
        void acceptVisitor(Visitor& visitor) {}
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
    };

    // Statements
    class Block;

    class Statement : public Node {
    protected:
        using Node::Node;
    };

    class AssignmentStatement : public Statement {
    protected:
        unique_ptr<Expression> target;
        unique_ptr<Expression> value;

        AssignmentStatement(Token token, unique_ptr<Expression> target, unique_ptr<Expression> value)
            : Statement{Location{token}}
            , target{std::move(target)}
            , value{std::move(value)} 
        {}

        virtual void print(PrintContext& os) const;
        virtual void acceptVisitor(Visitor& visitor) {}

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
            : Statement{token}
            , conditionals{std::move(conditionals)}
            , fallback{std::move(fallback)}
        {
            assert(this->conditionals.size() > 0 && "if statement must have at least one condition.");
        }

        virtual void print(PrintContext& pc) const;
        virtual void acceptVisitor(Visitor& visitor) {}

    public:
        static unique_ptr<IfStatement> create(Token token, std::vector<Branch>&& conditionals, std::optional<Block>&& fallback) {
            return unique_ptr<IfStatement>{new IfStatement(token, std::move(conditionals), std::move(fallback))};
        }
    };

    class ReturnStatement : public Statement {
    protected:
        unique_ptr<Expression> expression;

        ReturnStatement(Token token, unique_ptr<Expression>&& expression) : Statement{token}, expression{std::move(expression)} {}

        virtual void print(PrintContext& pc) const;
        virtual void acceptVisitor(Visitor& visitor) {}
    public:
        static unique_ptr<ReturnStatement> create(Token token, unique_ptr<Expression> expression) {
            return unique_ptr<ReturnStatement>{new ReturnStatement(token, std::move(expression))};
        }
    };

    class ExpressionStatement : public Statement {
    protected:
        unique_ptr<Expression> expression;

        ExpressionStatement(unique_ptr<Expression>&& expression) : Statement{expression->getLocation()}, expression{std::move(expression)} {}

        virtual void print(PrintContext& pc) const { expression->print(pc); }
        virtual void acceptVisitor(Visitor& visitor) {}
    public:
        static unique_ptr<ExpressionStatement> create(unique_ptr<Expression>&& expression) {
            return unique_ptr<ExpressionStatement>{new ExpressionStatement{std::move(expression)}};
        }
    };

    // Declarations

    class Declaration : public Node {
    protected:
        using Node::Node;
    };

    class FunctionDeclaration : public Declaration {
    public:
        class Parameter {
        public:
            std::string name;
            unique_ptr<Type> type;
            
            Parameter() {}
            Parameter(std::string_view& name, unique_ptr<Type>&& type) : name{name}, type{std::move(type)} {}
            //Parameter(Parameter&& parameter) = default;
            //Parameter& operator=(Parameter&& parameter) = default;
        };
    private:
        std::string name;
        std::vector<Parameter> parameters;
        int arity;
        unique_ptr<Type> returnType;
        std::vector<unique_ptr<Declaration>> declarations;

        FunctionDeclaration(Token token, std::vector<Parameter>&& parameters, unique_ptr<Type>&& returnType, std::vector<unique_ptr<Declaration>>&& declarations) 
            : Declaration{Location{token}}
            , name{token.chars}
            , parameters{std::move(parameters)}
            , arity{int(this->parameters.size())}
            , returnType{std::move(returnType)}
            , declarations{std::move(declarations)} {}

        virtual void acceptVisitor(Visitor& visitor) {}
        virtual void print(PrintContext& pc) const;
    public:
        static unique_ptr<FunctionDeclaration> create(Token token, std::vector<Parameter>&& parameters, unique_ptr<Type>&& returnType, std::vector<unique_ptr<Declaration>>&& declarations) {
            return unique_ptr<FunctionDeclaration>{new FunctionDeclaration(token, std::move(parameters), std::move(returnType), std::move(declarations))};
        }

    };

    class StructDeclaration : public Declaration {

    };

    class EnumDeclaration : public Declaration {
    protected:
        std::string identifier;

    };
    
    class VarDeclaration : public Declaration {
    protected:
        std::string identifier;
        unique_ptr<Type> type;
        unique_ptr<Expression> value;

        VarDeclaration(
            Token token, 
            std::string&& identifier, 
            unique_ptr<Type>&& type, 
            unique_ptr<Expression>&& value
        ) : Declaration{token}, identifier{std::move(identifier)}, type{std::move(type)}, value{std::move(value)} {}
        
        virtual void print(PrintContext& pc) const;
        virtual void acceptVisitor(Visitor& visitor) {}

    public:
        static unique_ptr<VarDeclaration> create(
            Token token, 
            std::string&& identifier, 
            unique_ptr<Type>&& type, 
            unique_ptr<Expression>&& value
        ) {
            return unique_ptr<VarDeclaration>(new VarDeclaration(token, std::move(identifier), std::move(type), std::move(value)));
        }
    };

    class StatementDeclaration : public Declaration {
    protected:
        unique_ptr<Statement> statement;

        StatementDeclaration(unique_ptr<Statement>&& statement) : Declaration{statement.get()->getLocation()}, statement{std::move(statement)} {}

        virtual void acceptVisitor(Visitor& visitor) {}
        virtual void print(PrintContext& pc) const; 
    public:
        static unique_ptr<StatementDeclaration> create(unique_ptr<Statement>&& statement) {
            return unique_ptr<StatementDeclaration>(new StatementDeclaration(std::move(statement)));
        }
    };

    class ProtocolDeclaration : public Declaration {

    };

    class ClassDeclaration : public Declaration {

    };


    class Visitor {
        virtual void visitDeclaration(Declaration& declaration) = 0;
        virtual void visitFunctionDeclaration(FunctionDeclaration& declaration) = 0;
        virtual void visitStructDeclaration(StructDeclaration& declaration) = 0;
        virtual void visitStatement(Statement& statement) = 0;
        virtual void visitExpressionStatement(ExpressionStatement& expressionStatement) = 0;

        virtual void visitLiteral(Literal& value) = 0;
        virtual void visitCallExpression(CallExpression& callExpression) = 0;
        virtual void visitBinaryExpression(BinaryExpression& binaryExpression) = 0;
    };
};

namespace AST {
    class Node;
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
            //for (int i = 0; i < nIndent; i++) {
            //    os << ' ';
            //}
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

#endif
