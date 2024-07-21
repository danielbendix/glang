#ifndef LANG_ast_h
#define LANG_ast_h

#include "common.h"
#include "token.h"
#include "resolution/identifier.h"
#include "resolution/member.h"
#include "unique_ref.h"
#include "type.h"

#include "containers/small_byte_array.h"

#include <string>
#include <vector>
#include <cassert>

#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/APInt.h"
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

using llvm::APInt;

struct DeclarationAttributes {
    bool static_ : 1;
    bool private_ : 1;

    void zero() {
        memset(this, 0, sizeof(DeclarationAttributes));
    }
};

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
            NK_Decl_Protocol,
            NK_Decl_Statement,

            // Statements
            NK_Stmt_Assignment,
            NK_Stmt_Compound_Assignment,
            NK_Stmt_If,
            NK_Stmt_Guard,
            NK_Stmt_Return,
            NK_Stmt_While,
            NK_Stmt_For,
            NK_Stmt_Expression,

            // Expression
            NK_Expr_Identifier,
            NK_Expr_Self,
            NK_Expr_Literal,
            NK_Expr_Unary,
            NK_Expr_Binary,
            NK_Expr_Call,
            NK_Expr_Subscript,
            NK_Expr_Initializer,
            NK_Expr_Member_Access,
            NK_Expr_Inferred_Member_Access,

            // Type nodes
            NK_Type_Literal,
            NK_Type_Modifier,

            // Binding nodes
            NK_Binding_Identifier,
        };

        const Kind kind;
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

        // TODO: Implement lldb output
        //void dump();

        virtual void print(PrintContext& pc) const = 0;

        static void deleteNode(AST::Node *node);
        static void deleteValue(AST::Node *node) { deleteNode(node); }
    };

    template <typename T>
    using unique_ptr = std::unique_ptr<T, Deleter<AST::Node, AST::Node::deleteValue>>;

    template <typename T>
    class iterator {
        using internal = std::vector<unique_ptr<T>>::iterator;
        internal it;

    public:
        iterator(internal it) : it{it} {}

        T& operator*() {
            return **it;
        }

        iterator& operator++() {
            ++it;
            return *this;
        }

        friend bool operator==(const iterator& lhs, const iterator& rhs) {
            return lhs.it == rhs.it;
        }
        friend bool operator!=(const iterator& lhs, const iterator& rhs) {
            return lhs.it != rhs.it;
        }
        friend class Block;
    };

    template <typename T>
    class const_iterator {
        using internal = std::vector<unique_ptr<T>>::const_iterator;
        internal it;

    public:
        const_iterator(internal it) : it{it} {}

        const T& operator*() const {
            return **it;
        }

        const_iterator& operator++() {
            ++it;
            return *this;
        }

        friend bool operator==(const const_iterator& lhs, const const_iterator& rhs) {
            return lhs.it == rhs.it;
        }
        friend bool operator!=(const const_iterator& lhs, const const_iterator& rhs) {
            return lhs.it != rhs.it;
        }
        friend class Block;
    };

    template <typename T>
    class value_iterator {
        using internal = std::vector<T>::iterator;
        internal it;

    public:
        value_iterator(internal it) : it{it} {}

        T& operator*() {
            return *it;
        }

        value_iterator& operator++() {
            ++it;
            return *this;
        }

        friend bool operator==(const value_iterator& lhs, const value_iterator& rhs) {
            return lhs.it == rhs.it;
        }
        friend bool operator!=(const value_iterator& lhs, const value_iterator& rhs) {
            return lhs.it != rhs.it;
        }
        friend class Block;
    };

    template <typename T>
    class const_value_iterator {
        using internal = std::vector<T>::const_iterator;
        internal it;

    public:
        const_value_iterator(internal it) : it{it} {}

        const T& operator*() const {
            return *it;
        }

        const_value_iterator& operator++() {
            ++it;
            return *this;
        }

        friend bool operator==(const const_value_iterator& lhs, const const_value_iterator& rhs) {
            return lhs.it == rhs.it;
        }
        friend bool operator!=(const const_value_iterator& lhs, const const_value_iterator& rhs) {
            return lhs.it != rhs.it;
        }
        friend class Block;
    };
}
std::ostream& operator<<(std::ostream& os, const AST::Node& node);

namespace AST {

    // Types

    template <typename Subclass, typename ReturnType, typename... Args>
    class TypeNodeVisitorT;

    class TypeNode : public Node {
    protected:
        using Node::Node;

        ~TypeNode() = default;
    public:
        template <typename Subclass, typename ReturnType, typename... Args>
        ReturnType acceptVisitor(TypeNodeVisitorT<Subclass, ReturnType, Args...>& visitor, Args... args);

        static bool classof(const Node *node) {
            return node->getKind() >= NK_Type_Literal && node->getKind() <= NK_Type_Modifier;;
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

    class TypeModifier : public TypeNode {
    public:
        enum class Modifier : uint8_t {
            Pointer,
            Optional,
            Location,
            Array,
            UnboundedArray,
        };
    protected:
        unique_ptr<TypeNode> child;
        SmallByteArray<Modifier> modifiers;

        // FIXME Better location
        TypeModifier(unique_ptr<TypeNode>&& child, std::span<Modifier> modifiers) : TypeNode{NK_Type_Modifier, Location{child->location}}, child{std::move(child)}, modifiers{modifiers} {}

        virtual void print(PrintContext& pc) const;
    public:
        static unique_ptr<TypeModifier> create(unique_ptr<TypeNode>&& child, std::span<Modifier> modifiers) {
            return unique_ptr<TypeModifier>{new TypeModifier{std::move(child), modifiers}};
        }

        SmallByteArray<Modifier>::iterator begin() const {
            return modifiers.begin();
        }

        SmallByteArray<Modifier>::iterator end() const {
            return modifiers.end();
        }

        TypeNode& getChild() {
            return *child;
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Type_Modifier;
        }
    };

    // Utilities
    
    class Declaration;
    class Block {
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

        Declaration& operator[](size_t index) const {
            return *declarations[index].get();
        }

        void resize(size_t newSize) {
            assert(newSize < declarations.size());
            declarations.resize(newSize);
        }

        // Iterator

        iterator<Declaration> begin() {
            return iterator<Declaration>(declarations.begin());
        }

        iterator<Declaration> end() {
            return iterator<Declaration>(declarations.end());
        }

        const_iterator<Declaration> begin() const {
            return const_iterator<Declaration>(declarations.cbegin());
        }

        const_iterator<Declaration> end() const {
            return const_iterator<Declaration>(declarations.cend());
        }

        const_iterator<Declaration> cbegin() const {
            return const_iterator<Declaration>(declarations.cbegin());
        }

        const_iterator<Declaration> cend() const {
            return const_iterator<Declaration>(declarations.cend());
        }
    };

    // Expressions
    
    template <typename Subclass, typename ReturnType, typename... Args>
    class ExpressionVisitorT;

    class Expression : public Node {
    public:
        template <typename Subclass, typename ReturnType, typename... Args>
        ReturnType acceptVisitor(ExpressionVisitorT<Subclass, ReturnType, Args...>& visitor, Args... args);

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
            return node->getKind() >= NK_Expr_Identifier && node->getKind() <= NK_Expr_Inferred_Member_Access;
        }
    };

    class Identifier : public Expression {
    protected:
        std::unique_ptr<IdentifierResolution, Deleter<IdentifierResolution>> resolution = nullptr;

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

        IdentifierResolution *getResolution() const {
            return resolution.get();
        }

        void setResolution(std::unique_ptr<IdentifierResolution, Deleter<IdentifierResolution>>&& resolution) {
            this->resolution = std::move(resolution);
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Expr_Identifier;
        }
    };

    class Self : public Expression {
        Self(Token token) : Expression{NK_Expr_Self, token} {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<Self> create(Token token) {
            return unique_ptr<Self>{new Self(token)};
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Expr_Self;
        }
    };

    class Literal : public Expression {
    public:
        enum class Type: uint8_t {
            Boolean = 0,
            Integer,
            Double,
            String,
            Nil,
        };

        enum class IntegerType: uint8_t {
            Binary,
            Octal,
            Decimal,
            Hexadecimal,
        };
    protected:
        using Internal = std::variant<
            bool,
            APInt,
            double,
            std::string,
            std::monostate
        >;
        Internal internal;
        IntegerType integerType;

        virtual void print(PrintContext& pc) const override;

        explicit Literal(Token token, Internal&& internal) : Expression{NK_Expr_Literal, Location{token}}, internal{std::move(internal)} {}
    public:
        explicit Literal(Token token, APInt&& integer, IntegerType integerType) : Literal{token, {std::move(integer)}} {
            this->integerType = integerType;
        }

        static unique_ptr<Literal> create(Token token, bool value) {
            return unique_ptr<Literal>{new Literal(token, value)};
        }

        static unique_ptr<Literal> create(Token token, double value) {
            return unique_ptr<Literal>{new Literal(token, value)};
        }

        static unique_ptr<Literal> create(Token token, APInt&& value, IntegerType integerType) {
            return unique_ptr<Literal>{new Literal(token, std::move(value), integerType)};
        }

        static unique_ptr<Literal> create(Token token, std::string&& value) {
            return unique_ptr<Literal>{new Literal(token, value)};
        }

        static unique_ptr<Literal> createNil(Token token) {
            return unique_ptr<Literal>{new Literal(token, std::monostate{})};
        }

        Literal::Type getLiteralType() const {
            return Literal::Type(internal.index());
        }

        bool getBoolean() const noexcept {
            return std::get<bool>(internal);
        }

        const APInt& getInteger() const {
            return std::get<APInt>(internal);
        }

        double getDouble() const {
            return std::get<double>(internal);
        }

        const std::string& getString() const {
            return std::get<std::string>(internal);
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

        void setWrappedArgument(size_t i, unique_ptr<Expression>&& wrapped) {
            assert(wrapped);
            std::ignore = arguments[i].release();
            arguments[i] = std::move(wrapped);
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Expr_Call;
        }
    };

    class SubscriptExpression : public Expression {
        unique_ptr<Expression> target;
        unique_ptr<Expression> index;
    protected:
        SubscriptExpression(Token token, unique_ptr<Expression>&& target, unique_ptr<Expression>&& index)
            : Expression{NK_Expr_Subscript, token}
            , target{std::move(target)}
            , index{std::move(index)} {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<SubscriptExpression> create(Token token, unique_ptr<Expression>&& target, unique_ptr<Expression>&& index) {
            return unique_ptr<SubscriptExpression>{new SubscriptExpression(token, std::move(target), std::move(index))};
        }

        Expression& getTarget() const {
            return *target;
        }

        Expression& getIndex() const {
            return *index;
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Expr_Subscript;
        }
    };

    class MemberAccessExpression : public Expression {
        unique_ptr<Expression> target;
        std::string memberName;
        unique_ptr_t<MemberResolution> resolution = nullptr;

        virtual void print(PrintContext& pc) const override;
    protected:
        MemberAccessExpression(Token token, unique_ptr<Expression>&& target, Token member) 
            : Expression{NK_Expr_Member_Access, token}
            , target{std::move(target)}
            , memberName{member.chars} {}
    public:
        static unique_ptr<MemberAccessExpression> create(Token token, unique_ptr<Expression>&& target, Token member) {
            return unique_ptr<MemberAccessExpression>{new MemberAccessExpression(token, std::move(target), member)};
        }

        Expression& getTarget() const {
            return *target;
        }

        const std::string& getMemberName() const {
            return memberName;
        }

        const MemberResolution& getResolution() const {
            return *resolution;
        }

        void setResolution(unique_ptr_t<MemberResolution>&& resolution) {
            this->resolution = std::move(resolution);
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Expr_Member_Access;
        }
    };

    class InferredMemberAccessExpression : public Expression {
        std::string memberName;
        unique_ptr_t<MemberResolution> resolution = nullptr;
        Type *inferredTarget = nullptr;

        virtual void print(PrintContext& pc) const override;
    protected:
        InferredMemberAccessExpression(Token token, Token member)
            : Expression{NK_Expr_Inferred_Member_Access, token}
            , memberName{member.chars} {}
    public:
        static unique_ptr<InferredMemberAccessExpression> create(Token token, Token member) {
            return unique_ptr<InferredMemberAccessExpression>{new InferredMemberAccessExpression(token, member)};
        }

        const std::string& getMemberName() const {
            return memberName;
        }

        const MemberResolution& getResolution() const {
            return *resolution;
        }

        void setResolution(unique_ptr_t<MemberResolution>&& resolution) {
            this->resolution = std::move(resolution);
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Expr_Inferred_Member_Access;
        }
    };

    class InitializerExpression : public Expression {
    public:
        using Pair = std::pair<unique_ptr<InferredMemberAccessExpression>, unique_ptr<Expression>>;
    private:       
        // This is a typename. Could contain a type parameter in the future. We should find a better type.
        unique_ptr<Identifier> identifier;
        std::vector<Pair> pairs;
    protected:
        InitializerExpression(Token token, unique_ptr<Identifier>&& identifier, std::vector<Pair>&& pairs) 
            : Expression{NK_Expr_Initializer, token}
            , identifier{std::move(identifier)}
            , pairs{std::move(pairs)}
            {}

        virtual void print(PrintContext& pc) const override;

    public:
        static unique_ptr<InitializerExpression> create(Token token, unique_ptr<Identifier>&& identifier, std::vector<Pair>&& pairs) {
            return unique_ptr<InitializerExpression>{new InitializerExpression(token, std::move(identifier), std::move(pairs))};
        }

        Identifier *getIdentifier() {
            return identifier.get();
        }

        size_t getNumberOfPairs() {
            return pairs.size();
        }

        Pair& getPair(size_t index) {
            return pairs[index];
        }
    };


    enum class UnaryOperator {
        Negate,
        BitwiseNegate,
        Not,
        AddressOf,
        Dereference,
        ZeroExtend,
        SignExtend,
        IntegerToFP,
        FPExtend,
        OptionalWrap, ///< wrap in implicit .some
    };

    class UnaryExpression : public Expression {
    protected:
        UnaryOperator op;
        unique_ptr<Expression> target;

        UnaryExpression(Token token, UnaryOperator op, unique_ptr<Expression>&& target) 
            : Expression{NK_Expr_Unary, token}, op{op}, target{std::move(target)}
        {}

        UnaryExpression(Location location, UnaryOperator op, unique_ptr<Expression>&& target) 
            : Expression{NK_Expr_Unary, location}, op{op}, target{std::move(target)}
        {}

        void print(PrintContext& pc) const override;
    public:
        static unique_ptr<UnaryExpression> create(Token token, UnaryOperator op, unique_ptr<Expression>&& target) {
            return unique_ptr<UnaryExpression>{new UnaryExpression(token, op, std::move(target))};
        }

        static unique_ptr<UnaryExpression> wrap(Expression& target, UnaryOperator op, Type& type) {
            auto result = unique_ptr<UnaryExpression>{
                new UnaryExpression(target.location, op, unique_ptr<Expression>(&target))
            };
            result->setType(&type);
            return result;
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

        ShiftLeft,
        ShiftRight,

        BitwiseAnd,
        BitwiseOr,
        BitwiseXor,

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
            : Expression{NK_Expr_Binary, Location{token}}
            , op{op}
            , left{std::move(left)}
            , right{std::move(right)} 
        {}

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

    // Bindings
    class Binding : public Node {
        Type *type;

        // TODO: Binding visitor
    protected:
        using Node::Node;

    public:
        void setType(Type *type) {
            this->type = type;
        }

        Type *getType() {
            return type;
        }

        static bool classof(const Node *node) {
            return node->getKind() >= NK_Binding_Identifier && node->getKind() <= NK_Binding_Identifier;
        }
    };

    class IdentifierBinding : public Binding {
        std::string identifier;
        bool isMutable = false;

        IdentifierBinding(Token identifier) 
            : Binding{NK_Binding_Identifier, identifier}
            , identifier{identifier.chars} 
        {}

        virtual void print(PrintContext& pc) const override;

    public:
        static unique_ptr<IdentifierBinding> create(Token identifier) {
            return unique_ptr<IdentifierBinding>{new IdentifierBinding(identifier)};
        }

        const std::string& getIdentifier() const {
            return identifier;
        }

        bool getIsMutable() const {
            return isMutable;
        }

        void setIsMutable(bool isMutable) {
            this->isMutable = isMutable;
        }
    
        static bool classof(const Node *node) {
            return node->getKind() == NK_Binding_Identifier;
        }
    };

    // Statements
    template <typename Subclass, typename ReturnType, typename... Args>
    class StatementVisitorT;

    class Statement : public Node {
    public:
        template <typename Subclass, typename ReturnType, typename... Args>
        ReturnType acceptVisitor(StatementVisitorT<Subclass, ReturnType, Args...>& visitor, Args&&... args);
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
    public:
        static unique_ptr<AssignmentStatement> create(Token token, unique_ptr<Expression> target, unique_ptr<Expression> value) {
            return unique_ptr<AssignmentStatement>{new AssignmentStatement(token, std::move(target), std::move(value))};
        }

        Expression& getTarget() const {
            return *target;
        }

        Expression& getValue() const {
            return *value;
        }

        void setWrappedValue(unique_ptr<Expression>&& wrapped) {
            assert(wrapped);
            std::ignore = value.release();
            value = std::move(wrapped);
        }
    };

    class CompoundAssignmentStatement : public Statement {
    protected:
        BinaryOperator op;
        unique_ptr<Expression> target;
        unique_ptr<Expression> operand;

        CompoundAssignmentStatement(Token token, BinaryOperator op, unique_ptr<Expression> target, unique_ptr<Expression> operand)
            : Statement{NK_Stmt_Compound_Assignment, Location{token}}
            , op{op}
            , target{std::move(target)}
            , operand{std::move(operand)} 
        {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<CompoundAssignmentStatement> create(Token token, BinaryOperator op, unique_ptr<Expression> target, unique_ptr<Expression> value) {
            return unique_ptr<CompoundAssignmentStatement>{new CompoundAssignmentStatement(token, op, std::move(target), std::move(value))};
        }

        const BinaryOperator getOp() const {
            return op;
        }

        Expression& getTarget() const {
            return *target;
        }

        Expression& getOperand() const {
            return *operand;
        }
    };

    class IfStatement : public Statement {
    public:
        class Branch {
            unique_ptr<Expression> condition;
            Block block;

        public:
            Branch(unique_ptr<Expression>&& condition, Block&& block) : condition{std::move(condition)}, block{std::move(block)} {}

            Expression& getCondition() const {
                return *condition;
            }

            Block& getBlock() {
                return block;
            }

            const Block& getBlock() const {
                return block;
            }

            friend class IfStatement;
        };
    protected:
        // TODO: Rename to branches
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

        size_t getConditionCount() const {
            return conditionals.size();
        }

        Branch& getBranch(size_t i) {
            return conditionals.at(i);
        }

        const Branch& getBranch(size_t i) const {
            return conditionals.at(i);
        }

        // TODO: Remove these
        Branch& getCondition(size_t i) {
            return conditionals.at(i);
        }

        const Branch& getCondition(size_t i) const {
            return conditionals.at(i);
        }

        Block* getFallback() {
            if (fallback.has_value()) {
                Block& block = fallback.value();
                return &block;
            } else {
                return nullptr;
            }
        }

        const Block* getFallback() const {
            if (fallback.has_value()) {
                const Block& block = fallback.value();
                return &block;
            } else {
                return nullptr;
            }
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Stmt_If;
        }
    };

    class GuardStatement : public Statement {
        unique_ptr<Expression> condition;
        Block block;
    protected:
        GuardStatement(Token token, unique_ptr<Expression>&& condition, Block&& block) : Statement{NK_Stmt_Guard, token}, condition{std::move(condition)}, block{std::move(block)} {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<GuardStatement> create(Token token, unique_ptr<Expression>&& condition, Block&& block) {
            return unique_ptr<GuardStatement>{new GuardStatement(token, std::move(condition), std::move(block))};
        }

        Expression& getCondition() {
            return *condition;
        }

        const Expression& getCondition() const {
            return *condition;
        }

        Block& getBlock() {
            return block;
        }

        const Block& getBlock() const {
            return block;
        }
    };

    class ReturnStatement : public Statement {
    protected:
        unique_ptr<Expression> value;

        ReturnStatement(Token token, unique_ptr<Expression>&& value) : Statement{NK_Stmt_Return, token}, value{std::move(value)} {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<ReturnStatement> create(Token token, unique_ptr<Expression> expression) {
            return unique_ptr<ReturnStatement>{new ReturnStatement(token, std::move(expression))};
        }

        Expression *getValue() const {
            return value.get();
        }

        void setWrappedValue(unique_ptr<Expression>&& wrapped) {
            assert(wrapped);
            std::ignore = value.release();
            value = std::move(wrapped);
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

        Expression& getCondition() const {
            return *condition;
        }

        Block& getBlock() {
            return code;
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
        unique_ptr<Binding> binding;
        unique_ptr<Expression> iterable;
        Block code;

        ForStatement(Token token, unique_ptr<Binding>&& binding, unique_ptr<Expression>&& iterable, Block&& code)
            : Statement{NK_Stmt_For, token}
            , binding{std::move(binding)}
            , iterable{std::move(iterable)}
            , code{std::move(code)}
        {}

        virtual void print(PrintContext& pc) const override;

    public:
        static unique_ptr<ForStatement> create(Token token, unique_ptr<Binding>&& binding, unique_ptr<Expression>&& iterable, Block&& code) {
            return unique_ptr<ForStatement>{new ForStatement(token, std::move(binding), std::move(iterable), std::move(code))};
        }

        Binding& getBinding() {
            return *binding;
        }

        const Binding& getBinding() const {
            return *binding;
        }

        Expression& getIterable() {
            return *iterable;
        }

        const Expression& getIterable() const {
            return *iterable;
        }

        Block& getBlock() {
            return code;
        }

        const Block& getBlock() const {
            return code;
        }
    };

    class ExpressionStatement : public Statement {
    protected:
        unique_ptr<Expression> expression;

        ExpressionStatement(unique_ptr<Expression>&& expression) : Statement{NK_Stmt_Expression, expression->getLocation()}, expression{std::move(expression)} {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<ExpressionStatement> create(unique_ptr<Expression>&& expression) {
            return unique_ptr<ExpressionStatement>{new ExpressionStatement{std::move(expression)}};
        }

        Expression& getExpression() {
            return *expression;
        }

        const Expression& getExpression() const {
            return *expression;
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Stmt_Expression;
        }
    };

    // Declarations
    
    template <typename Subclass, typename ReturnType, typename... Args>
    class DeclarationVisitorT;

    class Declaration : public Node {
    public:
        template <typename Subclass, typename ReturnType, typename... Args>
        ReturnType acceptVisitor(DeclarationVisitorT<Subclass, ReturnType, Args...>& visitor, Args&&... args);
    protected:
        using Node::Node;

        static bool classof(const Node *node) {
            // TODO: Ensure starting kind is correct
            return node->getKind() >= NK_Decl_Variable && node->getKind() <= NK_Decl_Statement;
        }
    };

    class VariableDeclaration : public Declaration {
    protected:
        unique_ptr<Binding> binding;
        Type* type = nullptr;
        unique_ptr<TypeNode> typeDeclaration;
        unique_ptr<Expression> initial;
        bool isMutable;

        VariableDeclaration(
            Token token, 
            bool isMutable,
            unique_ptr<Binding>&& binding,
            unique_ptr<TypeNode>&& type, 
            unique_ptr<Expression>&& initial
        ) : Declaration{NK_Decl_Variable, token}
          , isMutable{isMutable}
          , binding{std::move(binding)}
          , typeDeclaration{std::move(type)}
          , initial{std::move(initial)} {}
        
        virtual void print(PrintContext& pc) const override;

    public:
        static unique_ptr<VariableDeclaration> create(
            Token token, 
            bool isMutable,
            unique_ptr<Binding>&& binding,
            unique_ptr<TypeNode>&& type, 
            unique_ptr<Expression>&& initial
        ) {
            return unique_ptr<VariableDeclaration>{new VariableDeclaration{
                token, 
                isMutable, 
                std::move(binding),
                std::move(type), 
                std::move(initial)
            }};
        }

        const bool getIsMutable() const {
            return isMutable;
        }

        const Binding& getBinding() const {
            return *binding;
        }

        Binding& getBinding() {
            return *binding;
        }

        Expression *getInitialValue() const {
            return initial.get();
        }

        void setWrappedInitialValue(unique_ptr<Expression>&& wrapped) {
            assert(wrapped);
            std::ignore = initial.release();
            initial = std::move(wrapped);
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

    class FunctionParameter {
    public:
        std::string name;
        unique_ptr<TypeNode> typeDeclaration;
        Type *type = nullptr;
        
        FunctionParameter(std::string_view& name, unique_ptr<TypeNode>&& type) : name{name}, typeDeclaration{std::move(type)} {}
        //Parameter(Parameter&& parameter) : name{std::move(parameter.name)}, type{std::move(parameter.type)} {}
        //Parameter& operator=(Parameter&& parameter) = default;
    };

    class FunctionName {
        struct Label {
            uint32_t offset;
            uint32_t length;
        };

        const char *base;
        uint32_t length;
        uint32_t root;
        std::vector<Label> labels;
    };

    class InitializerDeclaration : public Declaration {
        std::vector<FunctionParameter> parameters;
        Block code;


    };

    class FunctionDeclaration : public Declaration {
    protected:
        std::string name;
        std::vector<FunctionParameter> parameters;
        int arity;
        unique_ptr<TypeNode> returnTypeDeclaration;
        FunctionType *type;
        Type *returnType;
        Block code;

        FunctionDeclaration(Token token, std::vector<FunctionParameter>&& parameters, unique_ptr<TypeNode>&& returnType, Block&& code) 
            : Declaration{NK_Decl_Function, Location{token}}
            , name{token.chars}
            , parameters{std::move(parameters)}
            , arity{int(this->parameters.size())}
            , returnTypeDeclaration{std::move(returnType)}
            , code{std::move(code)} {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<FunctionDeclaration> create(Token token, std::vector<FunctionParameter>&& parameters, unique_ptr<TypeNode>&& returnType, Block&& code) {
            return unique_ptr<FunctionDeclaration>{new FunctionDeclaration(token, std::move(parameters), std::move(returnType), std::move(code))};
        }

        const std::string& getName() const {
            return name;
        }

        TypeNode *getReturnTypeDeclaration() const {
            return returnTypeDeclaration.get();
        }

        Type *getReturnType() const {
            return type->getReturnType();
        }

        void setType(FunctionType& type) {
            this->type = &type;
        }

        FunctionType *getType() const {
            return type;
        }

        int getParameterCount() const {
            return parameters.size();
        }

        FunctionParameter& getParameter(int i) {
            return parameters[i];
        }

        const FunctionParameter& getParameter(int i) const {
            return parameters[i];
        }

        Block& getCode() {
            return code;
        }

        const Block& getCode() const {
            return code;
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
        std::vector<unique_ptr<Declaration>> declarations;

        StructDeclaration(Token token, std::string_view& name, std::vector<unique_ptr<Declaration>>&& declarations)
            : Declaration{NK_Decl_Struct, Location{token}}
            , name{name}
            , declarations{std::move(declarations)} {}
    public:
        static unique_ptr<StructDeclaration> create(Token token, std::string_view& name, std::vector<unique_ptr<Declaration>>&& declarations) {
            return unique_ptr<StructDeclaration>{new StructDeclaration(token, name, std::move(declarations))};
        }

        virtual void print(PrintContext& pc) const override;

        const std::string& getName() const {
            return name;
        }

        static bool classof(const Node *node) {
            return node->getKind() == NK_Decl_Struct;
        }

        iterator<Declaration> begin() {
            return iterator<Declaration>(declarations.begin());
        }

        iterator<Declaration> end() {
            return iterator<Declaration>(declarations.end());
        }

        const_iterator<Declaration> cbegin() const {
            return const_iterator<Declaration>(declarations.cbegin());
        }

        const_iterator<Declaration> cend() const {
            return const_iterator<Declaration>(declarations.cend());
        }
    };

    class EnumDeclaration : public Declaration {
    public:
        class Case {
        public:
            class Member {
                // TODO: Nullable symbol pointer
                std::string name;
                unique_ptr<TypeNode> type;
            public:
                Member(unique_ptr<TypeNode>&& type) : name{}, type{std::move(type)} {}
                Member(std::string_view name, unique_ptr<TypeNode>&& type) : name{name}, type{std::move(type)} {}

                const std::string& getName() {
                    return name;
                }

                TypeNode& getType() {
                    return *type;
                }
            };
        private:
            std::string name;
            std::vector<Member> members;
        public:
            Case(Token token, Token name) : name{name.chars}, members{} {}
            Case(Token token, Token name, std::vector<Member>&& members) : name{name.chars}, members{std::move(members)} {}
            Case(Case&) = delete;
            Case& operator=(Case&) = delete;
            Case(Case&&) = default;
            Case& operator=(Case&&) = default;

            const std::string& getName() const {
                return name;
            }

            const size_t getMemberCount() const {
                return members.size();
            }

            const bool hasMembers() const {
                return !members.empty();
            }

            value_iterator<Member> begin() {
                return value_iterator<Member>(members.begin());
            }

            value_iterator<Member> end() {
                return value_iterator<Member>(members.end());
            }

            const_value_iterator<Member> begin() const {
                return const_value_iterator<Member>(members.cbegin());
            }

            const_value_iterator<Member> end() const {
                return const_value_iterator<Member>(members.cend());
            }
        };
    protected:
        std::string name;
        unique_ptr<TypeNode> rawType;
        std::vector<Case> cases;
        std::vector<unique_ptr<Declaration>> declarations;

        EnumDeclaration(Token token, std::string_view& name, unique_ptr<TypeNode> rawType, std::vector<Case>&& cases, std::vector<unique_ptr<Declaration>>&& declarations) 
            : Declaration{NK_Decl_Enum, token}
            , name{name}
            , cases{std::move(cases)}
            , declarations{std::move(declarations)}
        {}

        virtual void print(PrintContext& pc) const override;
    public:
        static unique_ptr<EnumDeclaration> create(Token token, std::string_view& name, unique_ptr<TypeNode> rawType, std::vector<Case>&& cases, std::vector<unique_ptr<Declaration>>&& declarations) {
            return unique_ptr<EnumDeclaration>{new EnumDeclaration{token, name, std::move(rawType), std::move(cases), std::move(declarations)}};
        }

        size_t getNumberOfCases() const {
            return cases.size();
        }

        value_iterator<Case> begin() {
            return value_iterator<Case>(cases.begin());
        }

        value_iterator<Case> end() {
            return value_iterator<Case>(cases.end());
        }

        const_value_iterator<Case> begin() const {
            return const_value_iterator<Case>(cases.cbegin());
        }

        const_value_iterator<Case> end() const {
            return const_value_iterator<Case>(cases.cend());
        }

        const std::string& getName() const {
            return name;
        }
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

        Statement& getStatement() const {
            return *statement;
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

        void printInteger(const llvm::APInt& integer, unsigned base) {
            llvm::SmallVector<char, 20> string;
            integer.toString(string, base, false);
            os << std::string_view{string.data(), string.data() + string.size()};
        }

        friend class Node;
    };
}

#endif // LANG_ast_h
