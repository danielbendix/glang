#ifndef LANG_ast_h
#define LANG_ast_h

#include "common.h"
#include "token.h"
#include "resolution/identifier.h"
#include "resolution/member.h"
#include "unique_ref.h"
#include "type.h"
#include "memory.h"

#include "containers/symbol_table.h"
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
    template <typename T>
    using vector = std::vector<T, ArrayAllocator<T>>;

    using string = std::basic_string<char, std::char_traits<char>, ArrayAllocator<char>>;

    template <typename T, Allocator A>
    void *NONNULL allocateSpace(A& allocator) {
        return allocator.allocate(sizeof(T), alignof(T));
    }

    template <Allocator Allocator, typename F, typename T = std::remove_pointer_t<std::invoke_result_t<F, void *>>>
    T *NONNULL allocate(Allocator& allocator, F f) {
        void *space = allocateSpace<T>(allocator);
        return f(space);
    }

    class PrintContext;

    struct Location {
        // TODO: SourceFile reference
        int line;
        int column;
        int length;

        Location(Token token) : line{token.line}, column{token.column}, length{token.length} {}
    };

    std::ostream& operator<<(std::ostream& os, const Location& location);

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
            NK_Stmt_Break,
            NK_Stmt_Continue,
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
        const Location& getLocation() const { return location; }

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
        using internal = std::vector<T *NONNULL>::iterator;
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
        using internal = std::vector<T *NONNULL>::const_iterator;
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

        static bool classof(const Node *NONNULL node) {
            return node->getKind() >= NK_Type_Literal && node->getKind() <= NK_Type_Modifier;;
        }
    };

    class TypeLiteral : public TypeNode {
    protected:
        Symbol& name;

        TypeLiteral(Token token, Symbol& name) 
            : TypeNode{NK_Type_Literal, Location{token}}
            , name{name} 
        {}

        virtual void print(PrintContext& pc) const;

    public:
        template <Allocator A>
        static TypeLiteral *create(A& allocator, Token token, Symbol& name) {
            return allocate(allocator, [&](auto space) {
                return new(space) TypeLiteral{token, name};
            });
        }

        const Symbol& getName() const {
            return name;
        }

        static bool classof(const Node *NONNULL node) {
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
        TypeNode *NONNULL child;
        SmallByteArray<Modifier> modifiers;

        // FIXME Better location
        TypeModifier(TypeNode *NONNULL child, std::span<Modifier> modifiers) : TypeNode{NK_Type_Modifier, Location{child->location}}, child{child}, modifiers{modifiers} {}

        virtual void print(PrintContext& pc) const;
    public:
        template <Allocator A>
        static TypeModifier *create(A& allocator, TypeNode *NONNULL child, std::span<Modifier> modifiers) {
            return allocate(allocator, [&](auto space) {
                return new(space) TypeModifier{child, std::move(modifiers)};
            });
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

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Type_Modifier;
        }
    };

    // Utilities
    
    class Declaration;
    class Block {
    protected:
        std::vector<Declaration *NONNULL> declarations;

        Block(const Block&) = delete;
        Block& operator=(const Block&) = delete;
    public:
        Block(std::vector<Declaration *NONNULL>&& declarations) : declarations{std::move(declarations)} {}
        Block(Block&& other) = default;
        Block& operator=(Block&& other) = default;

        static std::unique_ptr<Block> create(std::vector<Declaration *NONNULL>&& declarations) {
            return std::unique_ptr<Block>(new Block(std::move(declarations)));
        }

        void print(PrintContext& pc) const;

        size_t size() const {
            return declarations.size();
        }

        Declaration& operator[](size_t index) const {
            return *declarations[index];
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

        static bool classof(const Node *NONNULL node) {
            return node->getKind() >= NK_Expr_Identifier && node->getKind() <= NK_Expr_Inferred_Member_Access;
        }
    };

    class Identifier : public Expression {
    protected:
        std::unique_ptr<IdentifierResolution, Deleter<IdentifierResolution>> resolution = nullptr;

        Symbol& name;

        Identifier(Token token, Symbol& name) : Expression{NK_Expr_Identifier, token}, name{name} {}

        virtual void print(PrintContext& pc) const override;
    public:
        template <Allocator Allocator>
        static Identifier *create(Allocator& allocator, Token token, Symbol& name) {
            return allocate(allocator, [&](auto space) {
                return new(space) Identifier{token, name};
            });
        }

        const Symbol& getName() const {
            return name;
        }

        IdentifierResolution *getResolution() const {
            return resolution.get();
        }

        void setResolution(std::unique_ptr<IdentifierResolution, Deleter<IdentifierResolution>>&& resolution) {
            this->resolution = std::move(resolution);
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Identifier;
        }
    };

    class Self : public Expression {
        Self(Token token) : Expression{NK_Expr_Self, token} {}

        virtual void print(PrintContext& pc) const override;
    public:
        template <Allocator Allocator>
        static Self *create(Allocator& allocator, Token token) {
            return allocate(allocator, [&](auto space) {
                return new(space) Self{token};
            });
        }

        static bool classof(const Node *NONNULL node) {
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

        template <Allocator Allocator>
        static Literal *create(Allocator& allocator, Token token, bool value) {
            return allocate(allocator, [&](auto space) {
                return new(space) Literal{token, value};
            });
        }

        template <Allocator Allocator>
        static Literal *create(Allocator& allocator, Token token, double value) {
            return allocate(allocator, [&](auto space) {
                return new(space) Literal{token, value};
            });
        }

        template <Allocator Allocator>
        static Literal *create(Allocator& allocator, Token token, APInt&& value, IntegerType integerType) {
            return allocate(allocator, [&](auto space) {
                return new(space) Literal{token, std::move(value), integerType};
            });
        }

        template <Allocator Allocator>
        static Literal *create(Allocator& allocator, Token token, std::string&& value) {
            return allocate(allocator, [&](auto space) {
                return new(space) Literal{token, std::move(value)};
            });
        }

        template <Allocator Allocator>
        static Literal *createNil(Allocator& allocator, Token token) {
            return allocate(allocator, [&](auto space) {
                return new(space) Literal{token, std::monostate{}};
            });
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

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Literal;
        }
    };

    class CallExpression : public Expression {
        Expression *NONNULL target;
        std::vector<Expression *NONNULL> arguments;

    protected:
        CallExpression(Token token, Expression *NONNULL target, std::vector<Expression *NONNULL>&& arguments) 
            : Expression{NK_Expr_Call, Location{token}}
            , target{target}
            , arguments{std::move(arguments)} {}

        virtual void print(PrintContext& pc) const override;
        CallExpression(const CallExpression&) = delete;
        CallExpression& operator=(const CallExpression&) = delete;
    public:
        template <Allocator Allocator>
        static CallExpression *create(Allocator& allocator, Token token, Expression *NONNULL target, std::vector<Expression *NONNULL>&& arguments) {
            return allocate(allocator, [&](auto space) {
                return new(space) CallExpression{token, target, std::move(arguments)};
            });
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

        void setWrappedArgument(size_t i, Expression *NONNULL wrapped) {
            assert(wrapped);
            arguments[i] = wrapped;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Call;
        }
    };

    class SubscriptExpression : public Expression {
        Expression *NONNULL target;
        Expression *NONNULL index;
    protected:
        SubscriptExpression(Token token, Expression *NONNULL target, Expression *NONNULL index)
            : Expression{NK_Expr_Subscript, token}
            , target{target}
            , index{index} {}

        virtual void print(PrintContext& pc) const override;
    public:
        template <Allocator Allocator>
        static SubscriptExpression *create(Allocator& allocator, Token token, Expression *NONNULL target, Expression *NONNULL index) {
            return allocate(allocator, [&](auto space) {
                return new(space) SubscriptExpression(token, target, index);
            });
        }

        Expression& getTarget() const {
            return *target;
        }

        Expression& getIndex() const {
            return *index;
        }

        static bool classof(const Node *NONNULL NONNULL node) {
            return node->getKind() == NK_Expr_Subscript;
        }
    };

    class MemberAccessExpression : public Expression {
        Expression *NONNULL target;
        Symbol& memberName;
        unique_ptr_t<MemberResolution> resolution = nullptr;

        virtual void print(PrintContext& pc) const override;
    protected:
        MemberAccessExpression(Token token, Expression *NONNULL target, Symbol& member) 
            : Expression{NK_Expr_Member_Access, token}
            , target{target}
            , memberName{member} {}
    public:
        template <Allocator Allocator>
        static MemberAccessExpression *create(Allocator& allocator, Token token, Expression *NONNULL target, Symbol& member) {
            return allocate(allocator, [&](auto space) {
                return new(space) MemberAccessExpression{token, target, member};
            });
        }

        Expression& getTarget() const {
            return *target;
        }

        const Symbol& getMemberName() const {
            return memberName;
        }

        const MemberResolution& getResolution() const {
            return *resolution;
        }

        void setResolution(unique_ptr_t<MemberResolution>&& resolution) {
            this->resolution = std::move(resolution);
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Member_Access;
        }
    };

    class InferredMemberAccessExpression : public Expression {
        Symbol& memberName;
        unique_ptr_t<MemberResolution> resolution = nullptr;
        Type *inferredTarget = nullptr;

        virtual void print(PrintContext& pc) const override;
    protected:
        InferredMemberAccessExpression(Token token, Symbol& memberName)
            : Expression{NK_Expr_Inferred_Member_Access, token}
            , memberName{memberName} {}
    public:
        template <Allocator Allocator>
        static InferredMemberAccessExpression *create(Allocator& allocator, Token token, Symbol& memberName) {
            return allocate(allocator, [&](auto space) {
                return new(space) InferredMemberAccessExpression{token, memberName};
            });
        }

        const Symbol& getMemberName() const {
            return memberName;
        }

        const MemberResolution& getResolution() const {
            return *resolution;
        }

        void setResolution(unique_ptr_t<MemberResolution>&& resolution) {
            this->resolution = std::move(resolution);
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Inferred_Member_Access;
        }
    };

    class InitializerExpression : public Expression {
    public:
        using Pair = std::pair<InferredMemberAccessExpression *NONNULL, Expression *NONNULL>;
    private:       
        // This is a typename. Could contain a type parameter in the future. We should find a better type.
        Identifier *NULLABLE identifier;
        std::vector<Pair> pairs;
    protected:
        InitializerExpression(Token token, Identifier *NULLABLE identifier, std::vector<Pair>&& pairs) 
            : Expression{NK_Expr_Initializer, token}
            , identifier{identifier}
            , pairs{std::move(pairs)}
            {}

        virtual void print(PrintContext& pc) const override;

    public:
        template <Allocator Allocator>
        static InitializerExpression *create(Allocator& allocator, Token token, Identifier *NULLABLE identifier, std::vector<Pair>&& pairs) {
            return allocate(allocator, [&](auto space) {
                return new(space) InitializerExpression{token, identifier, std::move(pairs)};
            });
        }

        Identifier *getIdentifier() {
            return identifier;
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
        Expression *NONNULL target;

        UnaryExpression(Token token, UnaryOperator op, Expression *NONNULL target) 
            : Expression{NK_Expr_Unary, token}, op{op}, target{target}
        {}

        UnaryExpression(Location location, UnaryOperator op, Expression *NONNULL target) 
            : Expression{NK_Expr_Unary, location}, op{op}, target{target}
        {}

        void print(PrintContext& pc) const override;
    public:
        template <Allocator Allocator>
        static UnaryExpression *create(Allocator& allocator, Token token, UnaryOperator op, Expression *NONNULL target) {
            return allocate(allocator, [&](auto space) {
                return new(space) UnaryExpression{token, op, target};
            });
        }

        template <Allocator Allocator>
        static UnaryExpression *wrap(Allocator& allocator, Expression& target, UnaryOperator op, Type& type) {
            auto result = allocate(allocator, [&](auto space) {
                return new(space) UnaryExpression(target.location, op, &target);
            });
            result->setType(&type);
            return result;
        }

        UnaryOperator getOp() const {
            return op;
        }

        Expression &getTarget() const {
            return *target;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Unary;
        }
    };

    enum class BinaryOperator {
        OpenRange,
        ClosedRange,

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
        Expression *NONNULL left;
        Expression *NONNULL right;
        BinaryExpression(Token token, BinaryOperator op, Expression *NONNULL left, Expression *NONNULL right) 
            : Expression{NK_Expr_Binary, Location{token}}
            , op{op}
            , left{left}
            , right{right} 
        {}

        void print(PrintContext& pc) const override;
    public:
        template <Allocator Allocator>
        static BinaryExpression *create(Allocator& allocator, Token token, BinaryOperator op, Expression *NONNULL left, Expression *NONNULL right) {
            return allocate(allocator, [&](auto space) {
                return new(space) BinaryExpression{token, op, left, right};
            });
        }

        BinaryOperator getOp() const {
            return op;
        }
        
        Expression& getLeft() const {
            return *left;
        }
        void setLeft(Expression& left) {
            this->left = &left;
        }

        Expression& getRight() const {
            return *right;
        }
        void setRight(Expression& right) {
            this->right = &right;
        }

        static bool classof(const Node *NONNULL node) {
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

        static bool classof(const Node *NONNULL node) {
            return node->getKind() >= NK_Binding_Identifier && node->getKind() <= NK_Binding_Identifier;
        }
    };

    class IdentifierBinding : public Binding {
        Symbol& identifier;
        bool isMutable = false;

        IdentifierBinding(Token token, Symbol& identifier) 
            : Binding{NK_Binding_Identifier, token}
            , identifier{identifier} 
        {}

        virtual void print(PrintContext& pc) const override;

    public:
        template <Allocator A>
        static IdentifierBinding *create(A& allocator, Token token, Symbol& identifier) {
            return allocate(allocator, [&](auto space) {
                return new(space) IdentifierBinding(token, identifier);
            });
        }

        const Symbol& getIdentifier() const {
            return identifier;
        }

        bool getIsMutable() const {
            return isMutable;
        }

        void setIsMutable(bool isMutable) {
            this->isMutable = isMutable;
        }
    
        static bool classof(const Node *NONNULL node) {
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
        Expression *NONNULL target;
        Expression *NONNULL value;

        AssignmentStatement(Token token, Expression *NONNULL target, Expression *NONNULL value)
            : Statement{NK_Stmt_Assignment, Location{token}}
            , target{target}
            , value{value} 
        {}

        virtual void print(PrintContext& pc) const override;
    public:
        template <Allocator Allocator>
        static AssignmentStatement *NONNULL create(Allocator& allocator, Token token, Expression *NONNULL target, Expression *NONNULL value) {
            return allocate(allocator, [&](auto space) {
                return new(space) AssignmentStatement{token, target, value};
            });
        }

        Expression& getTarget() const {
            return *target;
        }

        Expression& getValue() const {
            return *value;
        }

        void setWrappedValue(Expression *NONNULL wrapped) {
            assert(wrapped);
            value = wrapped;
        }
    };

    class CompoundAssignmentStatement : public Statement {
    protected:
        BinaryOperator op;
        Expression *NONNULL target;
        Expression *NONNULL operand;

        CompoundAssignmentStatement(Token token, BinaryOperator op, Expression *NONNULL target, Expression *NONNULL operand)
            : Statement{NK_Stmt_Compound_Assignment, Location{token}}
            , op{op}
            , target{target}
            , operand{operand} 
        {}

        virtual void print(PrintContext& pc) const override;
    public:
        template <Allocator Allocator>
        static CompoundAssignmentStatement *NONNULL create(Allocator& allocator, Token token, BinaryOperator op, Expression *NONNULL target, Expression *NONNULL operand) {
            return allocate(allocator, [&](auto space) {
                return new(space) CompoundAssignmentStatement{token, op, target, operand};
            });
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
            Expression *NONNULL condition;
            Block block;

        public:
            Branch(Expression *NONNULL condition, Block&& block) : condition{condition}, block{std::move(block)} {}

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
        template <Allocator Allocator>
        static IfStatement *NONNULL create(Allocator& allocator, Token token, std::vector<Branch>&& conditionals, std::optional<Block>&& fallback) {
            return allocate(allocator, [&](auto space) {
                return new(space) IfStatement{token, std::move(conditionals), std::move(fallback)};
            });
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

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Stmt_If;
        }
    };

    class GuardStatement : public Statement {
        Expression *NONNULL condition;
        Block block;
    protected:
        GuardStatement(Token token, Expression *NONNULL condition, Block&& block) : Statement{NK_Stmt_Guard, token}, condition{condition}, block{std::move(block)} {}

        virtual void print(PrintContext& pc) const override;
    public:
        template <Allocator Allocator>
        static GuardStatement *NONNULL create(Allocator& allocator, Token token, Expression *NONNULL condition, Block&& block) {
            return allocate(allocator, [&](auto space) {
                return new(space) GuardStatement{token, condition, std::move(block)};
            });
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
        Expression *NONNULL value;

        ReturnStatement(Token token, Expression *NONNULL value) : Statement{NK_Stmt_Return, token}, value{value} {}

        virtual void print(PrintContext& pc) const override;
    public:
        template <Allocator Allocator>
        static ReturnStatement *NONNULL create(Allocator& allocator, Token token, Expression *NONNULL expression) {
            return allocate(allocator, [&](auto space) {
                return new(space) ReturnStatement{token, expression};

            });
        }

        Expression *NONNULL getValue() const {
            return value;
        }

        void setWrappedValue(Expression *NONNULL wrapped) {
            assert(wrapped);
            value = wrapped;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Stmt_Return;
        }
    };

    class WhileStatement : public Statement {
    protected:
        Expression *NONNULL condition;
        Block code;

        WhileStatement(Token token, Expression *NONNULL condition, Block&& code) 
            : Statement{NK_Stmt_While, token}
            , condition{condition}
            , code{std::move(code)} 
        {}
    
        virtual void print(PrintContext& pc) const override;
    public:
        template <Allocator Allocator>
        static WhileStatement *NONNULL create(Allocator& allocator, Token token, Expression *NONNULL condition, Block&& code) {
            return allocate(allocator, [&](auto space) {
                return new(space) WhileStatement{token, condition, std::move(code)};

            });
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

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Stmt_While;
        }
    };

    class ForStatement : public Statement {
    protected:
        Binding *NONNULL binding;
        Expression *NONNULL iterable;
        Block code;

        ForStatement(Token token, Binding *NONNULL binding, Expression *NONNULL iterable, Block&& code)
            : Statement{NK_Stmt_For, token}
            , binding{binding}
            , iterable{iterable}
            , code{std::move(code)}
        {}

        virtual void print(PrintContext& pc) const override;

    public:
        template <Allocator Allocator>
        static ForStatement *NONNULL create(Allocator& allocator, Token token, Binding *NONNULL binding, Expression *NONNULL iterable, Block&& code) {
            return allocate(allocator, [&](auto space) {
                return new(space) ForStatement{token, binding, iterable, std::move(code)};
            });
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

    class BreakStatement : public Statement {
    protected:
        BreakStatement(Token token) : Statement{NK_Stmt_Break, token} {}

        virtual void print(PrintContext& pc) const override;

    public:
        template <Allocator Allocator>
        static BreakStatement *NONNULL create(Allocator& allocator, Token token) {
            return allocate(allocator, [&](auto space) {
                return new(space) BreakStatement{token};
            });
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Stmt_Break;
        }
    };

    class ContinueStatement : public Statement {
    protected:
        ContinueStatement(Token token) : Statement{NK_Stmt_Continue, token} {}

        virtual void print(PrintContext& pc) const override;

    public:
        template <Allocator Allocator>
        static ContinueStatement *NONNULL create(Allocator& allocator, Token token) {
            return allocate(allocator, [&](auto space) {
                return new(space) ContinueStatement{token};
            });
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Stmt_Continue;
        }
    };

    class ExpressionStatement : public Statement {
    protected:
        Expression *NONNULL expression;

        ExpressionStatement(Expression *NONNULL expression) : Statement{NK_Stmt_Expression, expression->getLocation()}, expression{expression} {}

        virtual void print(PrintContext& pc) const override;
    public:
        template <Allocator Allocator>
        static ExpressionStatement *NONNULL create(Allocator& allocator, Expression *NONNULL expression) {
            return allocate(allocator, [&](auto space) {
                return new(space) ExpressionStatement{expression};
            });
        }

        Expression& getExpression() {
            return *expression;
        }

        const Expression& getExpression() const {
            return *expression;
        }

        static bool classof(const Node *NONNULL node) {
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

        static bool classof(const Node *NONNULL node) {
            // TODO: Ensure starting kind is correct
            return node->getKind() >= NK_Decl_Variable && node->getKind() <= NK_Decl_Statement;
        }
    };

    class VariableDeclaration : public Declaration {
    protected:
        Binding *NONNULL binding;
        TypeNode *NULLABLE typeDeclaration;
        Expression *NULLABLE initial;
        Type *NULLABLE type = nullptr;
        bool isMutable;

        VariableDeclaration(
            Token token, 
            bool isMutable,
            Binding *NONNULL binding,
            TypeNode *NULLABLE type, 
            Expression *NULLABLE initial
        ) : Declaration{NK_Decl_Variable, token}
          , isMutable{isMutable}
          , binding{binding}
          , typeDeclaration{type}
          , initial{initial} {}
        
        virtual void print(PrintContext& pc) const override;

    public:
        template<Allocator Allocator>
        static VariableDeclaration *create(
            Allocator& allocator,
            Token token, 
            bool isMutable,
            Binding *NONNULL binding,
            TypeNode *NULLABLE type, 
            Expression *NULLABLE initial
        ) {
            return allocate(allocator, [&](void *space) {
                return new(space) VariableDeclaration{token, isMutable, binding, type, initial};
            });
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

        Expression *NULLABLE getInitialValue() const {
            return initial;
        }

        void setWrappedInitialValue(Expression *NONNULL wrapped) {
            assert(wrapped);
            initial = wrapped;
        }

        TypeNode *NULLABLE getTypeDeclaration() const {
            return typeDeclaration;
        }

        Type *getType() const {
            return type;
        }

        void setType(Type& type) {
            this->type = &type;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Decl_Variable;
        }
    };

    class FunctionParameter {
    public:
        Symbol& name;
        TypeNode *typeDeclaration;
        Type *NULLABLE type = nullptr;
        
        FunctionParameter(Symbol& name, TypeNode *type) : name{name}, typeDeclaration{type} {}
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
        Symbol& name;
        std::vector<FunctionParameter> parameters;
        int arity;
        TypeNode *returnTypeDeclaration;
        FunctionType *NULLABLE type;
        Type *NULLABLE returnType;
        Block code;

        FunctionDeclaration(Token token, Symbol& name, std::vector<FunctionParameter>&& parameters, TypeNode *returnType, Block&& code) 
            : Declaration{NK_Decl_Function, Location{token}}
            , name{name}
            , parameters{std::move(parameters)}
            , arity{int(this->parameters.size())}
            , returnTypeDeclaration{std::move(returnType)}
            , code{std::move(code)} {}

        virtual void print(PrintContext& pc) const override;
    public:
        template <Allocator A>
        static FunctionDeclaration *create(A& allocator, Token token, Symbol& name, std::vector<FunctionParameter>&& parameters, TypeNode *returnType, Block&& code) {
            return allocate(allocator, [&](auto space) {
                return new(space) FunctionDeclaration(token, name, std::move(parameters), returnType, std::move(code));

            });
        }

        const Symbol& getName() const {
            return name;
        }

        TypeNode *getReturnTypeDeclaration() const {
            return returnTypeDeclaration;
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

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Decl_Function;
        }
    };

    class StructDeclaration : public Declaration {
    protected:
        Symbol& name;
        std::vector<Declaration *NONNULL> declarations;

        StructDeclaration(Token token, Symbol& name, std::vector<Declaration *NONNULL>&& declarations)
            : Declaration{NK_Decl_Struct, Location{token}}
            , name{name}
            , declarations{std::move(declarations)} {}
    public:
        template <Allocator Allocator>
        static StructDeclaration *NONNULL create(Allocator& allocator, Token token, Symbol& name, std::vector<Declaration *NONNULL>&& declarations) {
            return allocate(allocator, [&](auto space) {
                return new(space) StructDeclaration{token, name, std::move(declarations)};
            });
        }

        virtual void print(PrintContext& pc) const override;

        const Symbol& getName() const {
            return name;
        }

        static bool classof(const Node *NONNULL node) {
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
                Symbol *NULLABLE name;
                TypeNode *type;
            public:
                Member(TypeNode *type) : name{nullptr}, type{type} {}
                Member(Symbol *name, TypeNode *type) : name{name}, type{type} {}

                const Symbol *getName() {
                    return name;
                }

                TypeNode& getType() {
                    return *type;
                }
            };
        private:
            Symbol& name;
            std::vector<Member> members;
        public:
            Case(Token token, Symbol& name) : name{name}, members{} {}
            Case(Token token, Symbol& name, std::vector<Member>&& members) : name{name}, members{std::move(members)} {}
            Case(Case&) = delete;
            Case& operator=(Case&) = delete;
            Case(Case&&) = default;
            Case& operator=(Case&&) = default;

            const Symbol& getName() const {
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
        Symbol& name;
        TypeNode *NULLABLE rawType;
        std::vector<Case> cases;
        std::vector<Declaration *NONNULL> declarations;

        EnumDeclaration(Token token, Symbol& name, TypeNode *NULLABLE rawType, std::vector<Case>&& cases, std::vector<Declaration *NONNULL>&& declarations) 
            : Declaration{NK_Decl_Enum, token}
            , name{name}
            , cases{std::move(cases)}
            , declarations{std::move(declarations)}
        {}

        virtual void print(PrintContext& pc) const override;
    public:
        template <Allocator A>
        static EnumDeclaration *NONNULL create(A& allocator, Token token, Symbol& name, TypeNode *NULLABLE rawType, std::vector<Case>&& cases, std::vector<Declaration *NONNULL>&& declarations) {
            return allocate(allocator, [&](auto space) {
                return new(space) EnumDeclaration{token, name, rawType, std::move(cases), std::move(declarations)};
            });
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

        const Symbol& getName() const {
            return name;
        }
    };
    
    class StatementDeclaration : public Declaration {
    protected:
        Statement *NONNULL statement;

        StatementDeclaration(Statement *NONNULL statement) : Declaration{NK_Decl_Statement, statement->getLocation()}, statement{statement} {}

        virtual void print(PrintContext& pc) const override; 
    public:
        template <Allocator Allocator>
        static StatementDeclaration *create(Allocator& allocator, Statement *NONNULL statement) {
            return allocate(allocator, [&](auto space) {
                return new(space) StatementDeclaration{statement};
            });
        }

        Statement& getStatement() const {
            return *statement;
        }
        
        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Decl_Statement;
        }
    };

    class ProtocolDeclaration : public Declaration {
    protected:
        Symbol& name;

        ProtocolDeclaration(Token token, Symbol& name) : Declaration{NK_Decl_Protocol, token}, name{name} {}

        virtual void print(PrintContext& pc) const override;
    public:
        const Symbol& getName() { return name; }
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

        PrintContext& operator<<(const Symbol& symbol) {
            os << symbol.string_view();
            return *this;
        }

        template <typename T>
        PrintContext& operator<<(T value) requires std::is_arithmetic_v<T> {
            os << value;
            return *this;
        }

        void printInteger(const llvm::APInt& integer, unsigned base) {
            llvm::SmallVector<char, 32> string;
            integer.toString(string, base, false);
            os << std::string_view{string.data(), string.data() + string.size()};
        }

        friend class Node;
    };
}

#endif // LANG_ast_h
