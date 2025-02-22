#ifndef LANG_ast_h
#define LANG_ast_h

#include "common.h"
#include "token.h"
#include "resolution/identifier.h"
#include "resolution/member.h"
#include "type.h"
#include "intrinsic.h"
#include "memory.h"
#include "context.h"

#include "containers/symbol_table.h"
#include "containers/span.h"
#include "containers/small_byte_array.h"
#include "containers/string.h"

#include <cassert>
#include <climits>

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
 *  Binding:
 *  Type:
 */

using llvm::APInt;

namespace AST {
    struct Modifiers final {
        using BITS_TYPE = u32;
        static constexpr size_t BIT_COUNT = sizeof(BITS_TYPE) * CHAR_BIT;
    private:
        BITS_TYPE bits = 0;
    public:
        enum class Modifier {
            Static,
            Public,
            Private,
            Unpadded,
            Compact,

            COUNT
        };

        static constexpr u8 MODIFIER_COUNT = u8(Modifier::COUNT);

        static_assert(u8(Modifier::COUNT) <= BIT_COUNT);

        constexpr Modifiers() {}

        constexpr Modifiers(std::initializer_list<Modifier> modifiers) {
            for (auto modifier : modifiers) {
                set(modifier);
            }
        }

        constexpr void reset() {
            bits = 0;
        }

        constexpr bool isEmpty() const {
            return bits == 0;
        }

        constexpr bool isNonEmpty() const {
            return bits != 0;
        }

        constexpr bool has(Modifier modifier) const {
            return bits & (1 << u8(modifier));
        }

        constexpr void set(Modifier modifier) {
            bits |= 1 << u8(modifier);
        }

        constexpr void invert() {
            bits = ~bits & ~(-1 << MODIFIER_COUNT);
        }

        constexpr u8 count() {
            return __builtin_popcount(bits);
        }

        constexpr Modifiers maskedBy(Modifiers other) const {
            Modifiers result = *this;
            result.bits &= other.bits;
            return result;
        }

        constexpr Modifiers disablingAllIn(Modifiers other) const {
            Modifiers result = *this;
            result.bits &= ~other.bits;
            return result;
        }

        constexpr BITS_TYPE get() const {
            return bits;
        }

        static constexpr const char *modifierName(Modifier modifier) {
            using enum Modifier;
            switch (modifier) {
                case Modifier::Static:
                    return "static";
                case Modifier::Public:
                    return "public";
                case Modifier::Private:
                    return "private";
                case Modifier::Unpadded:
                    return "unpadded";
                case Modifier::Compact:
                    return "compact";
                case Modifier::COUNT:
                    llvm_unreachable("[PROGRAMMER ERROR]: Modifier::COUNT value should never be used.");
            }
        }
        
    };
    using Modifier = Modifiers::Modifier;
    static constexpr Modifiers accessModifiers = {Modifier::Public, Modifier::Private};
}

namespace AST {
    class PrintContext;

    struct FileLocation {
        u32 offset;
        u32 length;

        FileLocation(u32 offset, u32 length) : offset{offset}, length{length} {}
    };

    class Node {
    public:
        enum Kind : u32 {
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
            NK_Expr_Literal_Nil,
            NK_Expr_Literal_False,
            NK_Expr_Literal_True,
            NK_Expr_Literal_Integer,
            NK_Expr_Literal_Floating,
            NK_Expr_Literal_Character,
            NK_Expr_Literal_String,
            NK_Expr_Self,
            NK_Expr_Unary,
            NK_Expr_Binary,
            NK_Expr_Intrinsic,
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
        u32 offset;
        Node(Kind kind, Token token) : kind{kind}, offset{token.offset} {}
        Node(Kind kind, u32 offset) : kind{kind}, offset{offset} {}

        Node(const Node&) = delete;
        Node& operator=(const Node&) = delete;
        ~Node() = default;

        Kind getKind() const { return kind; }

        void print(std::ostream& os) const;

        // TODO: Implement lldb output
        //void dump();

        void print(PrintContext& pc) const {}
        FileLocation getFileLocation() const;

        static void deleteNode(AST::Node *NONNULL node);
        static void deleteValue(AST::Node *NONNULL node) { deleteNode(node); }
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
            : TypeNode{NK_Type_Literal, token}
            , name{name} 
        {}


    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator A>
        static TypeLiteral *NONNULL create(A& allocator, Token token, Symbol& name) {
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
        enum class Modifier : u8 {
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
        TypeModifier(TypeNode *NONNULL child, std::span<Modifier> modifiers, u32 offset) : TypeNode{NK_Type_Modifier, offset}, child{child}, modifiers{modifiers} {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator A>
        static TypeModifier *NONNULL create(A& allocator, TypeNode *NONNULL child, std::span<Modifier> modifiers, u32 offset) {
            return allocate(allocator, [&](auto space) {
                return new(space) TypeModifier{child, std::move(modifiers), offset};
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
        Span<Declaration *NONNULL> declarations;

        using iterator = Span<Declaration *NONNULL>::iterator::dereferencing_iterator;
        using const_iterator = Span<Declaration *NONNULL>::const_iterator::dereferencing_iterator;

    public:
        Block(Span<Declaration *NONNULL> declarations) : declarations{declarations} {}

        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        size_t size() const {
            return declarations.size();
        }

        Declaration& operator[](u32 index) const {
            return *declarations[index];
        }

        void shrinkTo(u32 newSize) {
            declarations.shrinkTo(newSize);
        }

        // Iterator

        iterator begin() {
            return iterator(declarations.begin());
        }

        iterator end() {
            return iterator(declarations.end());
        }

        const_iterator begin() const {
            return const_iterator(declarations.begin());
        }

        const_iterator end() const {
            return const_iterator(declarations.end());
        }

        const_iterator cbegin() const {
            return const_iterator(declarations.cbegin());
        }

        const_iterator cend() const {
            return const_iterator(declarations.cend());
        }
    };

    // Expressions
    
    template <typename Subclass, typename ReturnType, typename... Args>
    class ExpressionVisitorT;

    class Expression : public Node {
    public:
        template <typename Subclass, typename ReturnType, typename... Args>
        ReturnType acceptVisitor(ExpressionVisitorT<Subclass, ReturnType, Args...>& visitor, Args... args);

        Type *NONNULL getType() const {
            return type;
        }

        void setType(Type *NONNULL type) {
            this->type = type;
        }
    protected:
        using Node::Node;

        Type *NULLABLE type = nullptr;

        static bool classof(const Node *NONNULL node) {
            return node->getKind() >= NK_Expr_Identifier && node->getKind() <= NK_Expr_Inferred_Member_Access;
        }
    };

    class Identifier : public Expression {
    protected:
        IdentifierResolution resolution;

        Symbol& name;

        Identifier(Token token, Symbol& name) : Expression{NK_Expr_Identifier, token}, name{name} {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static Identifier *NONNULL create(Allocator& allocator, Token token, Symbol& name) {
            return allocate(allocator, [&](auto space) {
                return new(space) Identifier{token, name};
            });
        }

        const Symbol& getName() const {
            return name;
        }

        IdentifierResolution getResolution() const {
            return resolution;
        }

        void setResolution(IdentifierResolution resolution) {
            this->resolution = resolution;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Identifier;
        }
    };

    class Self : public Expression {
        Self(Token token) : Expression{NK_Expr_Self, token} {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static Self *NONNULL create(Allocator& allocator, Token token) {
            return allocate(allocator, [&](auto space) {
                return new(space) Self{token};
            });
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Self;
        }
    };

    class Literal : public Expression {
    protected:
        using Expression::Expression;

    public:
        static bool classof(const Node *NONNULL node) {
            return node->getKind() >= NK_Expr_Literal_Nil && node->getKind() <= NK_Expr_Literal_String;
        }
    };

    class NilLiteral : public Literal {
    protected:
        NilLiteral(Token token) : Literal{NK_Expr_Literal_Nil, token} {}
    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static NilLiteral *NONNULL create(Allocator& allocator, Token token) {
            return allocate(allocator, [&](auto space) {
                return new(space) NilLiteral{token};
            });
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Literal_Nil;
        }
    };

    class BooleanLiteral : public Literal {
    protected:
        BooleanLiteral(Token token, bool value) : Literal{value ? NK_Expr_Literal_True : NK_Expr_Literal_False, token} {}
        BooleanLiteral(u32 offset, bool value) : Literal{value ? NK_Expr_Literal_True : NK_Expr_Literal_False, offset} {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static BooleanLiteral *NONNULL create(Allocator& allocator, Token token, bool value) {
            return allocate(allocator, [&](auto space) {
                return new(space) BooleanLiteral{token, value};
            });
        }

        template <typename NodeInstance>
        requires std::derived_from<NodeInstance, Node>
        static BooleanLiteral *NONNULL createDestroyingOther(NodeInstance& node, bool value) {
            static_assert(sizeof(BooleanLiteral) <= sizeof(NodeInstance));
            node.~NodeInstance();
            void *space = &node;
            return new(space) BooleanLiteral{node.offset, value};
        }

        bool getValue() const {
            return getKind() == NK_Expr_Literal_True;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Literal_False || node->getKind() == NK_Expr_Literal_True;
        }
    };

    class IntegerLiteral : public Literal {
    public:
        enum class Type: u8 {
            Binary,
            Octal,
            Decimal,
            Hexadecimal,
        };

        static constexpr u32 MAX_LENGTH = (1U << 29) - 1U;

        // A hack to put data inside the padding of APInt
        class Value : public llvm::APInt {
        public:
            u32 metadata;
            Value(llvm::APInt&& value, Type type, u32 length) : APInt{value} {
                assert(length <= MAX_LENGTH);
                metadata = (u32(type) << 29) | length;
            }

            Value& operator=(Value&& rhs) {
                metadata = rhs.metadata;
                APInt::operator=(std::move(rhs));
                return *this;
            }

            void signExtendInPlace(unsigned width) {
                APInt::operator=(sext(width));
            }

            void signExtendOrTruncateInPlace(unsigned width) {
                APInt::operator=(sextOrTrunc(width));
            }

            void setValue(llvm::APInt&& other) {
                APInt::operator=(std::move(other));
            }

            u32 getLength() const {
                return metadata & MAX_LENGTH;
            }

            Type getType() const {
                return Type(metadata >> 29);
            }

            friend class IntegerLiteral;
            friend class IntegerFold;
        };
    private:
        Value value;
    protected:
        IntegerLiteral(Token token, APInt&& value, Type integerType, u32 length)
            : Literal{NK_Expr_Literal_Integer, token}
            , value{std::move(value), integerType, length} {}
    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static IntegerLiteral *NONNULL create(Allocator& allocator, Token token, APInt&& value, Type integerType, u32 length) {
            return allocate(allocator, [&](auto space) {
                return new(space) IntegerLiteral{token, std::move(value), integerType, length};
            });
        }

        Value& getValue() {
            return value;
        }

        const APInt& getValue() const {
            return value;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Literal_Integer;
        }
    };

    class FloatingPointLiteral : public Literal {
        // Maybe use a more precise value, and track overflow.
        double value;
    protected:
        FloatingPointLiteral(Token token, double value) : Literal{NK_Expr_Literal_Floating, token}, value{value} {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static FloatingPointLiteral *NONNULL create(Allocator& allocator, Token token, double value) {
            return allocate(allocator, [&](auto space) {
                return new(space) FloatingPointLiteral{token, value};
            });
        }

        double getValue() const {
            return value;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Literal_Floating;
        }
    };
    
    class CharacterLiteral : public Literal {
    public:
        using Character = u32;
    private:
        const Character value;

        CharacterLiteral(Token token, Character value) : Literal{NK_Expr_Literal_Character, token}, value{value} {}
    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static CharacterLiteral *NONNULL create(Allocator& allocator, Token token, Character value) {
            return allocate(allocator, [&](auto space) {
                return new(space) CharacterLiteral{token, value};
            });
        }
        
        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Literal_Character;
        }
    };

    class StringLiteral : public Literal {
        String value;

        StringLiteral(Token token, String value) : Literal{NK_Expr_Literal_String, token}, value{std::move(value)} {}
    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static StringLiteral *NONNULL create(Allocator& allocator, Token token, String value) {
            return allocate(allocator, [&](auto space) {
                return new(space) StringLiteral{token, std::move(value)};
            });
        }

        const String& getValue() const {
            return value;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Literal_String;
        }
    };

    class CallExpression : public Expression {
        Expression *NONNULL target;
        Span<Expression *NONNULL> arguments;

    protected:
        CallExpression(Token token, Expression *NONNULL target, Span<Expression *NONNULL> arguments) 
            : Expression{NK_Expr_Call, token}
            , target{target}
            , arguments{arguments} {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static CallExpression *NONNULL create(Allocator& allocator, Token token, Expression *NONNULL target, Span<Expression *NONNULL> arguments) {
            return allocate(allocator, [&](auto space) {
                return new(space) CallExpression{token, target, arguments};
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

        void setArgument(size_t i, Expression *NONNULL argument) {
            arguments[i] = argument;
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

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static SubscriptExpression *NONNULL create(Allocator& allocator, Token token, Expression *NONNULL target, Expression *NONNULL index) {
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

        void setIndex(Expression *NONNULL index) {
            this->index = index;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Subscript;
        }
    };

    class MemberAccessExpression : public Expression {
        Expression *NONNULL target;
        Symbol& memberName;
        MemberResolution resolution;

    protected:
        MemberAccessExpression(Token token, Expression *NONNULL target, Symbol& member) 
            : Expression{NK_Expr_Member_Access, token}
            , target{target}
            , memberName{member} {}
    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static MemberAccessExpression *NONNULL create(Allocator& allocator, Token token, Expression *NONNULL target, Symbol& member) {
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

        MemberResolution getResolution() const {
            return resolution;
        }

        void setResolution(MemberResolution resolution) {
            this->resolution = resolution;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Member_Access;
        }
    };

    class InferredMemberAccessExpression : public Expression {
        Symbol& memberName;
        MemberResolution resolution;
        Type *NULLABLE inferredTarget = nullptr;

    protected:
        InferredMemberAccessExpression(Token token, Symbol& memberName)
            : Expression{NK_Expr_Inferred_Member_Access, token}
            , memberName{memberName} {}
    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static InferredMemberAccessExpression *NONNULL create(Allocator& allocator, Token token, Symbol& memberName) {
            return allocate(allocator, [&](auto space) {
                return new(space) InferredMemberAccessExpression{token, memberName};
            });
        }

        const Symbol& getMemberName() const {
            return memberName;
        }

        const MemberResolution getResolution() const {
            return resolution;
        }

        void setResolution(MemberResolution resolution) {
            this->resolution = std::move(resolution);
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Inferred_Member_Access;
        }
    };

    class InitializerExpression : public Expression {
    public:
        struct Pair {
            InferredMemberAccessExpression *NONNULL name;
            Expression *NONNULL value;

            Pair(InferredMemberAccessExpression *NONNULL name, Expression *NONNULL value)
                : name{name}, value{value} {}
        };
    private:       
        // This is a typename. Could contain a type parameter in the future. We should find a better type.
        Identifier *NULLABLE identifier;
        Span<Pair> pairs;
    protected:
        InitializerExpression(Token token, Identifier *NULLABLE identifier, Span<Pair> pairs) 
            : Expression{NK_Expr_Initializer, token}
            , identifier{identifier}
            , pairs{pairs}
            {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static InitializerExpression *NONNULL create(Allocator& allocator, Token token, Identifier *NULLABLE identifier, Span<Pair> pairs) {
            return allocate(allocator, [&](auto space) {
                return new(space) InitializerExpression{token, identifier, pairs};
            });
        }

        Identifier *NULLABLE getIdentifier() {
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
        PrefixDereference,
        PostfixDereference,
        ForceUnwrap,
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

        UnaryExpression(u32 offset, UnaryOperator op, Expression *NONNULL target) 
            : Expression{NK_Expr_Unary, offset}, op{op}, target{target}
        {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static UnaryExpression *NONNULL create(Allocator& allocator, Token token, UnaryOperator op, Expression *NONNULL target) {
            return allocate(allocator, [&](auto space) {
                return new(space) UnaryExpression{token, op, target};
            });
        }

        template <Allocator Allocator>
        static UnaryExpression *NONNULL wrap(Allocator& allocator, Expression& target, UnaryOperator op, Type& type) {
            auto result = allocate(allocator, [&](auto space) {
                return new(space) UnaryExpression(target.offset, op, &target);
            });
            result->setType(&type);
            return result;
        }

        UnaryOperator getOp() const {
            return op;
        }

        Expression& getTarget() const {
            return *target;
        }

        void setTarget(Expression *NONNULL target) {
            this->target = target;
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
            : Expression{NK_Expr_Binary, token}
            , op{op}
            , left{left}
            , right{right} 
        {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static BinaryExpression *NONNULL create(Allocator& allocator, Token token, BinaryOperator op, Expression *NONNULL left, Expression *NONNULL right) {
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

        void setLeft(Expression *NONNULL left) {
            this->left = left;
        }

        void setWrappedLeft(Expression *NONNULL left) {
            this->left = left;
        }

        Expression& getRight() const {
            return *right;
        }

        void setRight(Expression *NONNULL right) {
            this->right = right;
        }

        void setWrappedRight(Expression *NONNULL right) {
            this->right = right;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Expr_Binary;
        }
    };


    /* This node captures a call if it is present, instead of relying on CallExpression.
     * This is done to make the type checking process easier.
     * If a non-callable intrinsic ever has to return a callable, this should be reconsidered.
     */
    class IntrinsicExpression : public Expression {
        IntrinsicKind intrinsic;
    public:
        const bool hasTypeArguments;
        const bool hasCall;
    private:
        // TODO: These vectors should be arrays, with LBO for size <= 1.
        Span<TypeNode *NONNULL> typeArguments;
        Span<Expression *NONNULL> arguments;
        Symbol& name;

        IntrinsicExpression(
            Token token, 
            Symbol& name, 
            bool hasTypeArguments, 
            Span<TypeNode *NONNULL> typeArguments, 
            bool hasCall,
            Span<Expression *NONNULL> arguments
        ) : Expression{NK_Expr_Intrinsic, token}
          , name{name}
          , hasTypeArguments{hasTypeArguments}
          , typeArguments{typeArguments}
          , hasCall{hasCall}
          , arguments{arguments} {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static IntrinsicExpression *NONNULL create(
            Allocator& allocator, 
            Token token, 
            Symbol& name, 
            bool hasTypeArguments,
            Span<TypeNode *NONNULL> typeArguments, 
            bool hasCall,
            Span<Expression *NONNULL> arguments
        ) {
            return allocate(allocator, [&](auto space) {
                return new(space) IntrinsicExpression(token, name, hasTypeArguments, typeArguments, hasCall, arguments);
            });
        }

        IntrinsicKind getIntrinsic() const {
            return intrinsic;
        }

        void setIntrinsic(IntrinsicKind intrinsic) {
            this->intrinsic = intrinsic;
        }

        const Symbol& getName() const {
            return name;
        }

        const Span<TypeNode *NONNULL>& getTypeArguments() const {
            return typeArguments;
        }

        Span<Expression *NONNULL>& getArguments() {
            return arguments;
        }

        const Span<Expression *NONNULL>& getArguments() const {
            return arguments;
        }
    };

    // Bindings
    class Binding : public Node {
        Type *NULLABLE type = nullptr;

        // TODO: Binding visitor
    protected:
        using Node::Node;

    public:
        void setType(Type *NONNULL type) {
            this->type = type;
        }

        Type *NONNULL getType() const {
            return type;
        }

        bool hasType() const {
            return type != nullptr;
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


    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator A>
        static IdentifierBinding *NONNULL create(A& allocator, Token token, Symbol& identifier) {
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

    class VariableDeclaration;

    using Condition = llvm::PointerUnion<VariableDeclaration *NONNULL, Expression *NONNULL>;

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
            : Statement{NK_Stmt_Assignment, token}
            , target{target}
            , value{value} 
        {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static AssignmentStatement *NONNULL create(Allocator& allocator, Token token, Expression *NONNULL target, Expression *NONNULL value) {
            return allocate(allocator, [&](auto space) {
                return new(space) AssignmentStatement{token, target, value};
            });
        }

        Expression& getTarget() const {
            return *target;
        }

        void setTarget(Expression *NONNULL target) {
            this->target = target;
        }

        Expression& getValue() const {
            return *value;
        }

        void setValue(Expression *NONNULL value) {
            this->value = value;
        }

        void setWrappedValue(Expression *NONNULL wrapped) {
            value = wrapped;
        }
    };

    class CompoundAssignmentStatement : public Statement {
    protected:
        BinaryOperator op;
        Expression *NONNULL target;
        Expression *NONNULL operand;

        CompoundAssignmentStatement(Token token, BinaryOperator op, Expression *NONNULL target, Expression *NONNULL operand)
            : Statement{NK_Stmt_Compound_Assignment, token}
            , op{op}
            , target{target}
            , operand{operand} 
        {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

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

        void setTarget(Expression *NONNULL target) {
            this->target = target;
        }

        Expression& getOperand() const {
            return *operand;
        }

        void setOperand(Expression *NONNULL operand) {
            this->operand = operand;
        }

        void setWrappedOperand(Expression *NONNULL wrapped) {
            operand = wrapped;
        }
    };

    class IfStatement : public Statement {
    public:
        class Branch {
            Span<Condition> conditions;
            Block block;

        public:
            Branch(Span<Condition> conditions, Block block) : conditions{conditions}, block{block} {}

            size_t getNumConditions() const {
                return conditions.size();
            }

            Condition getCondition(size_t i) const {
                return conditions[i];
            }

            Span<Condition> getConditions() const {
                return conditions;
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
        Span<Branch> branches;
        std::optional<Block> fallback;

        // NOTE: this would be safer if it took a conditional, and a vector of subsequent ones, but initialization becomes more troublesome that way.
        IfStatement(Token token, Span<Branch> branches, std::optional<Block> fallback) 
            : Statement{NK_Stmt_If, token}
            , branches{branches}
            , fallback{fallback}
        {
            assert(this->branches.size() > 0 && "if statement must have at least one condition.");
        }

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static IfStatement *NONNULL create(Allocator& allocator, Token token, Span<Branch> branches, std::optional<Block> fallback) {
            return allocate(allocator, [&](auto space) {
                return new(space) IfStatement{token, branches, fallback};
            });
        }

        size_t getConditionCount() const {
            return branches.size();
        }

        Branch& getBranch(size_t i) {
            return branches[i];
        }

        const Branch& getBranch(size_t i) const {
            return branches[i];
        }

        // TODO: Remove these
        Branch& getCondition(size_t i) {
            return branches[i];
        }

        const Branch& getCondition(size_t i) const {
            return branches[i];
        }

        Block *NULLABLE getFallback() {
            if (fallback.has_value()) {
                Block& block = fallback.value();
                return &block;
            } else {
                return nullptr;
            }
        }

        const Block *NULLABLE getFallback() const {
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
        Span<Condition> conditions;
        Block block;
    protected:
        GuardStatement(Token token, Span<Condition> conditions, Block block) : Statement{NK_Stmt_Guard, token}, conditions{conditions}, block{block} {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static GuardStatement *NONNULL create(Allocator& allocator, Token token, Span<Condition> conditions, Block block) {
            return allocate(allocator, [&](auto space) {
                return new(space) GuardStatement{token, conditions, block};
            });
        }

        Span<Condition>& getConditions() {
            return conditions;
        }

        const Span<Condition>& getConditions() const {
            return conditions;
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

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static ReturnStatement *NONNULL create(Allocator& allocator, Token token, Expression *NONNULL expression) {
            return allocate(allocator, [&](auto space) {
                return new(space) ReturnStatement{token, expression};

            });
        }

        Expression *NONNULL getValue() const {
            return value;
        }

        void setValue(Expression *NONNULL value) {
            this->value = value;
        }

        void setWrappedValue(Expression *NONNULL wrapped) {
            value = wrapped;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Stmt_Return;
        }
    };

    class WhileStatement : public Statement {
    protected:
        Span<Condition> conditions;
        Block code;

        WhileStatement(Token token, Span<Condition> conditions, Block code) 
            : Statement{NK_Stmt_While, token}
            , conditions{conditions}
            , code{code} 
        {}
    
    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static WhileStatement *NONNULL create(Allocator& allocator, Token token, Span<Condition> conditions, Block code) {
            return allocate(allocator, [&](auto space) {
                return new(space) WhileStatement{token, conditions, code};
            });
        }

        size_t getNumConditions() const {
            return conditions.size();
        }

        Condition getCondition(size_t i) const {
            return conditions[i];
        }

        Span<Condition>& getConditions() {
            return conditions;
        }

        const Span<Condition>& getConditions() const {
            return conditions;
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

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

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

        void setIterable(AST::Expression *iterable) {
            this->iterable = iterable;
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

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

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

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

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

        ExpressionStatement(Expression *NONNULL expression) 
            : Statement{NK_Stmt_Expression, expression->offset}, expression{expression} {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

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
        Modifiers modifiers;

        Declaration(Node::Kind kind, Token token, Modifiers modifiers)
            : Node{kind, token}, modifiers{modifiers} {}
        Declaration(Node::Kind kind, u32 offset, Modifiers modifiers)
            : Node{kind, offset}, modifiers{modifiers} {}
    public:
        Modifiers getModifiers() const {
            return modifiers;
        }

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
        bool isChecked = false;

        VariableDeclaration(
            Token token, 
            Modifiers modifiers,
            bool isMutable,
            Binding *NONNULL binding,
            TypeNode *NULLABLE type, 
            Expression *NULLABLE initial
        ) : Declaration{NK_Decl_Variable, token, modifiers}
          , isMutable{isMutable}
          , binding{binding}
          , typeDeclaration{type}
          , initial{initial} {}
        
    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template<Allocator Allocator>
        static VariableDeclaration *NONNULL create(
            Allocator& allocator,
            Token token, 
            Modifiers modifiers,
            bool isMutable,
            Binding *NONNULL binding,
            TypeNode *NULLABLE type, 
            Expression *NULLABLE initial
        ) {
            return allocate(allocator, [&](void *space) {
                return new(space) VariableDeclaration{token, modifiers, isMutable, binding, type, initial};
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

        bool getIsChecked() const {
            return isChecked;
        }

        void markAsChecked() {
            isChecked = true;
        }

        Expression *NULLABLE getInitialValue() const {
            return initial;
        }

        void setInitialValue(Expression *NONNULL initial) {
            initial = initial;
        }

        void setWrappedInitialValue(Expression *NONNULL wrapped) {
            initial = wrapped;
        }

        TypeNode *NULLABLE getTypeDeclaration() const {
            return typeDeclaration;
        }

        Type *NONNULL getType() const {
            return type;
        }

        void setType(Type& type) {
            this->type = &type;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Decl_Variable;
        }

        using enum Modifiers::Modifier;
        static constexpr Modifiers allowedModifiers = {Static, Public, Private};
        static constexpr Modifiers allowedModifersInFunction = {};
        static constexpr Modifiers allowedModifersInGlobal = {Public, Private};
        static constexpr Modifiers allowedModifersInStruct = {Static,Public, Private};
    };

    struct FunctionParameter {
        Symbol *name;
        TypeNode *NONNULL typeDeclaration;
        
        FunctionParameter(Symbol& name, TypeNode *NONNULL type) : name{&name}, typeDeclaration{type} {}
    };

    class FunctionName {
        struct Label {
            u32 offset;
            u32 length;
        };

        const char *NONNULL base;
        u32 length;
        u32 root;
        Span<Label> labels;
    };

    class InitializerDeclaration : public Declaration {
        Span<FunctionParameter> parameters;
        Block code;


    };

    class FunctionDeclaration : public Declaration {
    protected:
        Symbol& name;
        Span<FunctionParameter> parameters;
        u32 arity;
        u32 closingBracket;
        TypeNode *NULLABLE returnTypeDeclaration;
        Block code;

        FunctionDeclaration(Token token, u32 closingBracket, Modifiers modifiers, Symbol& name, Span<FunctionParameter> parameters, TypeNode *NULLABLE returnType, Block code) 
            : Declaration{NK_Decl_Function, token, modifiers}
            , name{name}
            , parameters{parameters}
            , arity{u32(this->parameters.size())}
            , closingBracket{closingBracket}
            , returnTypeDeclaration{returnType}
            , code{code} {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;
        FileLocation getClosingBracketLocation() const;

        template <Allocator A>
        static FunctionDeclaration *NONNULL create(A& allocator, Token token, u32 closingBracket, Modifiers modifiers, Symbol& name, Span<FunctionParameter>&& parameters, TypeNode *NULLABLE returnType, Block code) {
            return allocate(allocator, [&](auto space) {
                return new(space) FunctionDeclaration(token, closingBracket, modifiers, name, parameters, returnType, code);
            });
        }

        const Symbol& getName() const {
            return name;
        }

        TypeNode *NULLABLE getReturnTypeDeclaration() const {
            return returnTypeDeclaration;
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

        using enum Modifiers::Modifier;
        static constexpr Modifiers allowedModifiers = {Static,Public, Private};
        static constexpr Modifiers allowedModifiersInGlobal = {Public, Private};
        static constexpr Modifiers allowedModifiersInStruct = {Static,Public, Private};
    };

    class StructDeclaration : public Declaration {
    protected:
        Symbol& name;
        Span<Declaration *NONNULL> declarations;

        using iterator = Span<Declaration *NONNULL>::iterator::dereferencing_iterator;
        using const_iterator = Span<Declaration *NONNULL>::const_iterator::dereferencing_iterator;

        StructDeclaration(Token token, Modifiers modifiers, Symbol& name, Span<Declaration *NONNULL> declarations)
            : Declaration{NK_Decl_Struct, token, modifiers}
            , name{name}
            , declarations{declarations} {}
    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static StructDeclaration *NONNULL create(Allocator& allocator, Token token, Modifiers modifiers, Symbol& name, Span<Declaration *NONNULL>&& declarations) {
            return allocate(allocator, [&](auto space) {
                return new(space) StructDeclaration{token, modifiers, name, declarations};
            });
        }

        const Symbol& getName() const {
            return name;
        }

        static bool classof(const Node *NONNULL node) {
            return node->getKind() == NK_Decl_Struct;
        }

        iterator begin() {
            return iterator(declarations.begin());
        }

        iterator end() {
            return iterator(declarations.end());
        }

        const_iterator begin() const {
            return const_iterator(declarations.begin());
        }

        const_iterator end() const {
            return const_iterator(declarations.end());
        }

        const_iterator cbegin() const {
            return const_iterator(declarations.cbegin());
        }

        const_iterator cend() const {
            return const_iterator(declarations.cend());
        }

        using enum Modifiers::Modifier;
        static constexpr Modifiers allowedModifiers = {Compact, Unpadded, Private, Public};
    };

    class EnumDeclaration : public Declaration {
    public:
        class Case {
        public:
            class Member {
                Symbol *NULLABLE name;
                TypeNode *NONNULL type;
            public:
                Member(TypeNode *NONNULL type) : name{nullptr}, type{type} {}
                Member(Symbol *NULLABLE name, TypeNode *NONNULL type) : name{name}, type{type} {}

                const Symbol *NULLABLE getName() {
                    return name;
                }

                TypeNode& getType() {
                    return *type;
                }
            };

            using iterator = Span<Member>::iterator;
            using const_iterator = Span<Member>::const_iterator;
        private:
            Symbol *NONNULL name;
            Span<Member> members;
        public:
            Case(Token token, Symbol& name, Span<Member> members) : name{&name}, members{std::move(members)} {}

            const Symbol& getName() const {
                return *name;
            }

            const size_t getMemberCount() const {
                return members.size();
            }

            const bool hasMembers() const {
                return !members.isEmpty();
            }

            iterator begin() {
                return members.begin();
            }

            iterator end() {
                return members.end();
            }

            const_iterator begin() const {
                return members.begin();
            }

            const_iterator end() const {
                return members.end();
            }
        };
    protected:
        Symbol& name;
        TypeNode *NULLABLE rawType;
        Span<Case> cases;
        Span<Declaration *NONNULL> declarations;

        EnumDeclaration(
            Token token, 
            Modifiers modifiers,
            Symbol& name, 
            TypeNode *NULLABLE rawType, 
            Span<Case> cases, 
            Span<Declaration *NONNULL> declarations
        ) 
            : Declaration{NK_Decl_Enum, token, modifiers}
            , name{name}
            , cases{cases}
            , declarations{declarations}
        {}

    public:
        using iterator = Span<Case>::iterator;
        using const_iterator = Span<Case>::const_iterator;

        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

        template <Allocator A>
        static EnumDeclaration *NONNULL create(
            A& allocator, 
            Token token, 
            Modifiers modifiers,
            Symbol& name, 
            TypeNode *NULLABLE rawType, 
            Span<Case>&& cases, 
            Span<Declaration *NONNULL>&& declarations
        ) {
            return allocate(allocator, [&](auto space) {
                return new(space) EnumDeclaration{token, modifiers, name, rawType, cases, declarations};
            });
        }

        size_t getNumberOfCases() const {
            return cases.size();
        }

        iterator begin() {
            return cases.begin();
        }

        iterator end() {
            return cases.end();
        }

        const_iterator begin() const {
            return cases.begin();
        }

        const_iterator end() const {
            return cases.end();
        }

        const Symbol& getName() const {
            return name;
        }

        static constexpr Modifiers allowedModifiers = {};
    };
    
    class StatementDeclaration : public Declaration {
    protected:
        Statement *NONNULL statement;

        StatementDeclaration(Statement *NONNULL statement) 
            : Declaration{NK_Decl_Statement, statement->offset, {}}
            , statement{statement} {}

    public:
        void print(PrintContext& pc) const; 
        FileLocation getFileLocation() const;

        template <Allocator Allocator>
        static StatementDeclaration *NONNULL create(Allocator& allocator, Statement *NONNULL statement) {
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

        ProtocolDeclaration(Token token, Modifiers modifiers, Symbol& name) 
            : Declaration{NK_Decl_Protocol, token, modifiers}
            , name{name} {}

    public:
        void print(PrintContext& pc) const;
        FileLocation getFileLocation() const;

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
        PrintContext(PrintContext&&) = delete;
        PrintContext& operator=(PrintContext&&) = delete;
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

        PrintContext& operator<<(String string) {
            os << string.stringView();
            return *this;
        }

        PrintContext& operator<<(const char *NONNULL str) {
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

        void withSeparator(const char *separator, auto& range) {
            bool needsSeparator = false;
            for (auto element : range) {
                if (needsSeparator) {
                    *this << separator;
                } else {
                    needsSeparator = true;
                }
                if constexpr (std::is_pointer_v<decltype(element)>) {
                    *this << *element;
                } else {
                    *this << element;
                }
            }
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
