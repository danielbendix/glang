#ifndef LANG_ast_h
#define LANG_ast_h

#include <vector>

/* The declaration of all AST nodes could be done in a simpler, declarative language.
 * That defines all the properties, and if they're public or accessed via getter/setter;
 * This would ensure consistency, and ease of changing conventions.
 */

namespace AST {
    class Visitor;

    class Node {
        struct Location {
            // TODO: SourceFile reference
            int line;
            int offset;
        };
        Location location;

        virtual void acceptVisitor(Visitor& visitor) = 0;
    };

    // Declarations

    class Declaration : public Node {
        
    };

    class FunctionDeclaration : public Declaration {
    public:
        class Parameter {
            std::string name;
            


        };

    };

    class EnumDeclaration : public Declaration {

    };

    class StructDeclaration : public Declaration {

    };

    class StatementDeclaration : public Declaration {

    };

    class ProtocolDeclaration : public Declaration {

    };

    class ClassDeclaration : public Declaration {

    };

    // Statements

    class Statement : public Node {
        
    };

    class AssignmentStatement : public Statement {
        
    };

    class ExpressionStatement : public Statement {
        
    };

    // Types

    class Type : public Node {
        
    };

    class TypeLiteral : public Type {
        std::string identifier;
    };

    // Expressions

    class Expression : public Node {

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
            std::string string;
        };

        Type type;
        Internal internal;
    public:
        Value(bool boolean) : type{Type::Boolean}, internal{.boolean=boolean} {}
        Value(int8_t int8) : type{Type::Int8}, internal{.int8=int8} {}
        Value(int16_t int16) : type{Type::Int16}, internal{.int16=int16} {}
        Value(int32_t int32) : type{Type::Int32}, internal{.int32=int32} {}
        Value(int64_t int64) : type{Type::Int64}, internal{.int64=int64} {}
        Value(uint8_t uint8) : type{Type::UInt8}, internal{.uint8=uint8} {}
        Value(uint16_t uint16) : type{Type::UInt16}, internal{.uint16=uint16} {}
        Value(uint32_t uint32) : type{Type::UInt32}, internal{.uint32=uint32} {}
        Value(uint64_t uint64) : type{Type::UInt64}, internal{.uint64=uint64} {}
        Value(float float_) : type{Type::Float}, internal{.float_=float_} {}
        Value(double double_) : type{Type::Double}, internal{.double_=double_} {}
        Value(std::string) : type{Type::String}, internal{.string=string} {}
    };

    class CallExpression : public Expression {
        Expression *callee;
        std::vector<Expression *> arguments;

    public:
        CallExpression(Expression& callee) : callee{&callee} {}
    };

    class BinaryExpression : public Expression {

        Expression *left;
        Expression *right;

    public:
        BinaryExpression(Expression& left, Expression& right) : left{&left}, right{&right} {}
        
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
    };

    class Visitor {
        virtual void visitDeclaration(Declaration& declaration) = 0;
        virtual void visitFunctionDeclaration(FunctionDeclaration& declaration) = 0;
        virtual void visitStructDeclaration(StructDeclaration& declaration) = 0;
        virtual void visitStatement(Statement& statement) = 0;
        virtual void visitExpressionStatement(ExpressionStatement& expressionStatement) = 0;

        virtual void visitValue(Value& value) = 0;
        virtual void visitCallExpression(CallExpression& callExpression) = 0;
        virtual void visitBinaryExpression(BinaryExpression& binaryExpression) = 0;
    };

};

#endif
