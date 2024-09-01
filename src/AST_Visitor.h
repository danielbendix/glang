#ifndef LANG_ast_visitor_h
#define LANG_ast_visitor_h

#include "AST.h"

namespace AST {
    template <typename Func>
    auto visit(Node& node, Func&& visitor) {
        using enum Node::Kind;
        switch (node.getKind()) {
            case NK_Decl_Variable:
                return std::invoke(visitor, *static_cast<VariableDeclaration *>(&node));
            case NK_Decl_Function:
                return std::invoke(visitor, *static_cast<FunctionDeclaration *>(&node));
            case NK_Decl_Struct:
                return std::invoke(visitor, *static_cast<StructDeclaration *>(&node));
            case NK_Decl_Enum:
                return std::invoke(visitor, *static_cast<EnumDeclaration *>(&node));
            case NK_Decl_Protocol:
                return std::invoke(visitor, *static_cast<ProtocolDeclaration *>(&node));
            case NK_Decl_Statement:
                return std::invoke(visitor, *static_cast<StatementDeclaration *>(&node));
            case NK_Stmt_Assignment:
                return std::invoke(visitor, *static_cast<AssignmentStatement *>(&node));
            case NK_Stmt_Compound_Assignment:
                return std::invoke(visitor, *static_cast<CompoundAssignmentStatement *>(&node));
            case NK_Stmt_If:
                return std::invoke(visitor, *static_cast<IfStatement *>(&node));
            case NK_Stmt_Guard:
                return std::invoke(visitor, *static_cast<GuardStatement *>(&node));
            case NK_Stmt_Return:
                return std::invoke(visitor, *static_cast<ReturnStatement *>(&node));
            case NK_Stmt_While:
                return std::invoke(visitor, *static_cast<WhileStatement *>(&node));
            case NK_Stmt_For:
                return std::invoke(visitor, *static_cast<ForStatement *>(&node));
            case NK_Stmt_Break:
                return std::invoke(visitor, *static_cast<BreakStatement *>(&node));
            case NK_Stmt_Continue:
                return std::invoke(visitor, *static_cast<ContinueStatement *>(&node));
            case NK_Stmt_Expression:
                return std::invoke(visitor, *static_cast<ExpressionStatement *>(&node));
            case NK_Expr_Identifier:
                return std::invoke(visitor, *static_cast<Identifier *>(&node));
            case NK_Expr_Self:
                return std::invoke(visitor, *static_cast<Self *>(&node));
            case NK_Expr_Literal_Nil:
                return std::invoke(visitor, *static_cast<NilLiteral *>(&node));
            case NK_Expr_Literal_False:
            case NK_Expr_Literal_True:
                return std::invoke(visitor, *static_cast<BooleanLiteral *>(&node));
            case NK_Expr_Literal_Character:
                return std::invoke(visitor, *static_cast<CharacterLiteral *>(&node));
            case NK_Expr_Literal_String:
                return std::invoke(visitor, *static_cast<StringLiteral *>(&node));
            case NK_Expr_Literal_Integer:
                return std::invoke(visitor, *static_cast<IntegerLiteral *>(&node));
            case NK_Expr_Literal_Floating:
                return std::invoke(visitor, *static_cast<FloatingPointLiteral *>(&node));
            case NK_Expr_Unary:
                return std::invoke(visitor, *static_cast<UnaryExpression *>(&node));
            case NK_Expr_Binary:
                return std::invoke(visitor, *static_cast<BinaryExpression *>(&node));
            case NK_Expr_Intrinsic:
                return std::invoke(visitor, *static_cast<IntrinsicExpression *>(&node));
            case NK_Expr_Call:
                return std::invoke(visitor, *static_cast<CallExpression *>(&node));
            case NK_Expr_Subscript:
                return std::invoke(visitor, *static_cast<SubscriptExpression *>(&node));
            case NK_Expr_Initializer:
                return std::invoke(visitor, *static_cast<InitializerExpression *>(&node));
            case NK_Expr_Member_Access:
                return std::invoke(visitor, *static_cast<MemberAccessExpression *>(&node));
            case NK_Expr_Inferred_Member_Access:
                return std::invoke(visitor, *static_cast<InferredMemberAccessExpression *>(&node));
            case NK_Type_Literal:
                return std::invoke(visitor, *static_cast<TypeLiteral *>(&node));
            case NK_Type_Modifier:
                return std::invoke(visitor, *static_cast<TypeModifier *>(&node));
            case NK_Binding_Identifier:
                return std::invoke(visitor, *static_cast<IdentifierBinding *>(&node));
        }
    }

    template <typename Func>
    auto visit(const Node& node, Func&& visitor) {
        using enum Node::Kind;
        switch (node.getKind()) {
            case NK_Decl_Variable:
                return std::invoke(visitor, *static_cast<const VariableDeclaration *>(&node));
            case NK_Decl_Function:
                return std::invoke(visitor, *static_cast<const FunctionDeclaration *>(&node));
            case NK_Decl_Struct:
                return std::invoke(visitor, *static_cast<const StructDeclaration *>(&node));
            case NK_Decl_Enum:
                return std::invoke(visitor, *static_cast<const EnumDeclaration *>(&node));
            case NK_Decl_Protocol:
                return std::invoke(visitor, *static_cast<const ProtocolDeclaration *>(&node));
            case NK_Decl_Statement:
                return std::invoke(visitor, *static_cast<const StatementDeclaration *>(&node));
            case NK_Stmt_Assignment:
                return std::invoke(visitor, *static_cast<const AssignmentStatement *>(&node));
            case NK_Stmt_Compound_Assignment:
                return std::invoke(visitor, *static_cast<const CompoundAssignmentStatement *>(&node));
            case NK_Stmt_If:
                return std::invoke(visitor, *static_cast<const IfStatement *>(&node));
            case NK_Stmt_Guard:
                return std::invoke(visitor, *static_cast<const GuardStatement *>(&node));
            case NK_Stmt_Return:
                return std::invoke(visitor, *static_cast<const ReturnStatement *>(&node));
            case NK_Stmt_While:
                return std::invoke(visitor, *static_cast<const WhileStatement *>(&node));
            case NK_Stmt_For:
                return std::invoke(visitor, *static_cast<const ForStatement *>(&node));
            case NK_Stmt_Break:
                return std::invoke(visitor, *static_cast<const BreakStatement *>(&node));
            case NK_Stmt_Continue:
                return std::invoke(visitor, *static_cast<const ContinueStatement *>(&node));
            case NK_Stmt_Expression:
                return std::invoke(visitor, *static_cast<const ExpressionStatement *>(&node));
            case NK_Expr_Identifier:
                return std::invoke(visitor, *static_cast<const Identifier *>(&node));
            case NK_Expr_Self:
                return std::invoke(visitor, *static_cast<const Self *>(&node));
            case NK_Expr_Literal_Nil:
                return std::invoke(visitor, *static_cast<const NilLiteral *>(&node));
            case NK_Expr_Literal_False:
            case NK_Expr_Literal_True:
                return std::invoke(visitor, *static_cast<const BooleanLiteral *>(&node));
            case NK_Expr_Literal_Character:
                return std::invoke(visitor, *static_cast<const CharacterLiteral *>(&node));
            case NK_Expr_Literal_String:
                return std::invoke(visitor, *static_cast<const StringLiteral *>(&node));
            case NK_Expr_Literal_Integer:
                return std::invoke(visitor, *static_cast<const IntegerLiteral *>(&node));
            case NK_Expr_Literal_Floating:
                return std::invoke(visitor, *static_cast<const FloatingPointLiteral *>(&node));
            case NK_Expr_Unary:
                return std::invoke(visitor, *static_cast<const UnaryExpression *>(&node));
            case NK_Expr_Binary:
                return std::invoke(visitor, *static_cast<const BinaryExpression *>(&node));
            case NK_Expr_Intrinsic:
                return std::invoke(visitor, *static_cast<const IntrinsicExpression *>(&node));
            case NK_Expr_Call:
                return std::invoke(visitor, *static_cast<const CallExpression *>(&node));
            case NK_Expr_Subscript:
                return std::invoke(visitor, *static_cast<const SubscriptExpression *>(&node));
            case NK_Expr_Initializer:
                return std::invoke(visitor, *static_cast<const InitializerExpression *>(&node));
            case NK_Expr_Member_Access:
                return std::invoke(visitor, *static_cast<const MemberAccessExpression *>(&node));
            case NK_Expr_Inferred_Member_Access:
                return std::invoke(visitor, *static_cast<const InferredMemberAccessExpression *>(&node));
            case NK_Type_Literal:
                return std::invoke(visitor, *static_cast<const TypeLiteral *>(&node));
            case NK_Type_Modifier:
                return std::invoke(visitor, *static_cast<const TypeModifier *>(&node));
            case NK_Binding_Identifier:
                return std::invoke(visitor, *static_cast<const IdentifierBinding *>(&node));
        }
    }
}

namespace AST {
    template <typename Func>
    auto visitLiteral(Literal& node, Func&& visitor) {
        using enum Node::Kind;
        switch (node.getKind()) {
            case NK_Expr_Literal_Nil:
                return std::invoke(visitor, *static_cast<NilLiteral *>(&node));
            case NK_Expr_Literal_False:
            case NK_Expr_Literal_True:
                return std::invoke(visitor, *static_cast<BooleanLiteral *>(&node));
            case NK_Expr_Literal_Character:
                return std::invoke(visitor, *static_cast<CharacterLiteral *>(&node));
            case NK_Expr_Literal_String:
                return std::invoke(visitor, *static_cast<StringLiteral *>(&node));
            case NK_Expr_Literal_Integer:
                return std::invoke(visitor, *static_cast<IntegerLiteral *>(&node));
            case NK_Expr_Literal_Floating:
                return std::invoke(visitor, *static_cast<FloatingPointLiteral *>(&node));
            default:
                llvm_unreachable("Unsupported literal kind.");
        }
    }

    template <typename Func>
    auto visitLiteral(const Literal& node, Func&& visitor) {
        using enum Node::Kind;
        switch (node.getKind()) {
            case NK_Expr_Literal_Nil:
                return std::invoke(visitor, *static_cast<const NilLiteral *>(&node));
            case NK_Expr_Literal_False:
            case NK_Expr_Literal_True:
                return std::invoke(visitor, *static_cast<const BooleanLiteral *>(&node));
            case NK_Expr_Literal_Character:
                return std::invoke(visitor, *static_cast<const CharacterLiteral *>(&node));
            case NK_Expr_Literal_String:
                return std::invoke(visitor, *static_cast<const StringLiteral *>(&node));
            case NK_Expr_Literal_Integer:
                return std::invoke(visitor, *static_cast<const IntegerLiteral *>(&node));
            case NK_Expr_Literal_Floating:
                return std::invoke(visitor, *static_cast<const FloatingPointLiteral *>(&node));
            default:
                llvm_unreachable("Unsupported literal kind.");
        }
    }
}

namespace AST {

    template <typename Subclass, typename ReturnType, typename... Args>
    ReturnType Declaration::acceptVisitor(DeclarationVisitorT<Subclass, ReturnType, Args...>& visitor, Args&&... args) {
        return visitor.visit(*this, std::forward<Args>(args)...);
    }
    
    template <typename Subclass, typename ReturnType, typename... Args>
    ReturnType Statement::acceptVisitor(StatementVisitorT<Subclass, ReturnType, Args...>& visitor, Args&&... args) {
        return visitor.visit(*this, std::forward<Args>(args)...);
    }

    template <typename Subclass, typename ReturnType, typename... Args>
    ReturnType Expression::acceptVisitor(ExpressionVisitorT<Subclass, ReturnType, Args...>& visitor, Args... args) {
        return visitor.visit(*this, std::forward<Args>(args)...);
    }

    template <typename Subclass, typename ReturnType, typename... Args>
    ReturnType TypeNode::acceptVisitor(TypeNodeVisitorT<Subclass, ReturnType, Args...>& visitor, Args... args) {
        return visitor.visit(*this, std::forward<Args>(args)...);
    }

    template <typename Subclass, typename ReturnType, typename... Args>
    class DeclarationVisitorT {
        Subclass& subclass() {
            return *static_cast<Subclass *>(this);
        }
    public:
        ReturnType visit(AST::Declaration& declaration, Args&&... args) {
            using enum Node::Kind;
            switch (declaration.getKind()) {
                case NK_Decl_Variable:
                    return subclass().visitVariableDeclaration(*static_cast<VariableDeclaration *>(&declaration), std::forward<Args>(args)...);
                case NK_Decl_Function:
                    return subclass().visitFunctionDeclaration(*static_cast<FunctionDeclaration *>(&declaration), std::forward<Args>(args)...);
                case NK_Decl_Struct:
                    return subclass().visitStructDeclaration(*static_cast<StructDeclaration *>(&declaration), std::forward<Args>(args)...);
                case NK_Decl_Enum:
                    return subclass().visitEnumDeclaration(*static_cast<EnumDeclaration *>(&declaration), std::forward<Args>(args)...);
                case NK_Decl_Protocol:
                    return subclass().visitProtocolDeclaration(*static_cast<ProtocolDeclaration *>(&declaration), std::forward<Args>(args)...);
                case NK_Decl_Statement:
                    return subclass().visitStatementDeclaration(*static_cast<StatementDeclaration *>(&declaration), std::forward<Args>(args)...);
                default:
                    llvm_unreachable("Unsupported declaration type.");
            }
        }
    };

    template <typename Subclass, typename ReturnType, typename... Args>
    class StatementVisitorT {
        Subclass& subclass() {
            return *static_cast<Subclass *>(this);
        }
    public:
        ReturnType visit(AST::Statement& statement, Args&&... args) {
            switch (statement.getKind()) {
                // TODO: Reorder
                using enum Node::Kind;
                case NK_Stmt_If:
                    return subclass().visitIfStatement(*static_cast<IfStatement *>(&statement), std::forward<Args>(args)...);
                case NK_Stmt_Guard:
                    return subclass().visitGuardStatement(*static_cast<GuardStatement *>(&statement), std::forward<Args>(args)...);
                case NK_Stmt_Assignment:
                    return subclass().visitAssignmentStatement(*static_cast<AssignmentStatement *>(&statement), std::forward<Args>(args)...);
                case NK_Stmt_Compound_Assignment:
                    return subclass().visitCompoundAssignmentStatement(*static_cast<CompoundAssignmentStatement *>(&statement), std::forward<Args>(args)...);
                case NK_Stmt_While:
                    return subclass().visitWhileStatement(*static_cast<WhileStatement *>(&statement), std::forward<Args>(args)...);
                case NK_Stmt_For:
                    return subclass().visitForStatement(*static_cast<ForStatement *>(&statement), std::forward<Args>(args)...);
                case NK_Stmt_Return:
                    return subclass().visitReturnStatement(*static_cast<ReturnStatement *>(&statement), std::forward<Args>(args)...);
                case NK_Stmt_Break:
                    return subclass().visitBreakStatement(*static_cast<BreakStatement *>(&statement), std::forward<Args>(args)...);
                case NK_Stmt_Continue:
                    return subclass().visitContinueStatement(*static_cast<ContinueStatement *>(&statement), std::forward<Args>(args)...);
                case NK_Stmt_Expression:
                    return subclass().visitExpressionStatement(*static_cast<ExpressionStatement *>(&statement), std::forward<Args>(args)...);
                default:
                    llvm_unreachable("Unsupported statement type.");
            }
        }
    };

    template <typename Subclass, typename ReturnType, typename... Args>
    class ExpressionVisitorT {
        Subclass& subclass() {
            return *static_cast<Subclass *>(this);
        }

    public:
        ReturnType visit(AST::Expression& expression, Args&&... args) {
            using enum Node::Kind;
            switch (expression.getKind()) {
                case NK_Expr_Literal_Nil:
                case NK_Expr_Literal_False:
                case NK_Expr_Literal_True:
                case NK_Expr_Literal_Integer:
                case NK_Expr_Literal_Floating:
                case NK_Expr_Literal_Character:
                case NK_Expr_Literal_String:
                    return subclass().visitLiteral(*static_cast<Literal *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Identifier:
                    return subclass().visitIdentifier(*static_cast<Identifier *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Unary:
                    return subclass().visitUnaryExpression(*static_cast<UnaryExpression *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Binary:
                    return subclass().visitBinaryExpression(*static_cast<BinaryExpression *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Intrinsic:
                    return subclass().visitIntrinsicExpression(*static_cast<IntrinsicExpression *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Call:
                    return subclass().visitCallExpression(*static_cast<CallExpression *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Subscript:
                    return subclass().visitSubscriptExpression(*static_cast<SubscriptExpression *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Initializer:
                    return subclass().visitInitializerExpression(*static_cast<InitializerExpression *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Member_Access:
                    return subclass().visitMemberAccessExpression(*static_cast<MemberAccessExpression *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Inferred_Member_Access:
                    return subclass().visitInferredMemberAccessExpression(*static_cast<InferredMemberAccessExpression *>(&expression), std::forward<Args>(args)...);
                default:
                    llvm_unreachable("Unsupported expression type");
            }
        }
    };

    template <typename Subclass, typename ReturnType, typename... Args>
    class TypeNodeVisitorT {
        Subclass& subclass() {
            return *static_cast<Subclass *>(this);
        }

    public:
        ReturnType visit(AST::TypeNode& typeNode, Args&&... args) {
            using enum Node::Kind;
            switch (typeNode.getKind()) {
                case NK_Type_Literal:
                    return subclass().visitTypeLiteral(*static_cast<TypeLiteral *>(&typeNode), std::forward<Args>(args)...);
                case NK_Type_Modifier:
                    return subclass().visitTypeModifier(*static_cast<TypeModifier *>(&typeNode), std::forward<Args>(args)...);
                default:
                    llvm_unreachable("Unsupported type node kind.");
            }
        }
    };
}

#endif // LANG_ast_visitor_h
