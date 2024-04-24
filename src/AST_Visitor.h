#ifndef LANG_ast_visitor_h
#define LANG_ast_visitor_h

#include "AST.h"

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
                case NK_Expr_Literal:
                    return subclass().visitLiteral(*static_cast<Literal *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Identifier:
                    return subclass().visitIdentifier(*static_cast<Identifier *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Unary:
                    return subclass().visitUnaryExpression(*static_cast<UnaryExpression *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Binary:
                    return subclass().visitBinaryExpression(*static_cast<BinaryExpression *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Call:
                    return subclass().visitCallExpression(*static_cast<CallExpression *>(&expression), std::forward<Args>(args)...);
                case NK_Expr_Member_Access:
                    return subclass().visitMemberAccessExpression(*static_cast<MemberAccessExpression *>(&expression), std::forward<Args>(args)...);
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
