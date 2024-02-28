#include "AST.h"

namespace AST {

    template <typename Subclass, typename ReturnType>
    ReturnType Declaration::acceptVisitor(DeclarationVisitorT<Subclass, ReturnType>& visitor) {
        switch (getKind()) {
            case NK_Decl_Variable:
                return visitor.visitVariableDeclaration(*static_cast<VariableDeclaration *>(this));
            case NK_Decl_Function:
                return visitor.visitFunctionDeclaration(*static_cast<FunctionDeclaration *>(this));
            case NK_Decl_Struct:
                return visitor.visitStructDeclaration(*static_cast<StructDeclaration *>(this));
            case NK_Decl_Class:
                return visitor.visitClassDeclaration(*static_cast<ClassDeclaration *>(this));
            case NK_Decl_Protocol:
                return visitor.visitProtocolDeclaration(*static_cast<ProtocolDeclaration *>(this));
            case NK_Decl_Statement:
                return visitor.visitStatementDeclaration(*static_cast<StatementDeclaration *>(this));
            default:
                llvm_unreachable("Unsupported declaration type.");
        }
    }
    
    template <typename Subclass, typename ReturnType>
    ReturnType Statement::acceptVisitor(StatementVisitorT<Subclass, ReturnType>& visitor) {
        switch (getKind()) {
            // TODO: Reorder
            case NK_Stmt_If:
                return visitor.visitIfStatement(*static_cast<IfStatement *>(this));
            case NK_Stmt_Assignment:
                return visitor.visitAssignmentStatement(*static_cast<AssignmentStatement *>(this));
            case NK_Stmt_While:
                return visitor.visitWhileStatement(*static_cast<WhileStatement *>(this));
            case NK_Stmt_For:
                return visitor.visitForStatement(*static_cast<ForStatement *>(this));
            case NK_Stmt_Return:
                return visitor.visitReturnStatement(*static_cast<ReturnStatement *>(this));
            case NK_Stmt_Expression:
                return visitor.visitExpressionStatement(*static_cast<ExpressionStatement *>(this));
            default:
                llvm_unreachable("Unsupported statement type.");
        }
    }

    template <typename Subclass, typename ReturnType>
    ReturnType Expression::acceptVisitor(ExpressionVisitorT<Subclass, ReturnType>& visitor) {
        switch (getKind()) {
            case NK_Expr_Literal:
                return visitor.visitLiteral(*static_cast<Literal *>(this));
            case NK_Expr_Identifier:
                return visitor.visitIdentifier(*static_cast<Identifier *>(this));
            case NK_Expr_Unary:
                return visitor.visitUnaryExpression(*static_cast<UnaryExpression *>(this));
            case NK_Expr_Binary:
                return visitor.visitBinaryExpression(*static_cast<BinaryExpression *>(this));
            case NK_Expr_Call:
                return visitor.visitCallExpression(*static_cast<CallExpression *>(this));
            default:
                llvm_unreachable("Unsupported expression type");
        }
    }

    template <typename Subclass, typename ReturnType>
    class DeclarationVisitorT {
    public:
        ReturnType visitVariableDeclaration(VariableDeclaration& variable) {
            return static_cast<Subclass *>(this)->visitVariableDeclaration(variable);
        }

        ReturnType visitFunctionDeclaration(FunctionDeclaration& function) {
            return static_cast<Subclass *>(this)->visitFunctionDeclaration(function);
        }

        ReturnType visitStructDeclaration(StructDeclaration& structDeclaration) {
            return static_cast<Subclass *>(this)->visitStructDeclaration(structDeclaration);
        }

        ReturnType visitEnumDeclaration(EnumDeclaration& enumDeclaration) {
            return static_cast<Subclass *>(this)->visitEnumDeclaration(enumDeclaration);
        }

        ReturnType visitClassDeclaration(ClassDeclaration& classDeclaration) {
            return static_cast<Subclass *>(this)->visitClassDeclaration(classDeclaration);
        }

        ReturnType visitProtocolDeclaration(ProtocolDeclaration& protocolDeclaration) {
            return static_cast<Subclass *>(this)->visitProtocolDeclaration(protocolDeclaration);
        }

        ReturnType visitStatementDeclaration(StatementDeclaration& statement) {
            return static_cast<Subclass *>(this)->visitStatementDeclaration(statement);
        }
    };

    template <typename Subclass, typename ReturnType>
    class StatementVisitorT {
    public:
        // TODO: Reorder
        
        ReturnType visitIfStatement(IfStatement& ifStatement) {
            return static_cast<Subclass *>(this)->visitIfStatement(ifStatement);
        }

        ReturnType visitAssignmentStatement(IfStatement& assignmentStatement) {
            return static_cast<Subclass *>(this)->visitAssignmentStatement(assignmentStatement);
        }

        // TODO: Add for statement

        ReturnType visitWhileStatement(WhileStatement& whileStatement) {
            return static_cast<Subclass *>(this)->visitWhileStatement(whileStatement);
        }

        ReturnType visitReturnStatement(ReturnStatement& returnStatement) {
            return static_cast<Subclass *>(this)->visitReturnStatement(returnStatement);
        }

        ReturnType visitExpressionStatement(ExpressionStatement& expression) {
            return static_cast<Subclass *>(this)->visitExpressionStatement(expression);
        }
    };

    template <typename Subclass, typename ReturnType>
    class ExpressionVisitorT {
    public:
        ReturnType visitIdentifier(Identifier& identifier) {
            return static_cast<Subclass *>(this)->visitIdentifier(identifier);
        }

        ReturnType visitLiteral(Literal& literal) {
            return static_cast<Subclass *>(this)->visitLiteral(literal);
        }

        ReturnType visitUnaryExpression(UnaryExpression& unary) {
            return static_cast<Subclass *>(this)->visitUnaryExpression(unary);
        }

        ReturnType visitBinaryExpression(BinaryExpression& binary) {
            return static_cast<Subclass *>(this)->visitBinaryExpression(binary);
        }

        ReturnType visitCallExpression(CallExpression& call) {
            return static_cast<Subclass *>(this)->visitCallExpression(call);
        }
    };


}
