#include "AST.h"


namespace AST {
    
    // TODO: Use a macro for visiting like llvm/InstVisitor.h#27
    template <typename Subclass, typename ReturnType>
    class ExpressionVisitorT {

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


    class ExpressionVisitorImpl : ExpressionVisitorT<ExpressionVisitorImpl, int> {
        int visitIdentifier(Identifier *identifier) {
            return 1;
        }

        int visitLiteral(Literal *literal) {
            return 0;
        }
    };
}

namespace AST {

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

    // Declaration

    void FunctionDeclaration::acceptVisitor(DeclarationVisitor& visitor) {
        visitor.visitFunctionDeclaration(*this);
    }

    void VariableDeclaration::acceptVisitor(DeclarationVisitor& visitor) {
        visitor.visitVariableDeclaration(*this);
    }

    void StatementDeclaration::acceptVisitor(DeclarationVisitor& visitor) {
        visitor.visitStatementDeclaration(*this);
    }

    // Statement

    void IfStatement::acceptVisitor(StatementVisitor& visitor) {
        visitor.visitIfStatement(*this);
    }

    void AssignmentStatement::acceptVisitor(StatementVisitor& visitor) {
        visitor.visitAssignmentStatement(*this);
    }

    void ReturnStatement::acceptVisitor(StatementVisitor& visitor) {
        visitor.visitReturnStatement(*this);
    }

    void WhileStatement::acceptVisitor(StatementVisitor& visitor) {
        visitor.visitWhileStatement(*this);
    }

    void ExpressionStatement::acceptVisitor(StatementVisitor& visitor) {
        visitor.visitExpressionStatement(*this);
    }

    // Expression

    void UnaryExpression::acceptVisitor(ExpressionVisitor& visitor) {
        visitor.visitUnaryExpression(*this);
    }

    void BinaryExpression::acceptVisitor(ExpressionVisitor& visitor) {
        visitor.visitBinaryExpression(*this);
    }

    void CallExpression::acceptVisitor(ExpressionVisitor& visitor) {
        visitor.visitCallExpression(*this);
    }

    void Identifier::acceptVisitor(ExpressionVisitor& visitor) {
        visitor.visitIdentifier(*this);
    }

    void Literal::acceptVisitor(ExpressionVisitor& visitor) {
        visitor.visitLiteral(*this);
    }
}
