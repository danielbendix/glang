#include "sema/fold.h"
#include "sema/fold/integer.h"

#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

using llvm::cast;
using llvm::TypeSwitch;
using enum AST::Node::Kind;

AST::Literal *NULLABLE foldLiteralUnary(AST::UnaryExpression& unary, AST::Literal& operand) {
    return TypeSwitch<AST::Literal *, AST::Literal *>(&operand)
        .Case([&unary] (AST::IntegerLiteral *integer) {
            return IntegerFold::unary(unary, *integer, nullptr);
        })
        .Default([] (AST::Literal *literal) {
            return nullptr;
        });

    return nullptr;
}

template <typename T, typename Fold>
requires std::derived_from<T, AST::Literal>
AST::Literal *NULLABLE foldBinaryHelper(AST::BinaryExpression& binary, AST::Literal& left, AST::Literal& right, Fold fold) {
    T& leftCasted = cast<T>(left);
    T& rightCasted = cast<T>(right);
    return fold(binary, leftCasted, rightCasted, nullptr);
}

AST::Literal *NULLABLE foldLiteralsBinary(AST::BinaryExpression& binary, AST::Literal& left, AST::Literal& right) {
    if (left.getKind() == right.getKind()) {
        switch (left.getKind()) {
            case NK_Expr_Literal_Integer:
                return foldBinaryHelper<AST::IntegerLiteral>(binary, left, right, IntegerFold::binary);
            default:
                return nullptr;
        }
    }

    return nullptr;
}

/// Allow all constant folding that can occur without a type to proceed.
/// This allows intermediate calculations to exceed eventual type bounds, as long as the final value is within bounds.
/// Any folds that require type information are deferred.
struct UntypedConstantFolder : public AST::ExpressionVisitorT<UntypedConstantFolder, AST::Literal *NULLABLE> {
    AST::Literal *NULLABLE foldExpression(AST::Expression& expression) {
        return expression.acceptVisitor(*this);
    }

    AST::Literal *NULLABLE visitUnaryExpression(AST::UnaryExpression& unary) {
        if (auto *target = foldExpression(unary.getTarget())) {
            unary.setTarget(target);
            return foldLiteralUnary(unary, *target);
        } else {
            return nullptr;
        }
    }

    AST::Literal *NULLABLE visitBinaryExpression(AST::BinaryExpression& binary) {
        auto *foldedLeft = foldExpression(binary.getLeft());
        auto *foldedRight = foldExpression(binary.getRight());

        if (foldedLeft) {
            binary.setLeft(foldedLeft);
        }
        if (foldedRight) {
            binary.setRight(foldedRight);
        }

        if (foldedLeft && foldedRight) {
            return foldLiteralsBinary(binary, *foldedLeft, *foldedRight);
        } else {
            return nullptr;
        }
    }

    AST::Literal *NULLABLE visitIntrinsicExpression(AST::IntrinsicExpression& intrinsic) {
        auto& arguments = intrinsic.getArguments();

        for (int i = 0; i < arguments.size(); ++i) {
            auto& argument = *arguments[i];
            if (auto *foldedArgument = foldExpression(argument)) {
                arguments[i] = &argument;
            }
        }
        return nullptr;
    }

    AST::Literal *NULLABLE visitCallExpression(AST::CallExpression& call) {
        for (int i = 0; i < call.argumentCount(); ++i) {
            auto& argument = call.getArgument(i);
            if (auto *foldedArgument = foldExpression(argument)) {
                call.setArgument(i, foldedArgument);
            }
        }
        return nullptr;
    }

    AST::Literal *NULLABLE visitSubscriptExpression(AST::SubscriptExpression& subscript) {
        auto& index = subscript.getIndex();
        if (auto *foldedIndex = foldExpression(index)) {
            subscript.setIndex(foldedIndex);
        }
        return nullptr;
    }

    AST::Literal *NULLABLE visitInitializerExpression(AST::InitializerExpression& initializer) {
        for (int pi = 0; pi < initializer.getNumberOfPairs(); ++pi) {
            auto& pair = initializer.getPair(pi);

            if (auto *foldedValue = foldExpression(*pair.second)) {
                pair.second = foldedValue;
            }
        }
        return nullptr;
    }

    AST::Literal *NULLABLE visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess) {
        return nullptr;
    }

    AST::Literal *NULLABLE visitInferredMemberAccessExpression(AST::InferredMemberAccessExpression& inferredMemberAccess) {
        return nullptr;
    }

    AST::Literal *NULLABLE visitLiteral(AST::Literal& literal) {
        return &literal;
    }

    AST::Literal *NULLABLE visitIdentifier(AST::Identifier& identifier) {
        return nullptr;
    }
};

AST::Expression *NULLABLE foldConstantsUntyped(AST::Expression& expression) {
    return UntypedConstantFolder().foldExpression(expression);
}
