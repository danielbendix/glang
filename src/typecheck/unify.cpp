#include "unify.h"
#include "AST_Visitor.h"
#include "type/visitor.h"
#include "diagnostic.h"

using enum PassResultKind;

using llvm::dyn_cast;
using llvm::dyn_cast_if_present;

template <typename T, typename Visitor>
T visitTypePair(Type& left, Type& right, Visitor visitor) {
    return visit(left, [&](auto& left) -> T {
        return visit(right, [&](auto& right) -> T {
            return visitor(left, right);
        });
    });
}

template <typename Visitor>
AST::Literal *NULLABLE visitLiteralPair(AST::BinaryExpression& binary, AST::Literal& left, AST::Literal& right, Visitor&& visitor) {
    return AST::visitLiteral(left, [&](auto& left) -> AST::Literal * {
        return AST::visitLiteral(right, [&](auto& right) -> AST::Literal * {
            if constexpr (std::is_invocable_v<Visitor, AST::BinaryExpression&, decltype(left), decltype(right)>) {
                return visitor(binary, left, right);
            } else {
                return nullptr;
            }
        });
    });
}


AST::Literal *NULLABLE foldIntegers(AST::BinaryExpression& binary, AST::IntegerLiteral& left, AST::IntegerLiteral& right) {
    AST::Literal *value;

    switch (binary.getOp()) {
        case AST::BinaryOperator::OpenRange:
            break;
        case AST::BinaryOperator::ClosedRange:
            break;
        case AST::BinaryOperator::Add:
            value = left.coalesce(right, [](auto& left, auto& right) { left += right; });
            break;
        case AST::BinaryOperator::Subtract:
            break;
        case AST::BinaryOperator::Multiply:
            break;
        case AST::BinaryOperator::Divide:
            break;
        case AST::BinaryOperator::Modulo:
            break;
        case AST::BinaryOperator::ShiftLeft:
            break;
        case AST::BinaryOperator::ShiftRight:
            break;
        case AST::BinaryOperator::BitwiseAnd:
            break;
        case AST::BinaryOperator::BitwiseOr:
            break;
        case AST::BinaryOperator::BitwiseXor:
            break;
        case AST::BinaryOperator::Equal:
            break;
        case AST::BinaryOperator::NotEqual:
            break;
        case AST::BinaryOperator::Less:
            break;
        case AST::BinaryOperator::LessEqual:
            break;
        case AST::BinaryOperator::Greater:
            break;
        case AST::BinaryOperator::GreaterEqual:
            break;
        case AST::BinaryOperator::LogicalAnd:
            break;
        case AST::BinaryOperator::LogicalOr:
            break;
    }

    if (value) {
        right.~IntegerLiteral();
    }

    return value;
}

AST::Literal *NULLABLE foldBooleans(AST::BinaryExpression& binary, AST::BooleanLiteral& left, AST::BooleanLiteral& right) {
    std::optional<bool> value{};
    switch (binary.getOp()) {
        case AST::BinaryOperator::OpenRange:
            break;
        case AST::BinaryOperator::ClosedRange:
            break;
        case AST::BinaryOperator::Add:
        case AST::BinaryOperator::Subtract:
        case AST::BinaryOperator::Multiply:
        case AST::BinaryOperator::Divide:
        case AST::BinaryOperator::Modulo:
            break;
        case AST::BinaryOperator::ShiftLeft:
        case AST::BinaryOperator::ShiftRight:
        case AST::BinaryOperator::BitwiseAnd:
        case AST::BinaryOperator::BitwiseOr:
        case AST::BinaryOperator::BitwiseXor:
            break;

        case AST::BinaryOperator::Equal: value = left.getValue() == right.getValue(); break;
        case AST::BinaryOperator::NotEqual: value = left.getValue() != right.getValue(); break;
        case AST::BinaryOperator::Less: value = left.getValue() < right.getValue(); break;
        case AST::BinaryOperator::LessEqual: value = left.getValue() <= right.getValue(); break;
        case AST::BinaryOperator::Greater: value = left.getValue() > right.getValue(); break;
        case AST::BinaryOperator::GreaterEqual: value = left.getValue() >= right.getValue(); break;

        case AST::BinaryOperator::LogicalAnd: value = left.getValue() && right.getValue(); break;
        case AST::BinaryOperator::LogicalOr: value = left.getValue() || right.getValue(); break;
    }

    if (value) {
        assert(false && "Set value on left.");
        return &left;
    } else {
        return nullptr;
    }
}

AST::Literal *NULLABLE fold(AST::BinaryExpression& binary, AST::Literal& left, AST::Literal& right) {
    using namespace AST;
    return visitLiteralPair(binary, left, right, overloaded {
        [](BinaryExpression& binary, IntegerLiteral& left, IntegerLiteral& right) {
            return foldIntegers(binary, left, right);
        },
        [](BinaryExpression& binary, BooleanLiteral& left, BooleanLiteral& right) {
            return foldBooleans(binary, left, right);
        }
    });
}

AST::Literal *NULLABLE add(AST::Literal& left, AST::Literal& right) {
    using namespace AST;
    visitLiteral(left, overloaded {
        [&](IntegerLiteral& leftInteger) {
            

        },
        [&](FloatingPointLiteral& leftFloating) {
            
        },
        [&](Literal& otherLeft) {

        }
    });

    return nullptr;
}

std::pair<Result, AST::Literal *NULLABLE> unifyLiterals(AST::BinaryExpression& binary, AST::Literal& left, AST::Literal& right) {
    switch (binary.getOp()) {

    case AST::BinaryOperator::OpenRange:
        break;
    case AST::BinaryOperator::ClosedRange:
        break;
    case AST::BinaryOperator::Add:
        break;
    case AST::BinaryOperator::Subtract:
        break;
    case AST::BinaryOperator::Multiply:
        break;
    case AST::BinaryOperator::Divide:
        break;
    case AST::BinaryOperator::Modulo:
        break;
    case AST::BinaryOperator::ShiftLeft:
        break;
    case AST::BinaryOperator::ShiftRight:
        break;
    case AST::BinaryOperator::BitwiseAnd:
        break;
    case AST::BinaryOperator::BitwiseOr:
        break;
    case AST::BinaryOperator::BitwiseXor:
        break;
    case AST::BinaryOperator::Equal:
        break;
    case AST::BinaryOperator::NotEqual:
        break;
    case AST::BinaryOperator::Less:
        break;
    case AST::BinaryOperator::LessEqual:
        break;
    case AST::BinaryOperator::Greater:
        break;
    case AST::BinaryOperator::GreaterEqual:
        break;
    case AST::BinaryOperator::LogicalAnd:
        break;
    case AST::BinaryOperator::LogicalOr:
        break;
    }

    return {OK, nullptr};
}

std::pair<Result, AST::Expression *NULLABLE> unifyTypesForBitwiseArithmetic(Type& left, Type& right) {
    return {ERROR, nullptr};
}

Type *unifyFPTypes(AST::BinaryExpression& binary, FPType& left, FPType& right) {
    using enum AST::UnaryOperator;
    auto& allocator = ThreadContext::get()->nodeAllocator;
    if (left.precision < right.precision) {
        auto wrappedLeft = AST::UnaryExpression::wrap(allocator, binary.getLeft(), FPExtend, right);
        binary.setWrappedLeft(wrappedLeft);
        return &right;
    } else {
        auto wrappedRight = AST::UnaryExpression::wrap(allocator, binary.getRight(), FPExtend, left);
        binary.setWrappedRight(wrappedRight);
        return &left;
    }
}

Type *unifyIntegerTypes(AST::BinaryExpression& binary, IntegerType& left, IntegerType& right) {
    using enum AST::UnaryOperator;
    if (left.isSigned == right.isSigned) {
        auto& allocator = ThreadContext::get()->nodeAllocator;
        auto op = left.isSigned ? SignExtend : ZeroExtend;
        if (left.bitWidth < right.bitWidth) {
            auto wrappedLeft = AST::UnaryExpression::wrap(allocator, binary.getLeft(), op, right);
            binary.setWrappedLeft(wrappedLeft);
            return &right;
        } else {
            auto wrappedRight = AST::UnaryExpression::wrap(allocator, binary.getRight(), op, left);
            binary.setWrappedRight(wrappedRight);
            return &left;
        }
    } else { // With mixed signedness, we only allow coercing unsigned to a larger signed type.
        auto& allocator = ThreadContext::get()->nodeAllocator;
        if (left.isSigned && left.bitWidth > right.bitWidth) {
            auto wrappedRight = AST::UnaryExpression::wrap(allocator, binary.getRight(), ZeroExtend, left);
            binary.setWrappedRight(wrappedRight);
            return &left;
        } else if (right.isSigned && right.bitWidth > left.bitWidth) {
            auto wrappedLeft = AST::UnaryExpression::wrap(allocator, binary.getLeft(), ZeroExtend, right);
            binary.setWrappedLeft(wrappedLeft);
            return &right;
        }

        // TODO: Better diagnostic.
        Diagnostic::error(binary, "Cannot unify types " + left.makeName() + " and " + right.makeName() + ".");
        return nullptr;
    }
}

Type *unifyTypesForComparison(AST::BinaryExpression& binary, Type& left, Type& right) {
    return visitTypePair<Type *>(left, right, overloaded {
        [&](FPType& left, FPType& right) -> Type * {
            return unifyFPTypes(binary, left, right);
        },
        [&](IntegerType& left, IntegerType& right) -> Type * {
            return unifyIntegerTypes(binary, left, right);
        },
        // Potentially add unification for FP and integer type.
        [&](auto& left, auto& right) -> Type * {
            Diagnostic::error(binary, "Cannot unify types " + left.makeName() + " and " + right.makeName() + ".");
            return nullptr;
        }
    });
}

Type *unifyTypesForEquality(AST::BinaryExpression& binary, Type& left, Type& right) {
    return visitTypePair<Type *>(left, right, overloaded {
        [&](FPType& left, FPType& right) -> Type * {
            return unifyFPTypes(binary, left, right);
        },
        [&](IntegerType& left, IntegerType& right) -> Type * {
            return unifyIntegerTypes(binary, left, right);
        },
        // Potentially unification of uneven optionals.
        [&](auto& left, auto& right) -> Type * {
            Diagnostic::error(binary, "Cannot unify types " + left.makeName() + " and " + right.makeName() + ".");
            return nullptr;
        },
    });
}

Type *unifyTypesForArithmetic(AST::BinaryExpression& binary, Type& left, Type& right) {
    return visitTypePair<Type *>(left, right, overloaded {
        [&](FPType& left, FPType& right) -> Type * {
            return unifyFPTypes(binary, left, right);
        },
        [&](IntegerType& left, IntegerType& right) -> Type * {
            return unifyIntegerTypes(binary, left, right);
        },
        // Potentially add unification for FP and integer type.
        [&](auto& left, auto& right) -> Type * {
            Diagnostic::error(binary, "Cannot unify types " + left.makeName() + " and " + right.makeName() + ".");
            return nullptr;
        }
    });
}

Type *unifyTypesForBitwiseArithmetic(AST::BinaryExpression& binary, Type& left, Type& right, Type *propagatedType) {
    using enum AST::UnaryOperator;
    return visitTypePair<Type *>(left, right, overloaded {
        [&](IntegerType& left, IntegerType& right) -> Type * {
            using enum AST::UnaryOperator;
            if (left.isSigned == right.isSigned) {
                auto& allocator = ThreadContext::get()->nodeAllocator;
                auto op = left.isSigned ? SignExtend : ZeroExtend;
                if (left.bitWidth < right.bitWidth) {
                    auto wrappedLeft = AST::UnaryExpression::wrap(allocator, binary.getLeft(), op, right);
                    binary.setWrappedLeft(wrappedLeft);
                    return &right;
                } else {
                    auto wrappedRight = AST::UnaryExpression::wrap(allocator, binary.getRight(), op, left);
                    binary.setWrappedRight(wrappedRight);
                    return &left;
                }
            } else { // With mixed signedness, the final type is ambigious, unless we have a propagated type as a tiebreaker.
                if (auto propagatedIntegerType = dyn_cast_if_present<IntegerType>(propagatedType)) {
                    if (left.bitWidth == right.bitWidth) {
                        if (left.isSigned == propagatedIntegerType->isSigned) {
                            return &left;
                        } else {
                            return &right;
                        }
                    }
                }
                // TODO: Better diagnostic.
                Diagnostic::error(binary, "Cannot unify types " + left.makeName() + " and " + right.makeName() + ".");
                return nullptr;
            }
        },
        [&](auto& left, auto& right) -> Type * {
            Diagnostic::error(binary, "Cannot unify types " + left.makeName() + " and " + right.makeName() + ".");
            return nullptr;
        }
    });

    assert(false);
    return nullptr;


}
