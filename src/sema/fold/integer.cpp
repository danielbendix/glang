#include "sema/fold/integer.h"

#include <algorithm>

// This treats all literals as signed literals.

using Operand = AST::IntegerLiteral::Value;
using enum AST::IntegerLiteral::Type;

void equalizeForBitwise(Operand& left, Operand& right) {
    auto lsb = left.getSignificantBits();
    auto rsb = right.getSignificantBits();

    auto bits= std::max(lsb, rsb);

    left.signExtendInPlace(bits);
    right.signExtendInPlace(bits);
}

void equalizeForAddSub(Operand& left, Operand& right) {
    auto lsb = left.getSignificantBits();
    auto rsb = right.getSignificantBits();

    auto bits= std::max(lsb, rsb);

    left.signExtendInPlace(bits);
    right.signExtendInPlace(bits);
}

void equalizeForMul(Operand& left, Operand& right) {
    auto lsb = left.getSignificantBits();
    auto rsb = right.getSignificantBits();

    auto bits = std::max(lsb, rsb) * 2 + 1;

    left.signExtendInPlace(bits);
    right.signExtendInPlace(bits);
}

void equalizeForDiv(Operand& left, Operand& right) {
    auto lsb = left.getSignificantBits();
    auto rsb = right.getSignificantBits();

    auto bits = std::max(lsb, rsb);

    left.signExtendInPlace(bits);
    right.signExtendInPlace(bits);
}

void equalizeForMod(Operand& left, Operand& right) {
    auto lsb = left.getSignificantBits();
    auto rsb = right.getSignificantBits();

    auto bits= std::max(lsb, rsb);

    left.signExtendInPlace(bits);
    right.signExtendInPlace(bits);
}


bool IntegerFold::bitwiseNegate(Operand& value, IntegerType *as) {
    if (!as) {
        return false;
    }
    value.signExtendOrTruncateInPlace(as->bitWidth);
    value.flipAllBits();
    return true;
}

bool IntegerFold::negate(Operand& value, IntegerType *as) {
    auto bits = value.getBitWidth();
    auto significantBits = value.getSignificantBits();
    if (value.isNegative() && bits == significantBits) {
        value.signExtendInPlace(bits + 1);
    }
    value.negate();
    return true;
}

// Binary operators.
bool IntegerFold::bitwiseOr(Operand& left, Operand& right, IntegerType *NULLABLE as) {
    equalizeForBitwise(left, right);
    left |= right;
    return true;
}

bool IntegerFold::bitwiseAnd(Operand& left, Operand& right, IntegerType *NULLABLE as) {
    equalizeForBitwise(left, right);
    left &= right;
    return true;
}

bool IntegerFold::bitwiseXor(Operand& left, Operand& right, IntegerType *NULLABLE as) {
    equalizeForBitwise(left, right);
    left ^= right;
    return true;
}

bool IntegerFold::shiftLeft(Operand& left, Operand& right, IntegerType *NULLABLE as) {
    // We need to define the exact semantics of left shift, and if it can occur without knowing the bit width.
    return false;
}

bool IntegerFold::shiftRight(Operand& left, Operand& right, IntegerType *NULLABLE as) {
    // We need to define the exact semantics of right shift, and if it can occur without knowing the bit width.
    return false;
}

bool IntegerFold::add(Operand& left, Operand& right, IntegerType *NULLABLE as) {
    equalizeForAddSub(left, right);
    left += right;
    return true;
}

bool IntegerFold::subtract(Operand& left, Operand& right, IntegerType *NULLABLE as) {
    equalizeForAddSub(left, right);
    left -= right;
    return true;
}

bool IntegerFold::multiply(Operand& left, Operand& right, IntegerType *NULLABLE as) {
    equalizeForMul(left, right);
    left *= right;
    return true;
}

bool IntegerFold::divide(Operand& left, Operand& right, IntegerType *NULLABLE as) {
    equalizeForDiv(left, right);
    left.setValue(left.sdiv(right));
    return true;
}

bool IntegerFold::modulo(Operand& left, Operand& right, IntegerType *NULLABLE as) {
    return false;
    // TODO: Do modulo with srem
    equalizeForDiv(left, right);
    left.setValue(left.srem(right));
    return true;
}

bool compare(const Operand& left, const Operand& right, auto comparison) {
    auto leftBitWidth = left.getBitWidth();
    auto rightBitWidth = right.getBitWidth();
    if (leftBitWidth == rightBitWidth) {
        return comparison(left, right);
    } else if (leftBitWidth < rightBitWidth) {
        llvm::APInt sextLeft = left.sext(rightBitWidth);
        return comparison(sextLeft, right);
    } else {
        llvm::APInt sextRight = right.sext(leftBitWidth);
        return comparison(left, sextRight);
    }
}

bool IntegerFold::equal(const Operand& left, const Operand& right, IntegerType *NULLABLE as) {
    return compare(left, right, [] (auto& left, auto& right) { return left == right; });
}

bool IntegerFold::notEqual(const Operand& left, const Operand& right, IntegerType *NULLABLE as) {
    return compare(left, right, [] (auto& left, auto& right) { return left != right; });
}

bool IntegerFold::lessThan(const Operand& left, const Operand& right, IntegerType *NULLABLE as) {
    return compare(left, right, [] (auto& left, auto& right) { return left.slt(right); });
}

bool IntegerFold::lessThanOrEqualTo(const Operand& left, const Operand& right, IntegerType *NULLABLE as) {
    return compare(left, right, [] (auto& left, auto& right) { return left.sle(right); });
}

bool IntegerFold::greaterThan(const Operand& left, const Operand& right, IntegerType *NULLABLE as) {
    return compare(left, right, [] (auto& left, auto& right) { return left.sgt(right); });
}

bool IntegerFold::greaterThanOrEqualTo(const Operand& left, const Operand& right, IntegerType *NULLABLE as) {
    return compare(left, right, [] (auto& left, auto& right) { return left.sge(right); });
}

AST::IntegerLiteral *NULLABLE foldUnaryArithmetic(AST::Node& locationNode, AST::IntegerLiteral& operand, IntegerType *NULLABLE as, auto function) {
    if (function(operand.getValue(), as)) {
        // FIXME. have special location data on IntegerLiteral.
        //operand.getLocation() = locationNode.getLocation();
        return &operand;
    } else {
        return nullptr;
    }
}

AST::Literal *NULLABLE IntegerFold::unary(AST::UnaryExpression& unary, AST::IntegerLiteral& operand, IntegerType *NULLABLE as) {
    switch (unary.getOp()) {
        case AST::UnaryOperator::Negate:
            return foldUnaryArithmetic(unary, operand, as, negate);
        case AST::UnaryOperator::BitwiseNegate:
            return foldUnaryArithmetic(unary, operand, as, bitwiseNegate);
        case AST::UnaryOperator::Not:
        case AST::UnaryOperator::AddressOf:
        case AST::UnaryOperator::PrefixDereference:
        case AST::UnaryOperator::PostfixDereference:
        case AST::UnaryOperator::ForceUnwrap:
            return nullptr;
        case AST::UnaryOperator::ZeroExtend:
        case AST::UnaryOperator::SignExtend:
        case AST::UnaryOperator::IntegerToFP:
        case AST::UnaryOperator::FPExtend:
        case AST::UnaryOperator::OptionalWrap:
            llvm_unreachable("Synthetic unary operators in unary fold.");
    }
}

AST::IntegerLiteral *NULLABLE foldBinaryArithmetic(AST::Node& locationNode, AST::IntegerLiteral& left, AST::IntegerLiteral& right, IntegerType *NULLABLE as, auto function) {
    if (function(left.getValue(), right.getValue(), as)) {
        right.~IntegerLiteral();
        // FIXME: Have special location on IntegerLiteral.
        //left.getLocation() = locationNode.getLocation();
        // FIXME: Use location of binary operator for result node.
        return &left;
    } else {
        return nullptr;
    }
}

AST::BooleanLiteral *NONNULL foldComparison(AST::Node& locationNode, AST::IntegerLiteral& left, AST::IntegerLiteral& right, IntegerType *NULLABLE as, auto function) {
    auto comparison  = function(left.getValue(), right.getValue(), as);
    right.~IntegerLiteral();
    return AST::BooleanLiteral::createDestroyingOther(left, comparison);
}

AST::Literal *NULLABLE IntegerFold::binary(AST::BinaryExpression& binary, AST::IntegerLiteral& left, AST::IntegerLiteral& right, IntegerType *NULLABLE as) {
    switch (binary.getOp()) {
        case AST::BinaryOperator::OpenRange:
        case AST::BinaryOperator::ClosedRange:
            return nullptr;
        case AST::BinaryOperator::Add:
            return foldBinaryArithmetic(binary, left, right, as, add);
        case AST::BinaryOperator::Subtract:
            return foldBinaryArithmetic(binary, left, right, as, subtract);
        case AST::BinaryOperator::Multiply:
            return foldBinaryArithmetic(binary, left, right, as, multiply);
        case AST::BinaryOperator::Divide:
            return foldBinaryArithmetic(binary, left, right, as, divide);
        case AST::BinaryOperator::Modulo:
            return foldBinaryArithmetic(binary, left, right, as, modulo);
        case AST::BinaryOperator::ShiftLeft:
            return foldBinaryArithmetic(binary, left, right, as, shiftLeft);
        case AST::BinaryOperator::ShiftRight:
            return foldBinaryArithmetic(binary, left, right, as, shiftRight);
        case AST::BinaryOperator::BitwiseAnd:
            return foldBinaryArithmetic(binary, left, right, as, bitwiseAnd);
        case AST::BinaryOperator::BitwiseOr:
            return foldBinaryArithmetic(binary, left, right, as, bitwiseOr);
        case AST::BinaryOperator::BitwiseXor:
            return foldBinaryArithmetic(binary, left, right, as, bitwiseXor);
        case AST::BinaryOperator::Equal:
            return foldComparison(binary, left, right, as, equal);
        case AST::BinaryOperator::NotEqual:
            return foldComparison(binary, left, right, as, notEqual);
        case AST::BinaryOperator::Less:
            return foldComparison(binary, left, right, as, lessThan);
        case AST::BinaryOperator::LessEqual:
            return foldComparison(binary, left, right, as, lessThanOrEqualTo);
        case AST::BinaryOperator::Greater:
            return foldComparison(binary, left, right, as, greaterThan);
        case AST::BinaryOperator::GreaterEqual:
            return foldComparison(binary, left, right, as, greaterThanOrEqualTo);
        case AST::BinaryOperator::LogicalAnd:
        case AST::BinaryOperator::LogicalOr:
            return nullptr;
    }
}
