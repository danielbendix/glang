#ifndef LANG_sema_fold_integer_h
#define LANG_sema_fold_integer_h

#include "AST.h"

/**
 * Integer literals & folding:
 * 09/11/2024:
 * To keep integer literals simple for now, they will always store extra space
 * for a sign bit. This means that large unsigned literals will cause heap 
 * allocations that are not strictly necessary.
 * This can be improved in the future.
 */

class IntegerFold {
    using Operand = AST::IntegerLiteral::Value;
public:
    // Unary operators.
    static bool bitwiseNegate(Operand& value, IntegerType *NULLABLE as);
    static bool negate(Operand& value, IntegerType *NULLABLE as);

    // Binary operators.
    static bool bitwiseOr(Operand& left, Operand& right, IntegerType *NULLABLE as);
    static bool bitwiseAnd(Operand& left, Operand& right, IntegerType *NULLABLE as);
    static bool bitwiseXor(Operand& left, Operand& right, IntegerType *NULLABLE as);

    static bool shiftLeft(Operand& left, Operand& right, IntegerType *NULLABLE as);
    static bool shiftRight(Operand& left, Operand& right, IntegerType *NULLABLE as);

    static bool add(Operand& left, Operand& right, IntegerType *NULLABLE as);
    static bool subtract(Operand& left, Operand& right, IntegerType *NULLABLE as);
    static bool multiply(Operand& left, Operand& right, IntegerType *NULLABLE as);
    static bool divide(Operand& left, Operand& right, IntegerType *NULLABLE as);
    static bool modulo(Operand& left, Operand& right, IntegerType *NULLABLE as);

    static bool equal(const Operand& left, const Operand& right, IntegerType *NULLABLE as);
    static bool notEqual(const Operand& left, const Operand& right, IntegerType *NULLABLE as);
    static bool lessThan(const Operand& left, const Operand& right, IntegerType *NULLABLE as);
    static bool lessThanOrEqualTo(const Operand& left, const Operand& right, IntegerType *NULLABLE as);
    static bool greaterThan(const Operand& left, const Operand& right, IntegerType *NULLABLE as);
    static bool greaterThanOrEqualTo(const Operand& left, const Operand& right, IntegerType *NULLABLE as);

    /// Attempt to perform fold of unary operator with an integer literal..
    /// Either a fold occurred, and a pointer to the new node is returned.
    /// OR
    /// No fold occurred, a null pointer is returned, and the values are unmodified.
    static AST::Literal *NULLABLE unary(AST::UnaryExpression& unary, AST::IntegerLiteral& operand, IntegerType *NULLABLE as);

    /// Attempt to perform fold of binary operation between integer literals.
    /// Either a fold occurred, and a pointer to the new node is returned.
    /// OR
    /// No fold occurred, a null pointer is returned, and the values are unmodified.
    static AST::Literal *NULLABLE binary(AST::BinaryExpression& binary, AST::IntegerLiteral& left, AST::IntegerLiteral& right, IntegerType *NULLABLE as);
};

#endif // LANG_sema_fold_integer_h
