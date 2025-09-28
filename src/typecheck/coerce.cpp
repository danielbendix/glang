#include "typecheck/coerce.h"
#include "diagnostic.h"

#include <llvm/Support/Casting.h>

using Result = PassResult;
using enum PassResultKind;

using llvm::cast, llvm::dyn_cast;

using enum AST::UnaryOperator;

// There is an overarching problem here in that we're not using the "value transfer" node,
// but the value node for diagnostics.

std::pair<Result, AST::Expression *> coerceBetweenIntegerTypes(
    IntegerType& destination, 
    IntegerType& source,
    AST::Expression& expression
) {
    if (destination.isSigned) {
        if (source.bitWidth < destination.bitWidth) {
            AST::UnaryExpression *wrap;
            if (source.isSigned) {
                wrap = AST::UnaryExpression::wrap(nodeAllocator(), expression, SignExtend, destination);
            } else {
                wrap = AST::UnaryExpression::wrap(nodeAllocator(), expression, ZeroExtend, destination);
            }
            return {OK, wrap};
        } else if (source.bitWidth == destination.bitWidth) {
            // This is a noop in LLVM.
            return {OK, &expression};
        } else {
            Diagnostic::error(expression, "Cannot coerce " + source.makeName() + " to " + destination.makeName() + ", as this could result in a loss of information.");
            return {ERROR, nullptr};
        }
    } else {
        if (source.isSigned) {
            Diagnostic::error(expression, "Cannot coerce signed value to unsigned destination. Use truncation or a conditional cast.");
            return {ERROR, nullptr};
        } else if (source.bitWidth < destination.bitWidth) {
            return {OK, AST::UnaryExpression::wrap(nodeAllocator(), expression, ZeroExtend, destination)};
        } else if (source.bitWidth == destination.bitWidth) {
            // This is a noop in LLVM.
            return {OK, &expression};
        } else {
            Diagnostic::error(expression, "Cannot coerce " + source.makeName() + " to " + destination.makeName() + ", as this could result in a loss of information.");
            return {ERROR, nullptr};
        }
    }
}

std::pair<Result, AST::Expression *> coerceBetweenFPTypes(
    FPType& destination, 
    FPType& source,
    AST::Expression& expression
) {
    if (destination.precision > source.precision) {
        return {OK, AST::UnaryExpression::wrap(nodeAllocator(), expression, FPExtend, destination)};
    } else {
        Diagnostic::error(expression, "Coercing " + source.makeName() + " to " + destination.makeName() + " could result in a loss of information.");
        return {ERROR, nullptr};
    }
}

std::pair<Result, AST::Expression *> coerceIntegerToFP(
    FPType& destination, 
    IntegerType& source,
    AST::Expression& expression
) {
    auto fractionBits = destination.fractionBits();
    if (fractionBits > source.bitWidth) {
        return {OK, AST::UnaryExpression::wrap(nodeAllocator(), expression, IntegerToFP, destination)};
    } else {
        Diagnostic::error(expression, "Coercing " + source.makeName() + " to " + destination.makeName() + " could result in a loss of information.");
        return {ERROR, nullptr};
    }
}

std::pair<Result, AST::Expression *> unwrappingOptionals(OptionalType& destination, Type& source, AST::Expression& expression) {
    std::pair<Result, AST::Expression *> result = {ERROR, nullptr};
    if (auto optionalType = dyn_cast<OptionalType>(destination.getContained())) {
        result = unwrappingOptionals(*optionalType, source, expression);
    } else {
        result = coerceType(*destination.getContained(), source, expression);
    }
    if (result.second) {
        return {result.first, AST::UnaryExpression::wrap(nodeAllocator(), *result.second, OptionalWrap, destination)};
    } else {
        return {result.first, AST::UnaryExpression::wrap(nodeAllocator(), expression, OptionalWrap, destination)};
    }
}

std::pair<Result, AST::Expression *> coerceType(Type& destination, Type& source, AST::Expression& expression) {
    if (&source == &destination) {
        return {OK, nullptr};
    }

    switch (destination.getKind()) {
    case TK_Void:
        Diagnostic::error(expression, "Cannot coerce " + source.makeName() + " to " + destination.makeName() + ". This may be a compiler programmer error.");
        return {ERROR, nullptr};
    case TK_Boolean:
        // TODO: Better diagnostic based on source type.
        Diagnostic::error(expression, "Cannot coerce to " + source.makeName() + ". Use a predicate to get a boolean.");
        return {ERROR, nullptr};
    case TK_Num_Integer: {
        auto& integerDestination = cast<IntegerType>(destination);
        if (auto integerSource = dyn_cast<IntegerType>(&source)) {
            return coerceBetweenIntegerTypes(integerDestination, *integerSource, expression);
        } else if (auto floatingSource = dyn_cast<FPType>(&source)) {
            Diagnostic::error(expression, "Cannot coerce floating-point type " + source.makeName() + " to integer type " + destination.makeName() + ", as this would discard the fractional component. TODO: Add note about intrinsics to use.");
            return {ERROR, nullptr};
        }
        Diagnostic::error(expression, "Cannot coerce " + source.makeName() + " to integer type.");
        return {ERROR, nullptr};
    }
    case TK_Num_FP: {
        auto& fpDestination = cast<FPType>(destination);
        if (auto fpSource = dyn_cast<FPType>(&source)) {
            return coerceBetweenFPTypes(fpDestination, *fpSource, expression);
        } else if (auto integerSource = dyn_cast<IntegerType>(&source)) {
            return coerceIntegerToFP(fpDestination, *integerSource, expression);
        }
        Diagnostic::error(expression, "Cannot coerce " + source.makeName() + " to floating point type.");
        return {ERROR, nullptr};
        // Allow:
        // f64 <- u/i32 & smaller
        // f32 <- u/i16 & smaller
        assert(false && "TODO");
    }
    case TK_String:
        assert(false);
    case TK_Pointer:
        Diagnostic::error(expression, "Cannot coerce " + source.makeName() +  " to " + destination.makeName() + ".");
        // TODO: We need type names.
        return {ERROR, nullptr}; 
    case TK_Optional: {
        auto& optionalDestination = cast<OptionalType>(destination);
        if (auto optionalSource = dyn_cast<OptionalType>(&source)) {
            // TODO: We should find out if this would be a "good cast" without the optionals.
            Diagnostic::error(expression, "Cannot cast between different optional types.");
            return {ERROR, nullptr};
        } else if (optionalDestination.getContained() == &source) {
            auto wrap = AST::UnaryExpression::wrap(nodeAllocator(), expression, AST::UnaryOperator::OptionalWrap, destination);
            return {OK, std::move(wrap)};
        } else if (auto integerSource = dyn_cast<IntegerType>(&source)) {
            return unwrappingOptionals(optionalDestination, source, expression);
        } else if (auto floatingSource = dyn_cast<FPType>(&source)) {
            return unwrappingOptionals(optionalDestination, source, expression);
        } else {
            return unwrappingOptionals(optionalDestination, source, expression);
        }
        llvm_unreachable("TODO: Solve uneven optional type coercion.");
        // TODO: Solve uneven optional type coercion
        break;
    }
    case TK_Function:
        assert(false);
    case TK_Struct:
        return {ERROR, nullptr};
    case TK_Enum:
        return {ERROR, nullptr};
    case TK_Protocol:
        assert(false);
    }


    Diagnostic::error(expression, "Cannot coerce " + source.makeName() + " to " + destination.makeName());
    return {ERROR, nullptr};
}

std::pair<Result, AST::Expression *> coerceCompoundAssignmentOperand(Type& to, Type& from, AST::BinaryOperator op, AST::Expression& rhs) {
    switch (op) {
    case AST::BinaryOperator::OpenRange:
    case AST::BinaryOperator::ClosedRange:
    case AST::BinaryOperator::Equal:
    case AST::BinaryOperator::NotEqual:
    case AST::BinaryOperator::Less:
    case AST::BinaryOperator::LessEqual:
    case AST::BinaryOperator::Greater:
    case AST::BinaryOperator::GreaterEqual:
    case AST::BinaryOperator::LogicalAnd:
    case AST::BinaryOperator::LogicalOr:
        llvm_unreachable("Unsupported binary operator for compound assignment.");
    case AST::BinaryOperator::Add:
    case AST::BinaryOperator::Subtract:
    case AST::BinaryOperator::Multiply:
    case AST::BinaryOperator::Divide:
    case AST::BinaryOperator::Modulo:
        return coerceType(to, from, rhs);
    case AST::BinaryOperator::ShiftLeft:
    case AST::BinaryOperator::ShiftRight:
        Diagnostic::error(rhs, "TODO: Implement type checking for shift operations.");
        return {ERROR, nullptr};
    case AST::BinaryOperator::BitwiseAnd:
    case AST::BinaryOperator::BitwiseOr:
    case AST::BinaryOperator::BitwiseXor:
        return coerceType(to, from, rhs);
    }
}
