#include "typecheck/unify.h"
#include "diagnostic.h"

#include <llvm/Support/Casting.h>

using Result = PassResult;
using enum PassResultKind;

using llvm::cast, llvm::dyn_cast;

using enum AST::UnaryOperator;

// There is an overarching problem here in that we're not using the "value transfer" node,
// but the value node for diagnostics.

std::pair<Result, unique_ptr_t<AST::Expression>> unifyIntegerTypes(
    IntegerType& destination, 
    IntegerType& source,
    AST::Expression& expression
) {
    if (destination.isSigned) {
        // This assumes that two signed types will never have the same bit width.
        if (source.bitWidth < destination.bitWidth) {
            unique_ptr_t<AST::UnaryExpression> wrap;
            if (source.isSigned) {
                wrap = AST::UnaryExpression::wrap(expression, SignExtend, destination);
            } else {
                wrap = AST::UnaryExpression::wrap(expression, ZeroExtend, destination);
            }
            return {OK, std::move(wrap)};
        } else {
            Diagnostic::error(expression, "Cannot coerce [SOURCE TYPE] to [DESTINATION TYPE], as this could result in a loss of information.");
            return {ERROR, nullptr};
        }
    } else {
        if (source.isSigned) {
            Diagnostic::error(expression, "Cannot coerce signed value to unsigned destination. Use truncation or a conditional cast.");
            return {ERROR, nullptr};
        } else if (source.bitWidth < destination.bitWidth) {
            return {OK, AST::UnaryExpression::wrap(expression, ZeroExtend, destination)};
        } else {
            Diagnostic::error(expression, "Cannot coerce [SOURCE TYPE] to [DESTINATION TYPE], as this could result in a loss of information.");
            return {ERROR, nullptr};
        }
    }
}

std::pair<Result, unique_ptr_t<AST::Expression>> unifyFloatingTypes(
    FloatingType& destination, 
    FloatingType& source,
    AST::Expression& expression
) {
    if (destination.precision > source.precision) {
        return {OK, AST::UnaryExpression::wrap(expression, FPExtend, destination)};
    } else {
        Diagnostic::error(expression, "Coercing [SOURCE TYPE] to [DESTINATION TYPE] could result in a loss of information.");
        return {ERROR, nullptr};
    }
}

std::pair<Result, unique_ptr_t<AST::Expression>> unifyIntegerToFP(
    FloatingType& destination, 
    IntegerType& source,
    AST::Expression& expression
) {
    auto fractionBits = destination.fractionBits();
    if (fractionBits > source.bitWidth) {
        return {OK, AST::UnaryExpression::wrap(expression, FPExtend, destination)};
    } else {
        Diagnostic::error(expression, "Coercing [SOURCE TYPE] to [DESTINATION TYPE] could result in a loss of information.");
        return {ERROR, nullptr};
    }
}

std::pair<Result, unique_ptr_t<AST::Expression>> unwrappingOptionals(OptionalType& destination, Type& source, AST::Expression& expression) {
    std::pair<Result, unique_ptr_t<AST::Expression>> result = {ERROR, nullptr};
    if (auto optionalType = dyn_cast<OptionalType>(destination.getContained())) {
        result = unwrappingOptionals(*optionalType, source, expression);
    } else {
        result = unifyTypes(*destination.getContained(), source, expression);
    }
    if (result.second) {
        return {result.first, AST::UnaryExpression::wrap(*result.second.release(), OptionalWrap, destination)};
    } else {
        return {result.first, nullptr};
    }
}

std::pair<Result, unique_ptr_t<AST::Expression>> unifyTypes(Type& destination, Type& source, AST::Expression& expression) noexcept {
    if (&source == &destination) {
        return {OK, nullptr};
    }

    switch (destination.getKind()) {

    case TK_Void:
        assert(false);
    case TK_Boolean:
        assert(false);
        return {ERROR, nullptr};
    case TK_Num_Integer: {
        auto& integerDestination = cast<IntegerType>(destination);
        if (auto integerSource = dyn_cast<IntegerType>(&source)) {
            return unifyIntegerTypes(integerDestination, *integerSource, expression);
        } else if (auto floatingSource = dyn_cast<FloatingType>(&source)) {

        }

        // TODO: Check if we can widen the types.
        assert(false && "TODO");
    }
    case TK_Num_Floating: {
        auto& fpDestination = cast<FloatingType>(destination);
        if (auto fpSource = dyn_cast<FloatingType>(&source)) {
            return unifyFloatingTypes(fpDestination, *fpSource, expression);
        } else if (auto integerSource = dyn_cast<IntegerType>(&source)) {
            return unifyIntegerToFP(fpDestination, *integerSource, expression);
        }
        Diagnostic::error(expression, "Cannot coerce [SOURCE TYPE] to floating point type.");
        return {ERROR, nullptr};
        // Allow:
        // f64 <- u/i32 & smaller
        // f32 <- u/i16 & smaller
        assert(false && "TODO");
    }
    case TK_String:
        assert(false);
    case TK_Pointer:
        return {ERROR, nullptr}; 
    case TK_Optional: {
        auto& optionalDestination = cast<OptionalType>(destination);
        if (auto optionalSource = dyn_cast<OptionalType>(&source)) {
            // TODO: We should find out if this would be a "good cast" without the optionals.
            Diagnostic::error(expression, "Cannot cast between different optional types.");
            return {ERROR, nullptr};
        } else if (optionalDestination.getContained() == &source) {
            auto wrap = AST::UnaryExpression::wrap(expression, AST::UnaryOperator::OptionalWrap, destination);
            return {OK, std::move(wrap)};
        } else if (auto integerSource = dyn_cast<IntegerType>(&source)) {
            return unwrappingOptionals(optionalDestination, source, expression);
        } else if (auto floatingSource = dyn_cast<FloatingType>(&source)) {
            return unwrappingOptionals(optionalDestination, source, expression);
        } else {
            return unwrappingOptionals(optionalDestination, source, expression);
        }
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
}
