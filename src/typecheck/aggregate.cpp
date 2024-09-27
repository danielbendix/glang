#include "typecheck/aggregate.h"
#include "typecheck/resolver.h"
#include "typecheck/expression.h"
#include "typecheck/coerce.h"
#include "type/visitor.h"

#include "llvm/ADT/TypeSwitch.h"

#include <ranges>

using namespace TypeVisitor;

using Result = PassResult;
using enum PassResultKind;

using llvm::TypeSwitch;
using AggregateType = llvm::PointerUnion<StructType *NONNULL>;

std::pair<AggregateType, bool> asAggregate(Type *type) {
    using ReturnType = std::pair<AggregateType, bool>;
    return visit(*type, overloaded {
        [](StructType& structType) -> ReturnType {
            return {&structType, structType.typeChecked};
        },
        [](OptionalType& optionalType) -> ReturnType {
            return asAggregate(optionalType.getContained());
        },
        [](auto& type) -> ReturnType {
            return {nullptr, false};
        }
    });
}

std::string getAggregateName(AggregateType aggregateType) {
    return TypeSwitch<AggregateType, std::string>(aggregateType)
        .Case<StructType *>([](StructType *structType) {
            return structType->makeName();
        });
}

class AggregateTypeChecker {
    TypeResolver& typeResolver;
    AggregateType current = nullptr;

    llvm::SmallVector<AggregateType, 4> checkStack;
    llvm::SmallVector<AST::Node *NONNULL, 4> diagnosticLocationStack;

    void push(AggregateType aggregate) {
        if (current) {
            checkStack.push_back(current);
        }

        current = aggregate;
    }

    void pop() {
        if (checkStack.empty()) {
            current = nullptr;
        } else {
            current = checkStack.pop_back_val();
        }
    }

    void pushLocation(AST::Node& node) {
        diagnosticLocationStack.push_back(&node);
    }

    void popLocation() {
        diagnosticLocationStack.pop_back();
    }

public:
    AggregateTypeChecker(TypeResolver& typeResolver) : typeResolver{typeResolver} {}

    Result typeCheckStructField(AST::VariableDeclaration& field) {
        Type *declaredType = nullptr;
        if (auto *typeDeclaration = field.getTypeDeclaration()) {
            declaredType = typeResolver.resolveType(*typeDeclaration);
            if (!declaredType) {
                return ERROR;
            }
        }

        Type *type = nullptr;
        if (AST::Expression *initial = field.getInitialValue()) {
            ExpressionTypeChecker checker{typeResolver};
            if (declaredType) {
                type = checker.typeCheckExpression(*initial, declaredType);
            } else {
                TypeResult typeResult = checker.typeCheckExpression(*initial);
                if (typeResult && typeResult.isConstraint()) {
                    Type *defaultType = typeResolver.defaultTypeFromTypeConstraint(typeResult.constraint());
                    if (defaultType) {
                        type = checker.typeCheckExpression(*initial, defaultType);
                    }
                } else {
                    type = typeResult;
                }
            }

            if (!type) {
                return ERROR;
            }

            if (declaredType && type != declaredType) {
                auto [coerceResult, wrapped] = coerceType(*declaredType, *type, *initial);
                if (wrapped) {
                    field.setWrappedInitialValue(std::move(wrapped));
                }
                if (coerceResult.failed()) {
                    return coerceResult;
                }
                type = declaredType;
            }
        } else {
            type = declaredType;
        }

        TypeVisitor::visit(*type, overloaded {
            [&](auto&) {}

        });

        auto [aggregate, isTypeChecked] = asAggregate(type);
        if (aggregate && !isTypeChecked) {
            if (aggregate == current) {
                Diagnostic::error(field, "Struct type '" + getAggregateName(current) + "' cannot recursively contain itself.");
                return ERROR;
            } else {
                pushLocation(field);
                auto result = typeCheckAggregate(aggregate);
                popLocation();
                if (result.failed()) {
                    return result;
                }
            }
        }

        assert(type);

        auto& binding = llvm::cast<AST::IdentifierBinding>(field.getBinding());
        binding.setType(type);
        binding.setIsMutable(field.getIsMutable());
        field.setType(*type);

        return OK;
    }

    Result typeCheckStruct(StructType& structType) {
        Result result = OK;

        if (structType.typeChecked) {
            return OK;
        }

        push(&structType);

        Layout layout{0, 0};

        for (auto field : structType.getFields()) {
            result |= typeCheckStructField(*field);
            if (result.ok()) {
                auto [newLayout, offset] = incorporateLayoutAsField_C_ABI(layout, field->getType()->getLayout());
                layout = newLayout;
                // TODO: Set offset for field.
            }
        }

        layout = addPaddingToLayout_C_ABI(layout);
        structType.layout = layout;

        for (auto method : structType.getMethods()) {
            // TODO: Typecheck methods as well
        }

        pop();

        structType.typeChecked = true;

        return result;
    }

    Result typeCheckAggregate(AggregateType aggregateType) {
        if (auto it = std::ranges::find(checkStack, aggregateType); it != checkStack.end()) {
            auto index = it - checkStack.begin();

            std::string cycleDescription = "Cycle detected: ";
            
            for (const auto cycleMember : std::views::drop(checkStack, index)) {
                cycleDescription += getAggregateName(cycleMember) + " -> ";
            }

            cycleDescription += getAggregateName(current);

            Diagnostic::error(*diagnosticLocationStack[index], std::move(cycleDescription));
            for (const auto diagnosticLocation : std::views::drop(diagnosticLocationStack, 1 + index)) {
                Diagnostic::note(*diagnosticLocation, "Cycle through here.");
            }

            return ERROR;
        }

        return TypeSwitch<AggregateType, Result>(aggregateType)
            .Case<StructType *>([this](StructType *structType) {
                return typeCheckStruct(*structType);
            });
    }
};

Result typeCheckStructs(std::vector<StructType *NONNULL>& structTypes, TypeResolver& typeResolver) {
    AggregateTypeChecker checker{typeResolver};

    Result result = OK;

    for (const auto structType : structTypes) {
        result |= checker.typeCheckStruct(*structType);
    }

    return result;
}
