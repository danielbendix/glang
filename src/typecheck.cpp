#include "AST.h"
#include "AST_Visitor.h"
#include "builtins.h"
#include "diagnostic.h"
#include "typecheck.h"
#include "typecheck/coerce.h"
#include "typecheck/resolver.h"
#include "typecheck/expression.h"
#include "typecheck/internal.h"
#include "typecheck/assignment.h"
#include "typecheck/aggregate.h"
#include "typecheck/enum.h"

#include "type.h"
#include "type/struct.h"
#include "containers/bitmap.h"
#include "containers/pointer_map.h"

#include "llvm/ADT/TypeSwitch.h"

#include <ranges>

using enum PassResultKind;
using Result = PassResult;

using llvm::TypeSwitch;
using llvm::isa, llvm::cast;

class GlobalVariableTypeChecker {
    TypeResolver& typeResolver;
    PointerMap<AST::IdentifierBinding *, AST::VariableDeclaration *> ancestors;
    llvm::SmallVector<AST::VariableDeclaration *, 4> checkStack;
    ExpressionTypeChecker::GlobalHandler globalHandler;

    std::vector<AST::VariableDeclaration *> orderedGlobals;
public:
    GlobalVariableTypeChecker(TypeResolver& typeResolver) 
        : typeResolver{typeResolver}
    {
        auto globalHandlerLambda = [this](AST::IdentifierBinding& binding) -> Result {
            auto global = this->ancestors[&binding];
            return typeCheckGlobal(*global);
        };
        static_assert(sizeof(globalHandlerLambda) <= 8);
        globalHandler = globalHandlerLambda;
    }

    Result typeCheckGlobal(AST::VariableDeclaration& variable) {
        if (auto it = std::ranges::find(checkStack, &variable); it != checkStack.end()) {
            for (auto global : checkStack) {
                Diagnostic::error(*global, "Cycle detected in globals.");
            }
            return ERROR;
        }

        Type *declaredType = nullptr;
        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            declaredType = typeResolver.resolveType(*typeDeclaration);
            if (!declaredType) {
                return ERROR;
            }
        }

        Type *type = nullptr;
        if (AST::Expression *initial = variable.getInitialValue()) {
            checkStack.push_back(&variable);
            ExpressionTypeChecker typeChecker{typeResolver, globalHandler};
            type = typeChecker.typeCheckExpressionUsingDeclaredOrDefaultType(*initial, declaredType);
            checkStack.pop_back();
            variable.markAsChecked();
            orderedGlobals.push_back(&variable);

            if (!type) {
                return ERROR;
            }

            if (declaredType && type != declaredType) {
                auto [coerceResult, wrapped] = coerceType(*declaredType, *type, *initial);
                if (wrapped) {
                    variable.setWrappedInitialValue(std::move(wrapped));
                }
                if (coerceResult.failed()) {
                    return ERROR;
                }
                type = declaredType;
            }
        } else {
            Diagnostic::error(variable, "Global variable declaration has no initial value.");
            return ERROR;
        }

        if (type) {
            auto& binding = llvm::cast<AST::IdentifierBinding>(variable.getBinding());
            binding.setType(type);
            binding.setIsMutable(variable.getIsMutable());
            variable.setType(*type);
            return OK;
        } else {
            return ERROR;
        }
    }

    Result typeCheckGlobals(std::vector<AST::VariableDeclaration *>& globals) {
        // This is a bad hack for now. Typechecking should be soon rewritten 
        // into a single pass, which should handle name resolution, typechecking, 
        // and control flow together.
        // TODO: Reserve size in underlying map.
        for (auto global : globals) {
            auto& identifierBinding = cast<AST::IdentifierBinding>(global->getBinding());
            ancestors.insert(&identifierBinding, global);
        }

        Result result = OK;
        for (auto global : globals) {
            if (global->getIsChecked()) continue;
            result |= typeCheckGlobal(*global);
        }

        if (result.ok()) {
            assert(orderedGlobals.size() == globals.size());
            std::swap(globals, orderedGlobals);
        }

        return result;
    }
};

class GlobalDeclarationTypeChecker {
    ModuleDef& moduleDefinition;
    TypeResolver typeResolver;

    const PointerMap<AST::IdentifierBinding *, AST::VariableDeclaration *>& ancestors;
    llvm::SmallVector<AST::VariableDeclaration *, 4> checkStack;
    ExpressionTypeChecker::GlobalHandler globalHandler;

public:
    GlobalDeclarationTypeChecker(ModuleDef& moduleDefinition, const Builtins& builtins, PointerMap<AST::IdentifierBinding *, AST::VariableDeclaration *>& ancestors) 
        : moduleDefinition{moduleDefinition}
        , typeResolver{moduleDefinition, builtins}
        , ancestors{ancestors}
    {
        auto globalHandlerLambda = [this](AST::IdentifierBinding& binding) -> Result {
            auto global = this->ancestors[&binding];
            return typeCheckGlobal(*global);
        };
        static_assert(sizeof(globalHandlerLambda) <= 8);
        globalHandler = globalHandlerLambda;
    }

    Result typeCheckGlobals(std::vector<AST::VariableDeclaration *>& globals) {
        GlobalVariableTypeChecker typeChecker{typeResolver};
        return typeChecker.typeCheckGlobals(globals);
    }

    Result typeCheckGlobal(AST::VariableDeclaration& variable) {
        if (auto it = std::ranges::find(checkStack, &variable); it != checkStack.end()) {
            for (auto global : checkStack) {
                Diagnostic::error(*global, "Cycle detected in globals.");
            }
            return ERROR;
        }

        Type *declaredType = nullptr;
        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            declaredType = typeResolver.resolveType(*typeDeclaration);
            if (!declaredType) {
                return ERROR;
            }
        }

        Type *type = nullptr;
        if (AST::Expression *initial = variable.getInitialValue()) {
            checkStack.push_back(&variable);
            ExpressionTypeChecker typeChecker{typeResolver, globalHandler};
            type = typeChecker.typeCheckExpressionUsingDeclaredOrDefaultType(*initial, declaredType);
            checkStack.pop_back();

            if (!type) {
                return ERROR;
            }

            if (declaredType && type != declaredType) {
                auto [coerceResult, wrapped] = coerceType(*declaredType, *type, *initial);
                if (wrapped) {
                    variable.setWrappedInitialValue(std::move(wrapped));
                }
                if (coerceResult.failed()) {
                    return ERROR;
                }
                type = declaredType;
            }
        } else {
            Diagnostic::error(variable, "Global variable declaration has no initial value.");
            return ERROR;
        }

        if (type) {
            auto& binding = llvm::cast<AST::IdentifierBinding>(variable.getBinding());
            binding.setType(type);
            binding.setIsMutable(variable.getIsMutable());
            variable.setType(*type);
            return OK;
        } else {
            return ERROR;
        }
    }

    Result typeCheckFunction(AST::FunctionDeclaration& function) {
        Result result = OK;
        Type *returnType;
        if (auto returnTypeNode = function.getReturnTypeDeclaration()) {
            returnType = typeResolver.resolveType(*returnTypeNode);
            if (!returnType) {
                result = ERROR;
                // TODO: Emit error
            }
        } else {
            returnType = typeResolver.voidType();
        }

        std::vector<Type *> parameterTypes;
        parameterTypes.reserve(function.getParameterCount());

        for (int i = 0; i < function.getParameterCount(); ++i) {
            auto& parameter = function.getParameter(i);
            Type *type = typeResolver.resolveType(*parameter.typeDeclaration);
            if (!type) {
                result = ERROR;
                // TODO: Emit error
            }
            parameter.type = type;
            parameterTypes.push_back(type);
        }
        if (result.ok()) {
            auto& allocator = typeAllocator();
            // TODO: Either put the parameter types in with the types, or in a destructible heap.
            auto functionType = allocate(allocator, [&](void *space) {
                return new(space) FunctionType{returnType, std::move(parameterTypes)};
            });
            function.setType(*functionType);
        }
        return result;
    }
};

class FunctionTypeChecker : public AST::DeclarationVisitorT<FunctionTypeChecker, void>, public AST::StatementVisitorT<FunctionTypeChecker, void> {

    Result result = OK;
    AST::FunctionDeclaration *currentFunction;
    TypeResolver typeResolver;

    void visitBlock(const AST::Block& block) {
        for (int bi = 0; bi < block.size(); bi++) {
            block[bi].acceptVisitor(*this);
        }
    }

public:
    FunctionTypeChecker(ModuleDef& moduleDefinition, const Builtins& builtins) 
        : typeResolver{moduleDefinition, builtins} 
    {}

    Result typeCheckFunctionBody(AST::FunctionDeclaration& function) {
        result = OK;
        currentFunction = &function;
        for (auto& declaration : function.getCode()) {
            declaration.acceptVisitor(*this);
            // TODO: If a declaration fails, we end, but if a statement fails, we can continue.
            if (result.failed()) break;
        }
        return result;
    }

    void visitVariableDeclaration(AST::VariableDeclaration& variable) {
        Type *declaredType = nullptr;
        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            declaredType = typeResolver.resolveType(*typeDeclaration);

            if (!declaredType) {
                Diagnostic::error(*typeDeclaration, "Unable to resolve type declaration.");
                result = ERROR;
                return;
            }
        }

        Type *type = nullptr;
        if (AST::Expression *initial = variable.getInitialValue()) {
            ExpressionTypeChecker checker{typeResolver};
            type = checker.typeCheckExpressionUsingDeclaredOrDefaultType(*initial, declaredType);

            if (!type) {
                result = ERROR;
                return;
            }

            if (declaredType && type != declaredType) {
                auto [coerceResult, wrapped] = coerceType(*declaredType, *type, *initial);
                if (wrapped) {
                    variable.setWrappedInitialValue(std::move(wrapped));
                }
                result |= coerceResult;
                type = declaredType;
            }
        } else {
            type = declaredType;
        }
        
        auto& binding = llvm::cast<AST::IdentifierBinding>(variable.getBinding());
        binding.setType(type);
        binding.setIsMutable(variable.getIsMutable());
        variable.setType(*type);
    }

    void visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        assert(false);
    }

    void visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        assert(false);
    }

    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        assert(false);
    }

    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        assert(false);
        // Store type as protocol type.
    }

    void visitStatementDeclaration(AST::StatementDeclaration& statement) {
        statement.getStatement().acceptVisitor(*this);
    }

    // Statements
    
    void visitAssignmentStatement(AST::AssignmentStatement& assignment) {
        ExpressionTypeChecker typeChecker{typeResolver};
        TypeResult target = typeChecker.typeCheckExpression(assignment.getTarget());
        if (!target) {
            result = ERROR;
            return;
        }
        if (target.isConstraint()) {
            result = ERROR;
            // TODO; get cause of r-value-ness.
            Diagnostic::error(assignment.getTarget(), "Cannot assign, target is not assignable");
            return;
        }
        if (!target.canAssign()) { 
            result = ERROR;
            // TODO; get cause of r-value-ness.
            Diagnostic::error(assignment.getTarget(), "Cannot assign, target is not assignable");
            return;
        }
        Type *targetType = target.asType();
        Type *valueType = typeChecker.typeCheckExpression(assignment.getValue(), targetType);
        if (!valueType) {
            result = ERROR;
            return;
        }

        auto [coerceResult, wrapped] = coerceType(*targetType, *valueType, assignment.getValue());
        if (wrapped) {
            assignment.setWrappedValue(std::move(wrapped));
        }
        result |= coerceResult;
    }

    void visitCompoundAssignmentStatement(AST::CompoundAssignmentStatement& assignment) {
        // TODO: This needs to type check the binary operation between the target and the operand.
        ExpressionTypeChecker typeChecker{typeResolver};
        TypeResult target = typeChecker.typeCheckExpression(assignment.getTarget());
        if (!target) {
            result = ERROR;
            return;
        }
        if (target.isConstraint()) {
            result = ERROR;
            Diagnostic::error(assignment.getTarget(), "Cannot compound assign, target is not assignable");
            return;
        }
        if (!target.canAssign()) { 
            result = ERROR;
            // TODO; get cause of r-value-ness.
            Diagnostic::error(assignment.getTarget(), "Cannot compound assign, target is not assignable");
            return;
        }
        Type *targetType = target.asType();
        Type *valueType = typeChecker.typeCheckExpression(assignment.getOperand(), targetType);
        if (!valueType) {
            result = ERROR;
            return;
        }

        auto [coerceResult, wrapped] = coerceCompoundAssignmentOperand(*targetType, *valueType, assignment.getOp(), assignment.getOperand());

        if (coerceResult.failed()) {
            result = ERROR;
            return;
        }

        if (wrapped) {
            assignment.setWrappedOperand(wrapped);
        }
    }

    void handleConditionalBinding(AST::VariableDeclaration& variable) {
        Type *declaredType = nullptr;
        Type *propagatedType = nullptr;
        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            declaredType = typeResolver.resolveType(*typeDeclaration);

            if (!declaredType) {
                Diagnostic::error(*typeDeclaration, "Unable to resolve type declaration.");
                result = ERROR;
                return;
            }

            propagatedType = declaredType->getOptionalType();
        }

        Type *type = nullptr;
        AST::Expression *initial = variable.getInitialValue();
        assert(initial);
        ExpressionTypeChecker checker{typeResolver};
        if (declaredType) {
            type = checker.typeCheckExpression(*initial, propagatedType);
        } else {
            type = checker.typeCheckExpression(*initial);
        }

        if (!type) {
            result = ERROR;
            return;
        }
        if (!isa<OptionalType>(type)) {
            Diagnostic::error(*initial, "Expected optional type in conditional binding.");
            result = ERROR;
            return;
        }
        auto optionalType = cast<OptionalType>(type);
        type = optionalType->getContained();

        if (declaredType && type != declaredType) {
            Diagnostic::error(*initial, "Cannot coerce in conditional binding.");
            result = ERROR;
            type = declaredType;
        }
        
        auto& binding = llvm::cast<AST::IdentifierBinding>(variable.getBinding());
        binding.setType(type);
        binding.setIsMutable(variable.getIsMutable());
        variable.setType(*type);
    }

    void handleCondition(AST::Condition condition) {
        TypeSwitch<AST::Condition>(condition)
            .Case<AST::VariableDeclaration *>([&](auto variable) {
                handleConditionalBinding(*variable);
            })
            .Case<AST::Expression *>([&](auto expression) {
                ExpressionTypeChecker typeChecker{typeResolver};
                Type *type = typeChecker.typeCheckExpression(*expression, typeResolver.booleanType());
                if (!type) {
                    if (!type) {
                        result = ERROR;
                    } else if (type != typeResolver.booleanType()) {
                        Diagnostic::error(*expression, "Condition in if statement is not a boolean");
                        result = ERROR;
                    }
                }
            });
    }

    void visitIfStatement(AST::IfStatement& ifStatement) {
        for (int bi = 0; bi < ifStatement.getConditionCount(); ++bi) {
            auto& branch = ifStatement.getCondition(bi);
            for (auto condition : branch.getConditions()) {
                handleCondition(condition);
            }
            visitBlock(branch.getBlock());
        }
        if (auto* fallback = ifStatement.getFallback()) {
            visitBlock(*fallback);
        }
    }

    void visitGuardStatement(AST::GuardStatement& guardStatement) {
        ExpressionTypeChecker typeChecker{typeResolver};

        for (auto condition : guardStatement.getConditions()) {
            handleCondition(condition);
        }
        visitBlock(guardStatement.getBlock());
    }

    void visitReturnStatement(AST::ReturnStatement& returnStatement) {
        Type *returnType = currentFunction->getReturnType();
        if (auto* value = returnStatement.getValue()) {
            if (returnType == typeResolver.voidType()) {
                Diagnostic::error(returnStatement, "Returning non-void value in function with void return type.");
                result = ERROR;
            }
            ExpressionTypeChecker typeChecker{typeResolver};
            Type *type = typeChecker.typeCheckExpression(*value, currentFunction->getReturnType());
            if (!type) {
                result = ERROR;
                return;
            }
            
            if (returnType != type) {
                auto [coerceResult, wrapped] = coerceType(*returnType, *type, *value);

                if (wrapped) {
                    returnStatement.setWrappedValue(std::move(wrapped));
                }

                result |= coerceResult;
            }
        } else {
            if (currentFunction->getReturnType() != typeResolver.voidType()) {
                Diagnostic::error(returnStatement, "No return value in function with non-void return type.");
                result = ERROR;
            }
        }
    }

    void visitWhileStatement(AST::WhileStatement& whileStatement) {
        for (int ci = 0; ci < whileStatement.getNumConditions(); ++ci) {
            AST::Condition condition = whileStatement.getCondition(ci);
            handleCondition(condition);
        }
        visitBlock(whileStatement.getBlock());
    }

    void visitForStatement(AST::ForStatement& forStatement) {
        ExpressionTypeChecker typeChecker{typeResolver};

        // TODO: Make a version that discards constraints and returns nullptr.
        Type *type = typeChecker.typeCheckExpression(forStatement.getIterable());

        if (!type) {
            result = ERROR;
            return;
        }

        Type *elementType = TypeSwitch<Type *, Type *>(type)
            .Case([&forStatement](ArrayType *arrayType) -> Type * {
                if (!arrayType->isBounded) {
                    Diagnostic::error(forStatement.getIterable(), "Cannot iterate over unbounded array.");
                    return nullptr;
                } else {
                    return arrayType->getContained();
                }
            })
            .Case([](RangeType *rangeType) -> Type * {
                return rangeType->getBoundType();
            })
            .Default([&forStatement](Type *type) -> Type * {
                Diagnostic::error(forStatement.getIterable(), "Target of for loop is not iterable.");
                return nullptr;
            });

        if (!elementType) {
            return;
        }

        forStatement.getBinding().setType(elementType);

        visitBlock(forStatement.getBlock());
    }

    void visitBreakStatement(AST::BreakStatement&) {}
    void visitContinueStatement(AST::ContinueStatement&) {}

    void visitExpressionStatement(AST::ExpressionStatement& expression) {
        ExpressionTypeChecker typeChecker{typeResolver};
        Type *type = typeChecker.typeCheckExpression(expression.getExpression());
        if (!type) {
            result = ERROR;
        } else if (type != typeResolver.voidType()) {
            Diagnostic::warning(expression, "Discarding non-void result");
        }
    }
};

PassResult populateEnumCases(auto& enums, TypeResolver& typeResolver) {
    PassResult result = OK;
    for (auto& enumType : enums) {
        result |= populateCasesInEnumType(*enumType, typeResolver);
    }
    return result;
}

PassResult typecheckModuleDefinition(ModuleDef& moduleDefinition)
{
    PassResult result = OK;

    std::vector<Type *> _owner;

    TypeResolver resolver{moduleDefinition, builtins};

    result |= populateEnumCases(moduleDefinition.enums, resolver);

    PointerMap<AST::IdentifierBinding *, AST::VariableDeclaration *> globalAncestors;
    GlobalDeclarationTypeChecker typeChecker{moduleDefinition, builtins, globalAncestors};

    for (const auto function : moduleDefinition.functions) {
        result |= typeChecker.typeCheckFunction(*function);
    }

    // Types are populated after here.

    result |= typeCheckStructs(moduleDefinition.structs, resolver);

    result |= typeChecker.typeCheckGlobals(moduleDefinition.globals);

    if (result.failed()) {
        return ERROR;
    }

    // All global types are known here.

    FunctionTypeChecker bodyTypeChecker{moduleDefinition, builtins};
    for (const auto function : moduleDefinition.functions) {
        result |= bodyTypeChecker.typeCheckFunctionBody(*function);
    }

    return result;
}
