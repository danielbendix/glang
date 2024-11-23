#include "AST.h"
#include "AST_Visitor.h"
#include "builtins.h"
#include "diagnostic.h"
#include "typecheck.h"
#include "typecheck/scope.h"
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
    Module& module;
    TypeResolver& typeResolver;
    PointerMap<AST::IdentifierBinding *, AST::VariableDeclaration *> ancestors;
    llvm::SmallVector<u32, 8> checkStack_;
    ScopeManager& scopeManager;
    ExpressionTypeChecker::GlobalHandler globalHandler;

    std::vector<GlobalDeclaration> orderedGlobals;
public:
    GlobalVariableTypeChecker(Module& module, ScopeManager& scopeManager, TypeResolver& typeResolver) 
        : module{module}, scopeManager{scopeManager}, typeResolver{typeResolver}
    {
        auto globalHandlerLambda = [this](u32 bindingIndex) -> Result {
            auto& binding = this->module.globalBindings[bindingIndex];
            auto declarationIndex = binding.declarationIndex;
            auto& declaration = this->module.globalDeclarations[declarationIndex];
            return typeCheckGlobal(declarationIndex, declaration);
        };
        static_assert(sizeof(globalHandlerLambda) <= 8);
        globalHandler = globalHandlerLambda;
    }

    Result typeCheckGlobal(u32 index, GlobalDeclaration& global) {
        ThreadContext::setCurrentFile(global.file);

        if (auto it = std::ranges::find(checkStack_, index); it != checkStack_.end()) {
            for (auto globalIndex : checkStack_) {
                Diagnostic::error(*module.globalDeclarations[globalIndex].declaration, "Cycle detected in globals.");
            }
            return ERROR;
        }

        AST::VariableDeclaration& variable = *global.declaration;

        Type *declaredType = nullptr;
        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            declaredType = typeResolver.resolveType(*typeDeclaration);
            if (!declaredType) {
                return ERROR;
            }
        }

        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            declaredType = typeResolver.resolveType(*typeDeclaration);
            if (!declaredType) {
                return ERROR;
            }
        }

        Type *type = nullptr;
        if (AST::Expression *initial = variable.getInitialValue()) {
            checkStack_.push_back(index);
            ExpressionTypeChecker typeChecker{scopeManager, typeResolver, globalHandler};
            type = typeChecker.typeCheckExpressionUsingDeclaredOrDefaultType(*initial, declaredType);
            checkStack_.pop_back();
            variable.markAsChecked();
            orderedGlobals.push_back(global);

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
            assert(global.bindingsSize == 1);
            auto& binding = *module.globalBindings[global.bindingsIndex].binding;
            binding.setType(type);
            binding.setIsMutable(variable.getIsMutable());
            variable.setType(*type);
            return OK;
        } else {
            return ERROR;
        }
    }

    Result typeCheckGlobals(std::vector<GlobalDeclaration>& globals) {
        assert(orderedGlobals.empty());
        orderedGlobals.reserve(globals.size());

        Result result = OK;
        for (auto [index, global] : llvm::enumerate(globals)) {
            if (global.declaration->getIsChecked()) { continue; }
            result |= typeCheckGlobal(index, global);
        }

        if (result.ok()) {
            assert(orderedGlobals.size() == globals.size());
            std::swap(globals, orderedGlobals);
        }

        return result;
    }
};

class GlobalDeclarationTypeChecker {
    Module& module;
    ScopeManager scopeManager;
    TypeResolver typeResolver;

    const PointerMap<AST::IdentifierBinding *, AST::VariableDeclaration *>& ancestors;
    llvm::SmallVector<AST::VariableDeclaration *, 4> checkStack;
//    ExpressionTypeChecker::GlobalHandler globalHandler;

public:
    GlobalDeclarationTypeChecker(Module& module, const Builtins& builtins, PointerMap<AST::IdentifierBinding *, AST::VariableDeclaration *>& ancestors) 
        : module{module}
        , scopeManager{module}
        , typeResolver{module, builtins}
        , ancestors{ancestors}
    {}

    Result typeCheckGlobals(std::vector<GlobalDeclaration>& globals) {
        scopeManager.reset();
        GlobalVariableTypeChecker typeChecker{module, scopeManager, typeResolver};
        return typeChecker.typeCheckGlobals(globals);
    }

    Result typeCheckFunction(Function& function, AST::FunctionDeclaration *declaration) {
        ThreadContext::setCurrentFile(function.file);

        Result result = OK;
        Type *returnType;

        if (auto returnTypeNode = declaration->getReturnTypeDeclaration()) {
            returnType = typeResolver.resolveType(*returnTypeNode);
            if (!returnType) {
                result = ERROR;
            }
        } else {
            returnType = typeResolver.voidType();
        }
        
        auto& allocator = typeAllocator();
        auto *typeSpace = allocator.allocate<FunctionType>();
        auto **parameters = (Type **) allocator.allocate(sizeof(Type *) * declaration->getParameterCount(), alignof(Type *));


        for (int i = 0; i < declaration->getParameterCount(); ++i) {
            auto& parameter = declaration->getParameter(i);
            Type *type = typeResolver.resolveType(*parameter.typeDeclaration);
            if (!type) {
                result = ERROR;
            }
            parameters[i] = type;
        }
        if (result.ok()) {
            auto& allocator = typeAllocator();
            auto functionType = new(typeSpace) FunctionType{returnType, parameters, size_t(declaration->getParameterCount())};
               
            function.type = functionType;
        }
        return result;
    }
};

class FunctionTypeChecker : public AST::DeclarationVisitorT<FunctionTypeChecker, void>, public AST::StatementVisitorT<FunctionTypeChecker, void> {

    Result result = OK;
    Function *currentFunction;
    AST::FunctionDeclaration *currentDeclaration;
    ScopeManager scopeManager;
    TypeResolver typeResolver;

    void visitBlock(const AST::Block& block) {
        scopeManager.pushInnerScope();
        for (int bi = 0; bi < block.size(); bi++) {
            block[bi].acceptVisitor(*this);
            if (result.failed()) {
                return;
            }
        }
        scopeManager.popInnerScope();
    }

public:
    FunctionTypeChecker(Module& module, const Builtins& builtins) 
        : scopeManager{module}, typeResolver{module, builtins} 
    {}

    Result typeCheckFunctionBody(Function& function, AST::FunctionDeclaration *declaration, u32 index) {
        ThreadContext::setCurrentFile(function.file);

        scopeManager.reset();

        result = OK;
        currentFunction = &function;
        currentDeclaration = declaration;

        auto functionType = function.type;

        scopeManager.pushOuterScope();
        for (u32 parameterIndex = 0; parameterIndex < function.parameterCount; ++parameterIndex) {
            auto& parameter = declaration->getParameter(parameterIndex);
            result |= scopeManager.pushParameter(
                parameter.name, 
                index,
                parameterIndex,
                declaration
            );
        }
        if (result.failed()) return result;

        scopeManager.pushInnerScope();
        for (auto& declaration : declaration->getCode()) {
            declaration.acceptVisitor(*this);
            // TODO: If a declaration fails, we end, but if a statement fails, we can continue.
            if (result.failed()) {
                break;
            }
        }
        if (result.failed()) {
            scopeManager.reset();
        } else {
            scopeManager.popInnerScope();
            scopeManager.popOuterScope();
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
            ExpressionTypeChecker checker{scopeManager, typeResolver};
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

        result |= scopeManager.pushBinding(binding.getIdentifier(), binding);
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
        ExpressionLValueTypeChecker lvalueTypeChecker{LValueKind::Assignment, scopeManager, typeResolver};
        TypeCheckResult targetResult = lvalueTypeChecker.typeCheckExpressionRequiringInferredType(&assignment.getTarget());
        assignment.setTarget(targetResult.folded());
        if (!targetResult.type()) {
            result = ERROR;
            return;
        }
        Type *targetType = targetResult.type();
        ExpressionTypeChecker typeChecker{scopeManager, typeResolver};
        auto valueResult = typeChecker.typeCheckExpressionUsingDeclaredType(&assignment.getValue(), targetType);
        assignment.setValue(valueResult.folded());
        Type *valueType = valueResult.type();
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
        ExpressionLValueTypeChecker lvalueTypeChecker{LValueKind::CompoundAssignment, scopeManager, typeResolver};
        TypeCheckResult targetResult = lvalueTypeChecker.typeCheckExpressionRequiringInferredType(&assignment.getTarget());
        assignment.setTarget(targetResult.folded());
        if (!targetResult.type()) {
            result = ERROR;
            return;
        }
        Type *targetType = targetResult.type();
        ExpressionTypeChecker typeChecker{scopeManager, typeResolver};
        auto valueResult = typeChecker.typeCheckExpressionUsingDeclaredType(&assignment.getOperand(), targetType);
        assignment.setOperand(valueResult.folded());
        Type *valueType = valueResult.type();
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

    [[nodiscard]]
    Result handleConditionalBinding(AST::VariableDeclaration& variable) {
        auto *initial = variable.getInitialValue();
        if (!initial) {
            Diagnostic::error(variable, "Conditional binding must have a value specified.");
            return ERROR;
        }

        Type *declaredType = nullptr;
        Type *propagatedType = nullptr;
        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            declaredType = typeResolver.resolveType(*typeDeclaration);

            if (!declaredType) {
                Diagnostic::error(*typeDeclaration, "Unable to resolve type declaration.");
                return ERROR;
            }

            propagatedType = declaredType->getOptionalType();
        }

        ExpressionTypeChecker checker{scopeManager, typeResolver};
        TypeCheckResult initialResult = propagatedType 
            ? checker.typeCheckExpressionUsingDeclaredType(initial, propagatedType) 
            : checker.typeCheckExpressionRequiringInferredType(initial);
        variable.setInitialValue(initialResult.folded());
        Type *type = initialResult.type();

        if (!type) {
            return ERROR;
        }
        if (!isa<OptionalType>(type)) {
            Diagnostic::error(*initial, "Expected optional type in conditional binding.");
            return ERROR;
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

        scopeManager.pushInnerScope();
        return scopeManager.pushBinding(binding.getIdentifier(), binding);
    }

    [[nodiscard]]
    Result handleCondition(AST::Condition *condition) {
        return TypeSwitch<AST::Condition, Result>(*condition)
            .Case<AST::VariableDeclaration *>([&](auto variable) {
                return handleConditionalBinding(*variable);
            })
            .Case<AST::Expression *>([&](auto expression) {
                ExpressionTypeChecker typeChecker{scopeManager, typeResolver};
                TypeCheckResult typeResult = typeChecker.typeCheckExpressionUsingDeclaredType(expression, typeResolver.booleanType());
                *condition = typeResult.folded();
                Type *type = typeResult.type();
                if (!type) {
                    if (!type) {
                        return ERROR;
                    } else if (type != typeResolver.booleanType()) {
                        Diagnostic::error(*expression, "Condition in if statement is not a boolean");
                        return ERROR;
                    }
                }
                return OK;
            });
    }

    void visitIfStatement(AST::IfStatement& ifStatement) {
        for (int bi = 0; bi < ifStatement.getConditionCount(); ++bi) {
            scopeManager.withAutoResetScope([&] {
                auto& branch = ifStatement.getCondition(bi);
                for (AST::Condition& condition : branch.getConditions()) {
                    result |= handleCondition(&condition);
                    if (result.failed()) return;
                }
                visitBlock(branch.getBlock());
            });
        }
        if (auto* fallback = ifStatement.getFallback()) {
            visitBlock(*fallback);
        }
    }

    void visitGuardStatement(AST::GuardStatement& guardStatement) {
        // TODO: This does not check in program order,
        // but is the easiest way to deal with guard scopes.
        visitBlock(guardStatement.getBlock());
        scopeManager.withAutoMergingScopes([&] {
            for (AST::Condition& condition : guardStatement.getConditions()) {
                result |= handleCondition(&condition);
                if (result.failed()) return;
            }
        });
    }

    void visitReturnStatement(AST::ReturnStatement& returnStatement) {
        Type *returnType = currentFunction->type->getReturnType();
        if (auto* value = returnStatement.getValue()) {
            if (returnType == typeResolver.voidType()) {
                Diagnostic::error(returnStatement, "Returning non-void value in function with void return type.");
                result = ERROR;
            }
            ExpressionTypeChecker typeChecker{scopeManager, typeResolver};
            auto typeResult = typeChecker.typeCheckExpressionUsingDeclaredType(value, returnType);
            value = typeResult.folded();
            returnStatement.setValue(value);
            auto type = typeResult.type();
            if (!typeResult.type()) {
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
            if (returnType != typeResolver.voidType()) {
                Diagnostic::error(returnStatement, "No return value in function with non-void return type.");
                result = ERROR;
            }
        }
    }

    void visitWhileStatement(AST::WhileStatement& whileStatement) {
        scopeManager.withAutoResetScope([&] {
            for (AST::Condition& condition : whileStatement.getConditions()) {
                result |= handleCondition(&condition);
                if (result.failed()) return;
            }
            visitBlock(whileStatement.getBlock());
        });
    }

    void visitForStatement(AST::ForStatement& forStatement) {
        ExpressionTypeChecker typeChecker{scopeManager, typeResolver};

        TypeCheckResult typeResult = typeChecker.typeCheckExpressionRequiringInferredType(&forStatement.getIterable());
        forStatement.setIterable(typeResult.folded());
        Type *type = typeResult.type();

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

        auto& binding = forStatement.getBinding();
        binding.setType(elementType);

        scopeManager.pushInnerScope();

        auto& identifierBinding = cast<AST::IdentifierBinding>(binding);
        scopeManager.pushBinding(identifierBinding.getIdentifier(), identifierBinding);
        visitBlock(forStatement.getBlock());

        scopeManager.popInnerScope();
    }

    void visitBreakStatement(AST::BreakStatement&) {}
    void visitContinueStatement(AST::ContinueStatement&) {}

    void visitExpressionStatement(AST::ExpressionStatement& expression) {
        ExpressionTypeChecker typeChecker{scopeManager, typeResolver};
        TypeCheckResult typeResult = typeChecker.typeCheckExpressionRequiringInferredType(&expression.getExpression());
        Type *type = typeResult.type();
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

PassResult typecheckModule(Module& module)
{
    PassResult result = OK;

    std::vector<Type *> _owner;

    TypeResolver resolver{module, builtins};

    result |= populateEnumCases(module.enums, resolver);

    PointerMap<AST::IdentifierBinding *, AST::VariableDeclaration *> globalAncestors;
    GlobalDeclarationTypeChecker typeChecker{module, builtins, globalAncestors};

    for (auto [function, declaration] : llvm::zip(module.functions, module.functionDeclarations)) {
        result |= typeChecker.typeCheckFunction(function, declaration);
    }

    // Types are populated after here.

    result |= typeCheckStructs(module.structs, module, resolver);

    result |= typeChecker.typeCheckGlobals(module.globalDeclarations);

    if (result.failed()) {
        return ERROR;
    }

    // All global types are known here.

    FunctionTypeChecker bodyTypeChecker{module, builtins};
    for (u32 functionIndex = 0; functionIndex < module.functions.size(); ++functionIndex) {
        auto& function = module.functions[functionIndex];
        auto *declaration = module.functionDeclarations[functionIndex];

        result |= bodyTypeChecker.typeCheckFunctionBody(function, declaration, functionIndex);
    }

    return result;
}
