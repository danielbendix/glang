#include "AST.h"
#include "AST_Visitor.h"
#include "diagnostic.h"
#include "typecheck.h"
#include "typecheck/coerce.h"
#include "typecheck/resolver.h"
#include "typecheck/expression.h"
#include "typecheck/internal.h"
#include "typecheck/assignment.h"
#include "typecheck/enum.h"

#include "type.h"
#include "type/struct.h"

#include "llvm/ADT/TypeSwitch.h"

using enum PassResultKind;
using Result = PassResult;

using llvm::dyn_cast;
using llvm::isa;
using llvm::TypeSwitch;

VoidType _void_;
BooleanType _boolean;
IntegerType _signed64{64, true};

VoidType *void_ = &_void_;
BooleanType *boolean = &_boolean;
IntegerType *signed64 = &_signed64;


class GlobalDeclarationTypeChecker {
    ModuleDef& moduleDefinition;
    TypeResolver typeResolver;
    // TODO: Add check stack here, to support recursive checking.

public:
    GlobalDeclarationTypeChecker(ModuleDef& moduleDefinition, StringMap<Type *>& builtins) : moduleDefinition{moduleDefinition}, typeResolver{moduleDefinition, builtins} {}

    Result typeCheckGlobal(AST::VariableDeclaration& variable) {
        Type *declaredType = nullptr;
        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            declaredType = typeResolver.resolveType(*typeDeclaration);
            if (!declaredType) {
                return ERROR;
            }
        }

        Type *initialType = nullptr;
        if (AST::Expression *initial = variable.getInitialValue()) {
            
        } else {
            // TODO: Verify that declaration can be without value.
        }

        if (declaredType) {
            variable.setType(*declaredType);
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
            returnType = void_;
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
            auto* functionType = new FunctionType{returnType, std::move(parameterTypes)};
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
    FunctionTypeChecker(ModuleDef& moduleDefinition, StringMap<Type *>& builtins) : typeResolver{moduleDefinition, builtins} {}

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
            if (declaredType) {
                type = checker.typeCheckExpression(*initial, declaredType);
            } else {
                type = checker.typeCheckExpression(*initial);
            }

            if (!type) {
                Diagnostic::error(*initial, "Unable to resolve type for expression XXX.");
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
        assert(false);

        // TODO: This needs to type check the binary operation between the target and the operand.
        ExpressionTypeChecker typeChecker{typeResolver};
        TypeResult target = typeChecker.typeCheckExpression(assignment.getTarget());
        if (!target) {
            result = ERROR;
            return;
        }
        if (target.isConstraint()) {
            result = ERROR;
            // TODO; get cause of r-value-ness.
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
        }
    }

    void visitIfStatement(AST::IfStatement& ifStatement) {
        ExpressionTypeChecker typeChecker{typeResolver};
        for (int ci = 0; ci < ifStatement.getConditionCount(); ++ci) {
            auto& branch = ifStatement.getCondition(ci);
            Type *type = typeChecker.typeCheckExpression(branch.getCondition(), boolean);
            if (!type) {
                result = ERROR;
            } else if (type != boolean) {
                Diagnostic::error(branch.getCondition(), "Condition in if statement is not a boolean");
                result = ERROR;
            }
            visitBlock(branch.getBlock());
        }
        if (auto* fallback = ifStatement.getFallback()) {
            visitBlock(*fallback);
        }
    }

    void visitGuardStatement(AST::GuardStatement& guardStatement) {
        ExpressionTypeChecker typeChecker{typeResolver};

        Type *type = typeChecker.typeCheckExpression(guardStatement.getCondition(), boolean);
        if (!type) {
            result = ERROR;
        } else if (type != boolean) {
            Diagnostic::error(guardStatement.getCondition(), "Condition in if statement is not a boolean");
            result = ERROR;
        }
        visitBlock(guardStatement.getBlock());
    }

    void visitReturnStatement(AST::ReturnStatement& returnStatement) {
        Type *returnType = currentFunction->getReturnType();
        if (auto* value = returnStatement.getValue()) {
            if (returnType == void_) {
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
            if (currentFunction->getReturnType() != void_) {
                Diagnostic::error(returnStatement, "No return value in function with non-void return type.");
                result = ERROR;
            }
        }
    }

    void visitWhileStatement(AST::WhileStatement& whileStatement) {
        ExpressionTypeChecker typeChecker{typeResolver};
        Type *type = typeChecker.typeCheckExpression(whileStatement.getCondition(), boolean);
        if (!type) {
            result = ERROR;
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

        Type *elementType =  TypeSwitch<Type *, Type *>(type)
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

    void visitExpressionStatement(AST::ExpressionStatement& expression) {
        ExpressionTypeChecker typeChecker{typeResolver};
        Type *type = typeChecker.typeCheckExpression(expression.getExpression());
        if (!type) {
            result = ERROR;
        } else if (type != void_) {
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

    StringMap<Type *> builtins;
    std::vector<Type *> _owner;

    TypeResolver resolver{moduleDefinition, builtins};

    createNumericTypes(builtins, _owner);

    result |= populateEnumCases(moduleDefinition.enums, resolver);

    GlobalDeclarationTypeChecker typeChecker{moduleDefinition, builtins};

    for (const auto& function : moduleDefinition._functions) {
        result |= typeChecker.typeCheckFunction(*function);
    }

    // Types are populated after here.

    // TODO: This is a hack
    for (const auto& structType : moduleDefinition.structs) {
        for (auto& field : structType->getFields()) {
            result |= typeChecker.typeCheckGlobal(*field);
        }
    }
    for (const auto& structType : moduleDefinition.structs) {
        for (auto& method : structType->getMethods()) {
            result |= typeChecker.typeCheckFunction(*method);
        }
    }
    for (const auto& global : moduleDefinition._globals) {
        result |= typeChecker.typeCheckGlobal(*global);
        assert(false);
    }

    if (result.failed()) {
        return ERROR;
    }

    // All global types are known here.

    FunctionTypeChecker bodyTypeChecker{moduleDefinition, builtins};
    for (const auto& function : moduleDefinition._functions) {
        result |= bodyTypeChecker.typeCheckFunctionBody(*function);
    }

    return result;
}
