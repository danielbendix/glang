#include "AST.h"
#include "AST_Visitor.h"
#include "diagnostic.h"
#include "typecheck.h"
#include "typecheck/expression.h"
#include "typecheck/internal.h"
#include "typecheck/assignment.h"

#include "type.h"
#include "type/struct.h"

#include "llvm/ADT/TypeSwitch.h"

using enum PassResultKind;
using Result = PassResult;

using llvm::dyn_cast;
using llvm::isa;

VoidType _void_;
BooleanType _boolean;
IntegerType _signed64{64, true};

VoidType *void_ = &_void_;
BooleanType *boolean = &_boolean;
IntegerType *signed64 = &_signed64;


class TypeResolver : public AST::TypeNodeVisitorT<TypeResolver, Type*> {
    ModuleDef& moduleDefinition;

public:
    TypeResolver(ModuleDef& moduleDefinition) : moduleDefinition{moduleDefinition} {};

    Type *resolveType(AST::TypeNode& typeNode) {
        if (auto type = typeNode.acceptVisitor(*this)) {
            return type;
        } else {
            std::cout << typeNode << '\n';
            Diagnostic::error(typeNode, "Unable to resolve type name");
            return nullptr;
        }
    }

    Type *visitTypeLiteral(AST::TypeLiteral& literal) {
        if (auto type = moduleDefinition.types.lookup(literal.getName())) {
            return *type;

        } else if (literal.getName() == "int") {
            return signed64;
        } else if (literal.getName() == "bool") {
            return boolean;
        } else {
            return nullptr;
        }
    }

    Type *visitTypeModifier(AST::TypeModifier& typeModifier) {
        Type *type = typeModifier.getChild().acceptVisitor(*this);

        using enum AST::TypeModifier::Modifier;
        for (auto modifier : typeModifier) {
            switch (modifier) {
                case Pointer:
                    type = type->getPointerType();
                    break;
                case Optional:
                    type = type->getOptionalType();
                    break;
            }
            std::cout << int(modifier) << ' ' << type << '\n';
            if (!type) {
                std::cout << "FAILED\n";
                break;
            }
        }
        return type;
    }
};

class GlobalDeclarationTypeChecker {
    ModuleDef& moduleDefinition;
    TypeResolver typeResolver;
    // TODO: Add check stack here, to support recursive checking.

public:
    GlobalDeclarationTypeChecker(ModuleDef& moduleDefinition) : moduleDefinition{moduleDefinition}, typeResolver{moduleDefinition} {}

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
    FunctionTypeChecker(ModuleDef& moduleDefinition) : typeResolver{moduleDefinition} {}

    Result typeCheckFunctionBody(AST::FunctionDeclaration& function) {
        result = OK;
        currentFunction = &function;
        for (auto& declaration : function.getCode()) {
            declaration.acceptVisitor(*this);
        }
        return result;
    }

    void visitVariableDeclaration(AST::VariableDeclaration& variable) {
        Type *declaredType = nullptr;
        if (auto *typeDeclaration = variable.getTypeDeclaration()) {
            declaredType = typeResolver.resolveType(*typeDeclaration);
            //std::cout << declaredType;

            if (!declaredType) {
                Diagnostic::error(*typeDeclaration, "Unable to resolve type declaration.");
                result = ERROR;
                return;
            }
        }

        Type *type = nullptr;
        if (AST::Expression *initial = variable.getInitialValue()) {
            ExpressionTypeChecker checker;
            if (declaredType) {
                type = checker.typeCheckExpression(*initial, declaredType);
            } else {
                type = checker.typeCheckExpression(*initial);
            }

            if (!type) {
                Diagnostic::error(*initial, "Unable to resolve type for expression.");
                result = ERROR;
            }
        } else {
            type = declaredType;
        }

        // TODO: FIXME: unify types here.

        if (declaredType) {
            variable.setType(*declaredType);
        } else {
            variable.setType(*type);
        }
    }

    void visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        assert(false);
    }

    void visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        assert(false);
    }

    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        // Store type as enum type.
    }

    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        // Store type as protocol type.
    }

    void visitStatementDeclaration(AST::StatementDeclaration& statement) {
        statement.getStatement().acceptVisitor(*this);
    }

    // Statements
    
    AST::AssignmentType unifyTypes(Type *targetType, Type *valueType) {
        if (targetType == valueType) {
            return AST::AssignmentType::Regular;
        }

        if (auto optionalTargetType = dyn_cast<OptionalType>(targetType)) {
            auto contained = optionalTargetType->getContained();
            if (contained == valueType) {
                if (isa<PointerType>(contained)) {
                    // NOTE: Perhaps the type checker should not know what the 
                    // representation of an optional pointer is.
                    return AST::AssignmentType::Regular;
                } else {
                    return AST::AssignmentType::ImplicitOptionalWrap;
                }
            }
        }

        assert(false);
    }

    void visitAssignmentStatement(AST::AssignmentStatement& assignment) {
        ExpressionTypeChecker typeChecker;
        TypeResult target = typeChecker.typeCheckExpression(assignment.getTarget());
        if (!target) {
            result = ERROR;
            return;
        }
        if (!target.canAssign) { 
            result = ERROR;
            // TODO; get cause of r-value-ness.
            Diagnostic::error(assignment.getTarget(), "Cannot assign, target is not assignable");
            return;
        }
        Type *valueType = typeChecker.typeCheckExpression(assignment.getValue(), target.type);
        if (!valueType) {
            result = ERROR;
        }

        unifyTypes(target.type, valueType);
    }

    void visitCompoundAssignmentStatement(AST::CompoundAssignmentStatement& assignment) {
        assert(false);

        // TODO: This needs to type check the binary operation between the target and the operand.
        ExpressionTypeChecker typeChecker;
        TypeResult target = typeChecker.typeCheckExpression(assignment.getTarget());
        if (!target) {
            result = ERROR;
            return;
        }
        if (!target.canAssign) { 
            result = ERROR;
            // TODO; get cause of r-value-ness.
            Diagnostic::error(assignment.getTarget(), "Cannot compound assign, target is not assignable");
            return;
        }
        Type *valueType = typeChecker.typeCheckExpression(assignment.getOperand(), target.type);
        if (!valueType) {
            result = ERROR;
        }
    }

    void visitIfStatement(AST::IfStatement& ifStatement) {
        ExpressionTypeChecker typeChecker;
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
        ExpressionTypeChecker typeChecker;

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
            ExpressionTypeChecker typeChecker;
            Type *type = typeChecker.typeCheckExpression(*value,  currentFunction->getReturnType());
            if (!type) {
                result = ERROR;
            }
        } else {
            if (currentFunction->getReturnType() != void_) {
                Diagnostic::error(returnStatement, "No return value in function with non-void return type.");
                result = ERROR;
            }
        }
    }

    void visitWhileStatement(AST::WhileStatement& whileStatement) {
        ExpressionTypeChecker typeChecker;
        Type *type = typeChecker.typeCheckExpression(whileStatement.getCondition(), boolean);
        if (!type) {
            result = ERROR;
        }
        visitBlock(whileStatement.getBlock());
    }

    void visitForStatement(AST::ForStatement& forStatement) {
        // TODO: Implement this.
    }

    void visitExpressionStatement(AST::ExpressionStatement& expression) {
        ExpressionTypeChecker typeChecker;
        Type *type = typeChecker.typeCheckExpression(expression.getExpression());
        if (!type) {
            result = ERROR;
        } else if (type != void_) {
            Diagnostic::warning(expression, "Discarding non-void result");
        }
    }
};

PassResult typecheckModuleDefinition(ModuleDef& moduleDefinition)
{
    PassResult result = OK;

    GlobalDeclarationTypeChecker typeChecker{moduleDefinition};

    for (const auto& function : moduleDefinition.functions) {
        result |= typeChecker.typeCheckFunction(*function);
    }
    // TODO: This is a hack
    for (const auto& type : moduleDefinition._types) {
        if (auto structType = dyn_cast<StructType>(type.get())) {
            for (auto& method : structType->getMethods()) {
                result |= typeChecker.typeCheckFunction(*method);
            }
        }
    }
    for (const auto& global : moduleDefinition.globals) {
        result |= typeChecker.typeCheckGlobal(*global);
        assert(false);
    }
    // TODO: This is a hack
    for (const auto& type : moduleDefinition._types) {
        if (auto structType = dyn_cast<StructType>(type.get())) {
            for (auto& field : structType->getFields()) {
                result |= typeChecker.typeCheckGlobal(*field);
            }
        }
    }

    FunctionTypeChecker bodyTypeChecker{moduleDefinition};
    for (const auto& function : moduleDefinition.functions) {
        result |= bodyTypeChecker.typeCheckFunctionBody(*function);
    }


    return result;
}
