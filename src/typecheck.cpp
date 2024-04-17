#include "AST.h"
#include "AST_Visitor.h"
#include "diagnostic.h"
#include "typecheck.h"

#include "type.h"
#include "type/struct.h"

#include "llvm/ADT/TypeSwitch.h"

using enum PassResultKind;
using Result = PassResult;

using llvm::dyn_cast;


VoidType void_;
BooleanType boolean;
IntegerType signed64{64, true};


class TypeResolver : public AST::TypeNodeVisitorT<TypeResolver, Type*> {
    ModuleDef& moduleDefinition;

public:
    TypeResolver(ModuleDef& moduleDefinition) : moduleDefinition{moduleDefinition} {};

    Type *resolveType(AST::TypeNode& typeNode) {
        if (auto type = typeNode.acceptVisitor(*this)) {
            return type;
        } else {
            Diagnostic::error(typeNode, "Unable to resolve type name");
            return nullptr;
        }
    }

    Type *visitTypeLiteral(AST::TypeLiteral& literal) {
        if (auto type = moduleDefinition.types.lookup(literal.getName())) {
            return *type;

        } else if (literal.getName() == "int") {
            return &signed64;
        } else if (literal.getName() == "bool") {
            return &boolean;
        } else {
            return nullptr;
        }
    }
};

class ExpressionAssignmentTypeChecker : public AST::ExpressionVisitorT<ExpressionAssignmentTypeChecker, Type *> {
    // TODO: We need an expression type checker once we implement member access.
public:
    ExpressionAssignmentTypeChecker() {}

    Type *typeCheckExpression(AST::Expression& expression) {
        return expression.acceptVisitor(*this);
    }

    Type *visitUnaryExpression(AST::UnaryExpression& unary) {
        Diagnostic::error(unary, "Cannot assign to result of unary expression.");
        return nullptr;
    }

    Type *visitBinaryExpression(AST::BinaryExpression& binary) {
        Diagnostic::error(binary, "Cannot assign to result of binary expression.");
        return nullptr;
    }

    Type *visitCallExpression(AST::CallExpression& call) {
        Diagnostic::error(call, "Cannot assign to result of function call.");
        return nullptr;
    }

    Type *visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess) {
        assert(false);
        // TODO: This may need to be checked as a normal expression.
        Type *targetType = typeCheckExpression(memberAccess.getTarget());
        // TODO: Resolve member for type.
        // TODO: Check that value is lvalue.
        
        std::cout << targetType << "\n";
        if (auto *structType = dyn_cast<StructType>(targetType)) {
            std::cout << structType;
        }

        assert(false);
        return nullptr;
    }

    Type *visitLiteral(AST::Literal& literal) {
        Diagnostic::error(literal, "Cannot assign to literal.");
        return nullptr;
    }

    Type *visitIdentifier(AST::Identifier& identifier) {
        auto* resolution = identifier.getResolution();
        Type *type = llvm::TypeSwitch<IdentifierResolution *, Type *>(resolution)
            .Case<LocalResolution>([this, &identifier](auto local) {
                auto &var = local->getVariableDeclaration();
                if (var.getIsMutable()) {
                    return local->getVariableDeclaration().getType();
                } else {
                    // TODO: Should this have the entire assignment statement?
                Diagnostic::error(identifier, "Cannot assign to let constant.");
                    return (Type *) nullptr;
                }
            })
            .Case<FunctionResolution>([this, &identifier](auto functionResolution) {
                Diagnostic::error(identifier, "Cannot assign to global function. Functions are immutable.");
                return nullptr;
            })
            .Case<FunctionParameterResolution>([this, &identifier](auto parameter) {
                Diagnostic::error(identifier, "Cannot assign to function parameter.");
                return nullptr;
                //return parameter->getFunctionDeclaration()->getParameter(parameter->getParameterIndex()).type;
            })
        ;
        identifier.setType(type);
        return type;
    }
};

class ExpressionTypeChecker : public AST::ExpressionVisitorT<ExpressionTypeChecker, Type *, Type *> {

public:
    ExpressionTypeChecker() {}

    Type *typeCheckExpression(AST::Expression& expression, Type *declaredType = nullptr) {
        return expression.acceptVisitor(*this, declaredType);
    }

    Type *visitUnaryExpression(AST::UnaryExpression& unary, Type *declaredType) {
        // TODO: Just passing on the declared type will not be sufficient
        Type *targetType = unary.getTarget().acceptVisitor(*this, declaredType);

        using enum AST::UnaryOperator;
        switch (unary.getOp()) {
            case AddressOf:
                assert(false);
            case Not:
                if (targetType == &boolean) {
                    return &boolean;
                } else {
                    return nullptr;
                }
            case Negate:
                // Fixme ensure that type is numeric.
                return targetType;
        }
    }

    Type *visitBinaryExpression(AST::BinaryExpression& binary, Type *declaredType) {
        Type *leftType = typeCheckExpression(binary.getLeft());
        Type *rightType = typeCheckExpression(binary.getRight());

        /* TODO: type checking integer and floating point types should 
         * perhaps have an extra type for unconstrained types for literals. */

        if (leftType != rightType) {

            std::cout << "Non-matching types in binary operation.\n" << leftType->getKind() << " " << rightType->getKind() << "\n";
            // TODO: Error
            return nullptr;
        }

        using enum AST::BinaryOperator;
        switch (binary.getOp()) {
            case LogicalOr:
            case LogicalAnd:
                if (leftType != &boolean) {
                    return nullptr;
                    // TODO: Error
                } else {
                    return &boolean;
                }
            case Less:
            case LessEqual:
            case Greater:
            case GreaterEqual:
                if (leftType != &signed64) {
                    Diagnostic::error(binary, "Attempting to compare non-integer types.");
                    return nullptr;
                } else {
                    return &boolean;
                }

            case BitwiseAnd:
            case BitwiseOr:
            case BitwiseXor:
                if (leftType != &signed64) {
                    Diagnostic::error(binary, "Attempting to compare non-integer types.");
                    return nullptr;
                } else {
                    return &signed64;
                }   

            case Equal:
            case NotEqual:
                return &boolean;
                break;
            case Add:
            case Subtract:
            case Multiply:
            case Divide:
            case Modulo:
                if (leftType != &signed64) {
                    Diagnostic::error(binary, "Attempting to perform arithmetic on non-integer types.");
                    return nullptr;
                } else {
                    return &signed64;
                }
                break;
        }

        binary.setType(leftType);
        return leftType;
    }

    Type *visitCallExpression(AST::CallExpression& call, Type *declaredType) {
        // Verify that function exists, and call matches arity.
        // Return return type of function.
        auto type = typeCheckExpression(call.getTarget());
        if (FunctionType *functionType = dyn_cast<FunctionType>(type)) {
            if (functionType->parameterCount() != call.argumentCount()) {
                std::cout << "Non-matching number of arguments in call.";
                return nullptr;
            }

            // This needs to be whether one type can be converted to the other.
            for (int i = 0; i < functionType->parameterCount(); ++i) {
                Type *argumentType = typeCheckExpression(call.getArgument(i));
                Type *parameterType = functionType->getParameter(i);
                if (argumentType != parameterType) {
                    std::cout << "Wrong argument type in call." << argumentType->getKind() << " " << parameterType->getKind();
                    return nullptr;
                }
            }

            Type *type = functionType->getReturnType();
            call.setType(type);
            return type;
        } else {
            Diagnostic::error(call, "Attempting to call non-function value.");
            return nullptr;
        }
    }

    Type *visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess, Type *declaredType) {
        Type *targetType = typeCheckExpression(memberAccess.getTarget(), nullptr);
        
        if (!targetType) {
            return nullptr;
        }

        if (auto structType = llvm::dyn_cast_if_present<StructType>(targetType)) {
            auto [memberResolution, memberType] = structType->resolveMember(memberAccess.getMemberName());
            if (memberResolution) {
                memberAccess.setType(memberType);
                memberAccess.setResolution(std::move(memberResolution));
                return memberType;
            } else {
                Diagnostic::error(memberAccess, "Unable to resolve struct member");
                return nullptr;
            }
        }

        Diagnostic::error(memberAccess, "Type does not support member access.");

        return nullptr;
    }

    Type *visitLiteral(AST::Literal& literal, Type *declaredType) {
        using enum AST::Literal::Type;
        // TODO: Validate against declaration type.
        switch (literal.getLiteralType()) {
            case Boolean:
                return &boolean;
            case Integer:
                // FIXME: Check declared integer type
                // Can be converted to smaller integer types or a floating point value.
                literal.setType(&signed64);
                return &signed64;
            case Double:
                Diagnostic::error(literal, "Floating-point literals are currently not supported.");
                exit(-1);
                break;
            case String:
                Diagnostic::error(literal, "String literals are currently not supported.");
                exit(-1);
                break;
        }
    }

    Type *visitIdentifier(AST::Identifier& identifier, Type *declaredType) {
        auto* resolution = identifier.getResolution();
        Type *type = llvm::TypeSwitch<IdentifierResolution *, Type *>(resolution)
            .Case<LocalResolution>([](auto local) {
                return local->getVariableDeclaration().getType();
            })
            .Case<FunctionResolution>([](auto functionResolution) {
                AST::FunctionDeclaration *function = functionResolution->getFunctionDeclaration();
                return function->getType();
//                std::vector<Type *> parameterTypes;
//                for (int i = 0; i < function->getParameterCount(); ++i) {
//                    parameterTypes.push_back(function->getParameter(i).type);
//                }
//                // FIXME: THIS IS A LEAK
//                return new FunctionType(function->getReturnType(), std::move(parameterTypes));
            })
            .Case<FunctionParameterResolution>([](auto parameter) {
                return parameter->getFunctionDeclaration()->getParameter(parameter->getParameterIndex()).type;
            })
        ;
        identifier.setType(type);
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
            returnType = &void_;
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

            if (!declaredType) {
                Diagnostic::error(*typeDeclaration, "Unable to resolve type declaration.");
                // TODO: Emit error
                result = ERROR;
                return;
            }
        }

        Type *initialType = nullptr;
        if (AST::Expression *initial = variable.getInitialValue()) {
            ExpressionTypeChecker checker;
            initialType = checker.typeCheckExpression(*initial);

            if (!initialType) {
                // TODO: Remove
                Diagnostic::error(*initial, "Unable to resolve type for expression.");
                result = ERROR;
            }
        }

        variable.setType(*initialType);
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

    void visitAssignmentStatement(AST::AssignmentStatement& assignment) {
        ExpressionAssignmentTypeChecker assignmentChecker;
        ExpressionTypeChecker typeChecker;
        Type *targetType = assignmentChecker.typeCheckExpression(assignment.getTarget());
        if (!targetType) {
            result = ERROR;
        }
        Type *valueType = typeChecker.typeCheckExpression(assignment.getValue(), targetType);
        if (!valueType) {
            result = ERROR;
        }
    }

    void visitIfStatement(AST::IfStatement& ifStatement) {
        ExpressionTypeChecker typeChecker;
        for (int ci = 0; ci < ifStatement.getConditionCount(); ++ci) {
            auto& branch = ifStatement.getCondition(ci);
            Type *type = typeChecker.typeCheckExpression(branch.getCondition(), &boolean);
            if (!type) {
                result = ERROR;
            } else if (type != &boolean) {
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

        Type *type = typeChecker.typeCheckExpression(guardStatement.getCondition(), &boolean);
        if (!type) {
            result = ERROR;
        } else if (type != &boolean) {
            Diagnostic::error(guardStatement.getCondition(), "Condition in if statement is not a boolean");
            result = ERROR;
        }
        visitBlock(guardStatement.getBlock());
    }

    void visitReturnStatement(AST::ReturnStatement& returnStatement) {
        Type *returnType = currentFunction->getReturnType();
        if (auto* value = returnStatement.getValue()) {
            if (returnType == &void_) {
                Diagnostic::error(returnStatement, "Returning non-void value in function with void return type.");
                result = ERROR;
            }
            ExpressionTypeChecker typeChecker;
            Type *type = typeChecker.typeCheckExpression(*value,  currentFunction->getReturnType());
            if (!type) {
                result = ERROR;
            }
        } else {
            if (currentFunction->getReturnType() != &void_) {
                Diagnostic::error(returnStatement, "No return value in function with non-void return type.");
                result = ERROR;
            }
        }
    }

    void visitWhileStatement(AST::WhileStatement& whileStatement) {
        ExpressionTypeChecker typeChecker;
        Type *type = typeChecker.typeCheckExpression(whileStatement.getCondition(), &boolean);
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
        Type *type = typeChecker.typeCheckExpression(expression.getExpression(), nullptr);
        if (!type) {
            result = ERROR;
        } else if (type != &void_) {
            std::cout << "Discarding non-void result.";
        }
    }
};

Result typeCheckType(Type *type) 
{

}

//template <class T>
//concept TypeResolver = requires(T t, std::string& s)
//{
//    { t(s) } -> std::same_as<Type*>;
//    { t.getIdentifier(s) } -> std::same_as<Type *>;
//};

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
        bodyTypeChecker.typeCheckFunctionBody(*function);
    }


    return result;
}
