#include "typecheck/assignment.h"

// TODO: We need to explain why we failed to assign to a value.
void diagnoseAssignmentToRValue(const AST::Expression& expression, const AST::AssignmentStatement& assignment) {

}

//class ExpressionAssignmentTypeChecker : public AST::ExpressionVisitorT<ExpressionAssignmentTypeChecker, Type *> {
//public:
//    ExpressionAssignmentTypeChecker() {}
//
//    Type *typeCheckExpression(AST::Expression& expression) {
//        return expression.acceptVisitor(*this);
//    }
//
//    Type *visitUnaryExpression(AST::UnaryExpression& unary) {
//        Diagnostic::error(unary, "Cannot assign to result of unary expression.");
//        return nullptr;
//    }
//
//    Type *visitBinaryExpression(AST::BinaryExpression& binary) {
//        Diagnostic::error(binary, "Cannot assign to result of binary expression.");
//        return nullptr;
//    }
//
//    Type *visitCallExpression(AST::CallExpression& call) {
//        Diagnostic::error(call, "Cannot assign to result of function call.");
//        return nullptr;
//    }
//
//    Type *visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess) {
//        Type *targetType = typeCheckExpression(memberAccess.getTarget());
//        if (!targetType) {
//            return nullptr;
//        }
//
//        if (auto structType = llvm::dyn_cast_if_present<StructType>(targetType)) {
//            auto [memberResolution, memberType] = structType->resolveMember(memberAccess.getMemberName());
//            if (memberResolution) {
//                memberAccess.setType(memberType);
//                memberAccess.setResolution(std::move(memberResolution));
//                return memberType;
//            } else {
//                Diagnostic::error(memberAccess, "Unable to resolve struct member");
//                return nullptr;
//            }
//        }
//
//        Diagnostic::error(memberAccess, "Type does not support member access.");
//
//        return nullptr;
//    }
//
//    Type *visitLiteral(AST::Literal& literal) {
//        Diagnostic::error(literal, "Cannot assign to literal.");
//        return nullptr;
//    }
//
//    Type *visitIdentifier(AST::Identifier& identifier) {
//        auto* resolution = identifier.getResolution();
//        Type *type = llvm::TypeSwitch<IdentifierResolution *, Type *>(resolution)
//            .Case<LocalResolution>([this, &identifier](auto local) {
//                auto &var = local->getVariableDeclaration();
//                if (var.getIsMutable()) {
//                    return local->getVariableDeclaration().getType();
//                } else {
//                    // TODO: Should this have the entire assignment statement?
//                Diagnostic::error(identifier, "Cannot assign to let constant.");
//                    return (Type *) nullptr;
//                }
//            })
//            .Case<FunctionResolution>([this, &identifier](auto functionResolution) {
//                Diagnostic::error(identifier, "Cannot assign to global function. Functions are immutable.");
//                return nullptr;
//            })
//            .Case<FunctionParameterResolution>([this, &identifier](auto parameter) {
//                Diagnostic::error(identifier, "Cannot assign to function parameter.");
//                return nullptr;
//                //return parameter->getFunctionDeclaration()->getParameter(parameter->getParameterIndex()).type;
//            })
//        ;
//        identifier.setType(type);
//        return type;
//    }
//};
