#ifndef LANG_typecheck_resolver_h
#define LANG_typecheck_resolver_h

#include "typecheck.h"
#include "typeconstraint.h"

#include "builtins.h"

class TypeResolver : public AST::TypeNodeVisitorT<TypeResolver, Type*> {
    ModuleDef& moduleDefinition;
    const Builtins& builtins;

private:
    Type *resolveType(const Symbol& name) {
        if (auto type = moduleDefinition.types.lookup(name)) {
            return *type;
        } else if (auto builtin = builtins.types.lookup(name)) {
            return *builtin;
        }
        return nullptr;
    }

public:
    TypeResolver(ModuleDef& moduleDefinition, const Builtins& builtins) : moduleDefinition{moduleDefinition}, builtins{builtins} {};

    Type *defaultTypeFromTypeConstraint(TypeConstraint *constraint) {
        switch (constraint->getKind()) {
            case TypeConstraintKind::Numeric:
                return defaultIntegerType();
            case TypeConstraintKind::Floating:
                return defaultFPType();
            case TypeConstraintKind::Optional:
                return nullptr;
        }
    }

    Type *resolveType(AST::Identifier& identifier) {
        if (auto type = resolveType(identifier.getName())) {
            return type;
        } else {
            Diagnostic::error(identifier, "Cannot resolve type with name " + identifier.getName().string());
            return {};
        }
    }

    Type *resolveType(AST::TypeNode& typeNode) {
        return typeNode.acceptVisitor(*this);
        if (auto type = typeNode.acceptVisitor(*this)) {
            return type;
        } else {
            Diagnostic::error(typeNode, "Unable to resolve type name");
            return nullptr;
        }
    }

    Type *visitTypeLiteral(AST::TypeLiteral& literal) {
        if (auto type = resolveType(literal.getName())) {
            return type;
        } else {
            Diagnostic::error(literal, "Cannot resolve type with name " + literal.getName().string());
            return {};
        }
    }

    Type *visitTypeModifier(AST::TypeModifier& typeModifier) {
        Type *type = typeModifier.getChild().acceptVisitor(*this);

        if (!type) {
            return {};
        }

        using enum AST::TypeModifier::Modifier;
        for (auto modifier : typeModifier) {
            switch (modifier) {
                case Pointer:
                    type = type->getPointerType();
                    break;
                case Optional:
                    type = type->getOptionalType();
                    break;
                case AST::TypeModifier::Modifier::Location:
                    assert(false);
                    break;
                case AST::TypeModifier::Modifier::Array:
                    type = type->getBoundedArrayType();
                    break;
                case AST::TypeModifier::Modifier::UnboundedArray:
                    type = type->getUnboundedArrayType();
                    break;
                }
            if (!type) {
                break;
            }
        }
        return type;
    }

    VoidType *voidType() const {
        return builtins.voidType;
    }

    BooleanType *booleanType() const {
        return builtins.booleanType;
    }

    IntegerType *defaultIntegerType() const {
        return builtins.defaultIntegerType;
    }

    FPType *defaultFPType() const {
        return builtins.defaultFPType;
    }
};

#endif // LANG_typecheck_resolver_h
