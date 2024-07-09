#ifndef LANG_typecheck_resolver_h
#define LANG_typecheck_resolver_h

#include "typecheck.h"

class TypeResolver : public AST::TypeNodeVisitorT<TypeResolver, Type*> {
    ModuleDef& moduleDefinition;
    StringMap<Type *>& builtins;

private:
    Type *resolveType(const std::string& name) {
        if (auto type = moduleDefinition.types.lookup(name)) {
            return *type;
        } else if (auto builtin = builtins.lookup(name)) {
            return *builtin;
        }
        return nullptr;
    }

public:
    TypeResolver(ModuleDef& moduleDefinition, StringMap<Type *>& builtins) : moduleDefinition{moduleDefinition}, builtins{builtins} {};

    Type *resolveType(AST::Identifier& identifier) {
        if (auto type = resolveType(identifier.getName())) {
            return type;
        } else {
            Diagnostic::error(identifier, "Cannot resolve type with name " + identifier.getName());
            return {};
        }
    }

    Type *resolveType(AST::TypeNode& typeNode) {
        return typeNode.acceptVisitor(*this);
        if (auto type = typeNode.acceptVisitor(*this)) {
            return type;
        } else {
            std::cout << typeNode << '\n';
            Diagnostic::error(typeNode, "Unable to resolve type name");
            return nullptr;
        }
    }

    Type *visitTypeLiteral(AST::TypeLiteral& literal) {
        if (auto type = resolveType(literal.getName())) {
            return type;
        } else {
            Diagnostic::error(literal, "Cannot resolve type with name " + literal.getName());
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
            }
            if (!type) {
                std::cout << "FAILED\n";
                break;
            }
        }
        return type;
    }
};

#endif // LANG_typecheck_resolver_h
