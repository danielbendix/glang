#ifndef LANG_resolution_identifier_h
#define LANG_resolution_identifier_h

#include "common.h"
#include "type.h"

#include <memory>

namespace AST {
    class Declaration;
    class IdentifierBinding;
    class FunctionDeclaration;
    class StructDeclaration;
};

struct IdentifierResolution {
    enum class Kind : uint8_t {
        UNRESOLVED,
        Global,
        Function,
        Parameter,
        Local,
        Type,
    };

    struct Unresolved {
        Kind kind = Kind::UNRESOLVED;
    };

    struct Local {
        Kind kind = Kind::Local;
        AST::IdentifierBinding *NONNULL binding;

        Local(AST::IdentifierBinding *NONNULL binding) : binding{binding} {}
    };

    struct Global {
        Kind kind = Kind::Global;
        bool isExtern;
        AST::IdentifierBinding *NONNULL binding;

        Global(AST::IdentifierBinding *NONNULL binding, bool isExtern) : binding{binding}, isExtern{isExtern} {}
    };

    struct Function {
        Kind kind = Kind::Function;
        AST::FunctionDeclaration *NONNULL function;

        Function(AST::FunctionDeclaration *NONNULL function) : function{function} {}
    };

    struct Parameter {
        Kind kind = Kind::Parameter;
        int parameterIndex;
        AST::FunctionDeclaration *NONNULL function;

        Parameter(AST::FunctionDeclaration *NONNULL function, int parameterIndex) : function{function}, parameterIndex{parameterIndex} {}
    };

    struct TypeIdentifier {
        Kind kind = Kind::Type;
        Type *NONNULL type;

        TypeIdentifier(Type *NONNULL type) : type{type} {}
    };
    
    union AS {
        Unresolved unresolved;
        Local local;
        Global global;
        Function function;
        Parameter parameter;
        TypeIdentifier type;
    } as;

    IdentifierResolution() : as{Unresolved{}} {}

    Kind getKind() const {
        return as.unresolved.kind;
    }

    operator bool() const {
        return getKind() != Kind::UNRESOLVED;
    }

    static IdentifierResolution unresolved() {
        IdentifierResolution res;
        res.as.unresolved = Unresolved();
        return res;
    }

    static IdentifierResolution local(AST::IdentifierBinding *NONNULL binding) {
        IdentifierResolution res;
        res.as.local = Local(binding);
        return res;
    }

    static IdentifierResolution global(AST::IdentifierBinding *NONNULL binding, bool isExtern) {
        IdentifierResolution res;
        res.as.global = Global(binding, isExtern);
        return res;
    }

    static IdentifierResolution function(AST::FunctionDeclaration *NONNULL function) {
        IdentifierResolution res;
        res.as.function = Function(function);
        return res;
    }

    static IdentifierResolution parameter(AST::FunctionDeclaration *NONNULL function, int parameterIndex) {
        IdentifierResolution res;
        res.as.parameter = Parameter(function, parameterIndex);
        return res;
    }

    static IdentifierResolution type(Type *NONNULL type) {
        IdentifierResolution res;
        res.as.type = TypeIdentifier(type);
        return res;
    }
};

static_assert(sizeof(IdentifierResolution) <= 16);

#endif // LANG_resolution_identifier_h
