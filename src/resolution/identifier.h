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

struct Function;

struct IdentifierResolution {
    enum class Kind : u8 {
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
        u32 bindingIndex;
        AST::IdentifierBinding *NONNULL binding;

        Global(u32 bindingIndex, AST::IdentifierBinding *NONNULL binding, bool isExtern) 
            : bindingIndex{bindingIndex}, binding{binding}, isExtern{isExtern} {}
    };

    struct Parameter {
        Kind kind = Kind::Parameter;
        u16 index;
        Function *NONNULL function;

        Parameter(Function *NONNULL function, u16 parameterIndex) : function{function}, index{parameterIndex} {}
    };

    struct Function_ {
        Kind kind = Kind::Function;
        u32 index;
        Function *NONNULL function;

        Function_(Function *NONNULL function, u32 index) : function{function}, index{index} {}
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
        Function_ function;
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

    static IdentifierResolution global(u32 bindingIndex, AST::IdentifierBinding *NONNULL binding, bool isExtern) {
        IdentifierResolution res;
        res.as.global = Global(bindingIndex, binding, isExtern);
        return res;
    }

    static IdentifierResolution function(Function *NONNULL function, u32 index) {
        IdentifierResolution res;
        res.as.function = Function_(function, index);
        return res;
    }

    static IdentifierResolution parameter(Function *NONNULL function, int parameterIndex) {
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
