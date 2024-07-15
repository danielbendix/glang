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

class IdentifierResolution {
public:
    enum Kind {
        IRK_Global,
        IRK_Function,
        IRK_Parameter,
        IRK_Local,
        IRK_Type,
    };
private:
    Kind kind;
protected:
    IdentifierResolution(Kind kind) : kind{kind} {}
public:
    Kind getKind() const {
        return kind;
    }

    static void deleteValue(IdentifierResolution *value);
};


class LocalResolution : public IdentifierResolution {
    AST::IdentifierBinding *binding;

    LocalResolution(AST::IdentifierBinding *binding) : IdentifierResolution{IRK_Local}, binding{binding} {}
public:
    static unique_ptr_t<LocalResolution> create(AST::IdentifierBinding& binding) {
        return unique_ptr_t<LocalResolution>{new LocalResolution(&binding)};
    }
    
    AST::IdentifierBinding& getBinding() const {
        return *binding;
    }

    static bool classof(const IdentifierResolution *resolution) {
        return resolution->getKind() == IRK_Local;
    }
};

class GlobalResolution : public IdentifierResolution {
    bool isExtern;
    AST::IdentifierBinding *variable;

    GlobalResolution(AST::IdentifierBinding *variable, bool isExtern) : IdentifierResolution{IRK_Global}, variable{variable}, isExtern{isExtern} {}
public:
    static unique_ptr_t<GlobalResolution> create(AST::IdentifierBinding& variable, bool isExtern) {
        return unique_ptr_t<GlobalResolution>{new GlobalResolution(&variable, isExtern)};
    }
    static bool classof(const IdentifierResolution *resolution) {
        return resolution->getKind() == IRK_Global;
    }
};

class FunctionResolution : public IdentifierResolution {
    AST::FunctionDeclaration *function;

    FunctionResolution(AST::FunctionDeclaration *function) : IdentifierResolution{IRK_Function}, function{function} {}
public:
    static unique_ptr_t<FunctionResolution> create(AST::FunctionDeclaration& function) {
        return unique_ptr_t<FunctionResolution>{new FunctionResolution(&function)};
    }

    AST::FunctionDeclaration *getFunctionDeclaration() {
        return function;
    }

    static bool classof(const IdentifierResolution *resolution) {
        return resolution->getKind() == IRK_Function;
    }
};

class FunctionParameterResolution : public IdentifierResolution {
    int parameterIndex;
    AST::FunctionDeclaration *function;

    FunctionParameterResolution(AST::FunctionDeclaration *function, int parameterIndex) : IdentifierResolution{IRK_Parameter}, function{function}, parameterIndex{parameterIndex} {}
public:
    static unique_ptr_t<FunctionParameterResolution> create(AST::FunctionDeclaration *function, int parameterIndex) {
        return unique_ptr_t<FunctionParameterResolution>{new FunctionParameterResolution(function, parameterIndex)};
    }

    AST::FunctionDeclaration *getFunctionDeclaration() const {
        return function;
    }

    int getParameterIndex() const {
        return parameterIndex;
    }

    static bool classof(const IdentifierResolution *resolution) {
        return resolution->getKind() == IRK_Parameter;
    }
};

class IdentifierTypeResolution : public IdentifierResolution {
    Type *type;

    IdentifierTypeResolution(Type *type) : IdentifierResolution{IRK_Type}, type{type} {}
public:
    static unique_ptr_t<IdentifierTypeResolution> create(Type *type) {
        return unique_ptr_t<IdentifierTypeResolution>{ new IdentifierTypeResolution(type)};
    }
};

#endif // LANG_resolution_identifier_h
