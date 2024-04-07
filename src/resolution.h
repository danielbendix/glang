#ifndef LANG_resolution_h
#define LANG_resolution_h

#include "common.h"

#include <memory>


namespace AST {
    class Declaration;
    class VariableDeclaration;
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
    };
private:
    Kind kind;
protected:
    IdentifierResolution(Kind kind) : kind{kind} {}
public:
    Kind getKind() const {
        return kind;
    }

    static void deleteValue(IdentifierResolution *value) {

    }
};

class LocalResolution : public IdentifierResolution {
    AST::VariableDeclaration *variable;

    LocalResolution(AST::VariableDeclaration *variable) : IdentifierResolution{IRK_Local}, variable{variable} {}
public:
    static unique_ptr_t<LocalResolution> create(AST::VariableDeclaration& variable) {
        return unique_ptr_t<LocalResolution>{new LocalResolution(&variable)};
    }
    
    AST::VariableDeclaration& getVariableDeclaration() const {
        return *variable;
    }

    static bool classof(const IdentifierResolution *resolution) {
        return resolution->getKind() == IRK_Local;
    }
};

class GlobalResolution : public IdentifierResolution {
    bool isExtern;
    AST::VariableDeclaration *variable;

    GlobalResolution(AST::VariableDeclaration *variable, bool isExtern) : IdentifierResolution{IRK_Global}, variable{variable}, isExtern{isExtern} {}
public:
    static unique_ptr_t<GlobalResolution> create(AST::VariableDeclaration& variable, bool isExtern) {
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



class MemberResolution {
public:
    enum Kind {
        MRK_Member,
    };
private:
    Kind kind;

    int index;
protected:
    MemberResolution(Kind kind, int index) : kind{kind}, index{index} {}
public:
    Kind getKind() const {
        return kind;
    }

    static void deleteValue(IdentifierResolution *value) {
        

    }
};


#endif // LANG_resolution_h
