#ifndef LANG_resolution_member_h
#define LANG_resolution_member_h

#include "resolution.h"

namespace AST {
    class VariableDeclaration;
    class FunctionDeclaration;
};

class MemberResolution {
public:
    enum Kind {
        MRK_Struct_Field,
        MRK_Struct_Method,
    };
private:
    Kind kind;
protected:
    MemberResolution(Kind kind) : kind{kind} {}
public:
    Kind getKind() const {
        return kind;
    }

    static void deleteValue(MemberResolution *value);
};

class StructFieldResolution : public MemberResolution {
    int index;
    AST::VariableDeclaration *variable;

    StructFieldResolution(AST::VariableDeclaration& variable, int index) : MemberResolution{MRK_Struct_Field}, variable{&variable}, index{index} {}
public:
    static unique_ptr_t<StructFieldResolution> create(AST::VariableDeclaration& variable, int index) {
        return unique_ptr_t<StructFieldResolution>{new StructFieldResolution(variable, index)};
    }

    int getIndex() const {
        return index;
    }

    static bool classof(const MemberResolution *resolution) {
        return resolution->getKind() == MRK_Struct_Field;
    }
};

class StructMethodResolution : public MemberResolution {
    AST::FunctionDeclaration *method;

    StructMethodResolution(AST::FunctionDeclaration& method) : MemberResolution{MRK_Struct_Method}, method{&method} {}
public:
    static unique_ptr_t<StructMethodResolution> create(AST::FunctionDeclaration& method) {
        return unique_ptr_t<StructMethodResolution>{new StructMethodResolution(method)};
    }

    static bool classof(const MemberResolution *resolution) {
        return resolution->getKind() == MRK_Struct_Method;
    }
};

#endif // LANG_resolution_member_h
