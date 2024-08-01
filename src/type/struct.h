#ifndef LANG_type_struct_h
#define LANG_type_struct_h

#include "common.h"

#include "type.h"
#include "AST.h"

#include "llvm/ADT/PointerUnion.h"

#include "containers/symbol_map.h"

#include "resolution/member.h"

using llvm::PointerUnion;

class StructType : public Type {

    bool test() {
        return true;
    }

    using Property = PointerUnion<AST::VariableDeclaration *, AST::FunctionDeclaration *>;

    bool wellFormed;

    const Symbol& name;

    SymbolMap<Property> properties;

    std::vector<AST::VariableDeclaration *> fields;
    std::vector<AST::FunctionDeclaration *> methods;

    StructType(const Symbol& name, bool wellFormed, SymbolMap<Property>&& properties, std::vector<AST::VariableDeclaration *>&& fields, std::vector<AST::FunctionDeclaration *>&& methods) 
        : Type{TK_Struct}
        , wellFormed{wellFormed}
        , name{name}
        , properties{std::move(properties)}
        , fields{std::move(fields)}
        , methods{std::move(methods)} {}
public:
    static unique_ptr_t<StructType> create(const Symbol& name, bool wellFormed, SymbolMap<Property>&& properties, std::vector<AST::VariableDeclaration *>&& fields, std::vector<AST::FunctionDeclaration *>&& methods) {
        return unique_ptr_t<StructType>{new StructType(name, wellFormed, std::move(properties), std::move(fields), std::move(methods))};
    }

    std::pair<unique_ptr_t<MemberResolution>, Type *> resolveMember(const Symbol& name);
    std::pair<unique_ptr_t<MemberResolution>, Type *> resolveStaticMember(const Symbol& name);

    llvm::StructType *getStructType(llvm::LLVMContext& context) const;

    const std::vector<AST::VariableDeclaration *>& getFields() const {
        return fields;
    }

    const std::vector<AST::FunctionDeclaration *>& getMethods() const {
        return methods;
    }

    static bool classof(const Type *type) {
        return type->getKind() == TK_Struct;
    }
};

#endif // LANG_type_struct_h
