#include "common.h"

#include "type.h"
#include "AST.h"

#include "llvm/ADT/PointerUnion.h"

#include "containers/string_map.h"

#include "resolution/member.h"

using llvm::PointerUnion;

class StructType : public Type {
    using Property = PointerUnion<AST::VariableDeclaration *, AST::FunctionDeclaration *>;

    bool wellFormed;

    std::string name;

    StringMap<Property> properties;

    std::vector<AST::VariableDeclaration *> fields;
    std::vector<AST::FunctionDeclaration *> methods;

    StructType(const std::string& name, bool wellFormed, StringMap<Property>&& properties, std::vector<AST::VariableDeclaration *>&& fields, std::vector<AST::FunctionDeclaration *>&& methods) 
        : Type{TK_Struct}
        , wellFormed{wellFormed}
        , name{name}
        , properties{std::move(properties)}
        , fields{std::move(fields)}
        , methods{std::move(methods)} {}
public:
    static unique_ptr_t<StructType> create(const std::string& name, bool wellFormed,StringMap<Property>&& properties, std::vector<AST::VariableDeclaration *>&& fields, std::vector<AST::FunctionDeclaration *>&& methods) {
        return unique_ptr_t<StructType>{new StructType(name, wellFormed, std::move(properties), std::move(fields), std::move(methods))};
    }

    std::pair<unique_ptr_t<MemberResolution>, Type *> resolveMember(const std::string& name);
    std::pair<unique_ptr_t<MemberResolution>, Type *> resolveStaticMember(const std::string& name);

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
