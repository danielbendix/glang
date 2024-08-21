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

    using Property = PointerUnion<AST::VariableDeclaration *, AST::FunctionDeclaration *>;

    bool wellFormed;

    const Symbol& name;

    SymbolMap<Property> properties;

    std::vector<AST::VariableDeclaration *> fields;
    std::vector<AST::FunctionDeclaration *> methods;

    union {
        uint64_t single;
        uint64_t *multiple;
    } initializedFields;

    StructType(const Symbol& name, 
               bool wellFormed, 
               SymbolMap<Property>&& properties, 
               std::vector<AST::VariableDeclaration *>&& fields, 
               std::vector<AST::FunctionDeclaration *>&& methods,
               std::span<uint64_t> initializedFields) 
        : Type{TK_Struct}
        , wellFormed{wellFormed}
        , name{name}
        , properties{std::move(properties)}
        , fields{std::move(fields)}
        , methods{std::move(methods)} {
            switch (initializedFields.size()) {
                case 0:
                    this->initializedFields.single = 0;
                    break;
                case 1:
                    this->initializedFields.single = initializedFields[0];
                    break;
                default: {
                    this->initializedFields.multiple = new uint64_t[initializedFields.size()];
                    std::memcpy(this->initializedFields.multiple, initializedFields.data(), sizeof(uint64_t) * initializedFields.size());
                    break;
                }
            }

        }
public:

    ~StructType() {
        if (fields.size() > 64) {
            delete[] initializedFields.multiple;
        }
    }
    static unique_ptr_t<StructType> create(const Symbol& name, 
                                           bool wellFormed, 
                                           SymbolMap<Property>&& properties, 
                                           std::vector<AST::VariableDeclaration *>&& fields, 
                                           std::vector<AST::FunctionDeclaration *>&& methods,
                                           std::span<uint64_t> initializedFields) {
        return unique_ptr_t<StructType>{new StructType(name, wellFormed, std::move(properties), std::move(fields), std::move(methods), initializedFields)};
    }

    void getName(std::string& result) const {
        result.append(name.string_view());
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

    const uint64_t *getInitializedFields() const {
        if (fields.size() <= 64) {
            return &initializedFields.single;
        } else {
            return initializedFields.multiple;
        }
    }

    static bool classof(const Type *type) {
        return type->getKind() == TK_Struct;
    }
};

#endif // LANG_type_struct_h
