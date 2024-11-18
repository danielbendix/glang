#ifndef LANG_type_struct_h
#define LANG_type_struct_h

#include "common.h"

#include "type.h"
#include "layout.h"
#include "AST.h"

#include "llvm/ADT/PointerUnion.h"

#include "containers/bitmap.h"
#include "containers/symbol_map.h"

#include "resolution/member.h"

using llvm::PointerUnion;

class StructType : public Type {

    using Property = PointerUnion<AST::VariableDeclaration *, AST::FunctionDeclaration *>;
public:
    bool typeChecked = false;
    // False if the struct:
    // - Contained invalid nested declaration types.
    // - Contained duplicate declaration names.
    const bool wellFormed;
    const bool isCompact;
    const bool isUnpadded;
    u32 fieldCount;
private:

    struct Field {
        Type *type;
        Symbol *name;
        u32 offset;
        Align alignment;
    };

    Layout layout = {0, 0};
    const Symbol& name;

    SymbolMap<Property> properties;

    llvm::Type *packType = nullptr;
    std::vector<AST::VariableDeclaration *> fields;
    // TODO: Put into collective array with fields.
    std::vector<u32> offsets = {};
    std::vector<AST::FunctionDeclaration *> methods;

    union {
        u64 single;
        u64 *multiple;
    } initializedFields;

    StructType(
        const Symbol& name, 
        bool wellFormed, 
        bool isCompact,
        bool isUnpadded,
        SymbolMap<Property>&& properties, 
        std::vector<AST::VariableDeclaration *>&& fields, 
        std::vector<AST::FunctionDeclaration *>&& methods
    ) 
        : Type{TK_Struct}
        , wellFormed{wellFormed}
        , isCompact{isCompact}
        , isUnpadded{isUnpadded}
        , name{name}
        , properties{std::move(properties)}
        , fields{std::move(fields)}
        , methods{std::move(methods)}
        , initializedFields{0} {
        }
public:

    ~StructType() {
        if (fields.size() > 64) {
            delete[] initializedFields.multiple;
        }
    }

    static StructType *NONNULL create(
        const Symbol& name, 
        bool wellFormed, 
        bool isCompact,
        bool isUnpadded,
        SymbolMap<Property>&& properties, 
        std::vector<AST::VariableDeclaration *>&& fields, 
        std::vector<AST::FunctionDeclaration *>&& methods
    ) 
    {
        return allocate(typeAllocator(), [&](void *space) {
            return new (space) StructType{name, wellFormed, isCompact, isUnpadded, std::move(properties), std::move(fields), std::move(methods)};
        });
    }

    void getName(std::string& result) const {
        result.append(name.string_view());
    }

    using MemberType = llvm::PointerIntPair<Type *, 1, bool>;

    std::pair<MemberResolution, MemberType> resolveMember(const Symbol& name);
    std::pair<MemberResolution, MemberType> resolveStaticMember(const Symbol& name);

    llvm::StructType *getStructType(llvm::LLVMContext& context) const;

    const std::vector<AST::VariableDeclaration *>& getFields() const {
        return fields;
    }

    const std::vector<AST::FunctionDeclaration *>& getMethods() const {
        return methods;
    }

    const u64 *getInitializedFields() const {
        if (fields.size() <= 64) {
            return &initializedFields.single;
        } else {
            return initializedFields.multiple;
        }
    }

    void setInitializedFields(const Bitmap& initializedFields) {
        assert(initializedFields.count() == fields.size());

        if (initializedFields.count() > 64) {
            auto& allocator = typeAllocator();
            this->initializedFields.multiple = (u64 *) allocator.allocate(sizeof(u64) * initializedFields.count() / 64, alignof(u64));
            initializedFields.copyInto(this->initializedFields.multiple);
        } else {
            initializedFields.copyInto(&this->initializedFields.single);
        }
    }

    llvm::Type *getPackType() const {
        return packType;
    }

    void setPackType(llvm::Type *packType) {
        this->packType = packType;
    }

    static bool classof(const Type *type) {
        return type->getKind() == TK_Struct;
    }

    friend class Type;
    friend class AggregateTypeChecker;
};

#endif // LANG_type_struct_h
