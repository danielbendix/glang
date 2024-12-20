#ifndef LANG_type_enum_h
#define LANG_type_enum_h

#include "type.h"
#include "AST.h"

#include "containers/symbol_map.h"

class EnumType : public Type {
public:
    class Case {
    public:
        class Member {
            const Symbol *name;
            Type *type;
        public:
            Member(const Symbol *name, Type *type) : name{name}, type{type} {}
        };

        Case(size_t tag, const Symbol& name, Type *type) : tag{tag}, name{name}, type{type} {}

        Case(size_t tag, const Symbol& name, Type *type, std::vector<Member>&& members) : tag{tag}, name{name}, members{std::move(members)}, type{type} {}

    private:
        size_t tag;
        const Symbol& name;
        std::vector<Member> members;
        Type *type;

        friend class EnumType;
    };
private:

    // TODO: Remove this, type check by parallel iteration with module.enumDeclarations.
    AST::EnumDeclaration *declaration;

    bool wellFormed = true;
    const Symbol& name;

    size_t minTag = -1;
    size_t maxTag = -1;

    u8 bits = 0;
public:
    const u32 file;
private:

    std::vector<Case> cases;
    SymbolMap<size_t> caseMap;

    EnumType(const Symbol& name, AST::EnumDeclaration *enumDeclaration, u32 file) 
        : Type{TK_Enum}, name{name}, declaration{enumDeclaration}, file{file} {}
public:
    void *codegen;

public:
    static EnumType *NONNULL create(const Symbol& name, AST::EnumDeclaration& declaration, u32 file) {
        return allocate(typeAllocator(), [&](void *space) {
            return new (space) EnumType{declaration.getName(), &declaration, file};
        });
    }

    void getName(std::string& result) const {
        result.append(name.string_view());
    }

    std::pair<MemberResolution, Type *> resolveMember(const Symbol& name);
    std::pair<MemberResolution, Type *> resolveStaticMember(const Symbol& name);

    AST::EnumDeclaration *getDeclaration() const {
        return declaration;
    }

    size_t getNumberOfCases() const {
        return cases.size();
    }

    void setEmpty() {

    }

    void setCases(u8 bits, std::pair<size_t, size_t> tags, std::vector<Case>&& cases, SymbolMap<size_t>&& caseMap) {
        this->bits = bits;
        this->minTag = tags.first;
        this->maxTag = tags.second;
        this->cases = std::move(cases);
        this->caseMap = std::move(caseMap);
    }

    static bool classof(const Type *type) {
        return type->getKind() == TK_Enum;
    }
};

// Resolve type names.
// Resolve cases, and associated data.
// Resolve

#endif // LANG_type_enum_h
