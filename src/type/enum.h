#ifndef LANG_type_enum_h
#define LANG_type_enum_h

#include "type.h"
#include "AST.h"

class EnumType : public Type {
public:
    class Case {
    public:
        class Member {
            std::string name;
            Type *type;
        public:
            Member(const std::string& name, Type *type) : name{name}, type{type} {}
        };

        Case(size_t tag, const std::string& name, Type *type) : tag{tag}, name{name}, type{type} {}

        Case(size_t tag, const std::string& name, Type *type, std::vector<Member>&& members) : tag{tag}, name{name}, members{std::move(members)}, type{type} {}

    private:
        size_t tag;
        std::string name;
        std::vector<Member> members;
        Type *type;

        friend class EnumType;
    };
private:

    AST::EnumDeclaration *declaration;

    bool wellFormed = true;
    std::string name;

    size_t minTag = -1;
    size_t maxTag = -1;

    uint8_t bits = 0;

    std::vector<Case> cases;
    StringMap<size_t> caseMap;

public:
    void *codegen;

public:
    EnumType(const std::string& name, AST::EnumDeclaration *enumDeclaration) : Type{TK_Enum}, name{name}, declaration{enumDeclaration} {}

    std::pair<unique_ptr_t<MemberResolution>, Type *> resolveMember(const std::string& name);
    std::pair<unique_ptr_t<MemberResolution>, Type *> resolveStaticMember(const std::string& name);

    AST::EnumDeclaration *getDeclaration() const {
        return declaration;
    }

    size_t getNumberOfCases() const {
        return cases.size();
    }

    void setEmpty() {

    }

    void setCases(uint8_t bits, std::pair<size_t, size_t> tags, std::vector<Case>&& cases, StringMap<size_t>&& caseMap) {
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
