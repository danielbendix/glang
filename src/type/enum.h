#ifndef LANG_type_enum_h
#define LANG_type_enum_h

#include "type.h"
#include "AST.h"

#include "containers/symbol_map.h"

/**
 * An enumeration type always backed by an integral value.
 *
 *
 */

class EnumType : public Type {
public:
    using CaseID = u32;

    class Case {
    public:
        Case(llvm::APInt tag, const Symbol& name) : tag{std::move(tag)}, name{name} {}

        const llvm::APInt tag;
        const Symbol& name;

        friend class EnumType;
    };
private:
    bool wellFormed = true;
    /// Whether or not this enum allows 
    bool hasZeroValue = false;
    u16 bitWidth = ~0;
    const Symbol& name;
    /// If not present, values are not available for cases.
    IntegerType *NULLABLE rawType;

public:
    const FileID file;
private:

    std::vector<Case> cases;
    SymbolMap<CaseID> caseMap;

    EnumType(const Symbol& name, FileID file) 
        : Type{TK_Enum}, name{name}, file{file} {}
public:
    void *codegen;

public:
    static EnumType *NONNULL create(const Symbol& name, FileID file) {
        return allocate(typeAllocator(), [&](void *space) {
            return new (space) EnumType{name, file};
        });
    }

    void getName(std::string& result) const {
        result.append(name.string_view());
    }

    std::pair<MemberResolution, Type *> resolveMember(const Symbol& name, AST::Node& node);
    std::pair<MemberResolution, Type *> resolveStaticMember(const Symbol& name, AST::Node& node);

    size_t getNumberOfCases() const {
        return cases.size();
    }

    void setEmpty() {

    }

    IntegerType *getRawType() const {
        return rawType;
    }

    void setRawType(IntegerType *NONNULL rawType) {
        this->rawType = rawType;
    }

    void setCases(u16 bitWidth, std::vector<Case>&& cases, SymbolMap<CaseID>&& caseMap) {
        this->bitWidth = bitWidth;
        this->cases = std::move(cases);
        this->caseMap = std::move(caseMap);
    }

    llvm::Type *_getLLVMType(llvm::LLVMContext& context) const {
        if (rawType) {
            return rawType->_getLLVMType(context);
        } else {
            return llvm::IntegerType::get(context, bitWidth);
        }
    }

    u16 getBitWidth() const {
        return bitWidth;
    }

    llvm::APInt const& getTag(CaseID id) const {
        return cases[id].tag;
    }

    static bool classof(const Type *type) {
        return type->getKind() == TK_Enum;
    }
};

#endif // LANG_type_enum_h
