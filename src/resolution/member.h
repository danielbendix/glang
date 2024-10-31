#ifndef LANG_resolution_member_h
#define LANG_resolution_member_h

#include "resolution.h"

struct MemberResolution {

    enum class Kind : uint8_t {
        UNRESOLVED,
        StructField,
        StructMethod,
        EnumCase,
    };

    struct Unresolved {
        Kind kind = Kind::UNRESOLVED;
    };

    struct StructField {
        Kind kind = Kind::StructField;
        uint32_t index;

        StructField(uint32_t index) : index{index} {}
    };

    struct StructMethod {
        Kind kind = Kind::StructMethod;
        uint32_t index;

        StructMethod(uint32_t index) : index{index} {}
    };

    struct EnumCase {
        Kind kind = Kind::EnumCase;
        uint32_t index;

        EnumCase(uint32_t index) : index{index} {}
    };

    union AS {
        Unresolved unresolved;
        StructField structField;
        StructMethod structMethod;
        EnumCase enumCase;
    } as;

    Kind getKind() const {
        return as.unresolved.kind;
    }

    operator bool() const {
        return getKind() != Kind::UNRESOLVED;
    }

    MemberResolution() : as{Unresolved()} {}
    MemberResolution(StructField structField) : as{.structField = structField} {}
    MemberResolution(StructMethod structMethod) : as{.structMethod = structMethod} {}
    MemberResolution(EnumCase enumCase) : as{.enumCase = enumCase} {}

    static MemberResolution structField(uint32_t index) {
        return StructField(index);
    }

    static MemberResolution structMethod(uint32_t index) {
        return StructMethod(index);
    }

    static MemberResolution enumCase(uint32_t index) {
        return EnumCase(index);
    }
};

static_assert(sizeof(MemberResolution) <= 8);

#endif // LANG_resolution_member_h
