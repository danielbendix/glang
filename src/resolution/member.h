#ifndef LANG_resolution_member_h
#define LANG_resolution_member_h

#include "resolution.h"

struct MemberResolution {

    enum class Kind : u8 {
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
        u32 index;

        StructField(u32 index) : index{index} {}
    };

    struct StructMethod {
        Kind kind = Kind::StructMethod;
        u32 index;

        StructMethod(u32 index) : index{index} {}
    };

    struct EnumCase {
        Kind kind = Kind::EnumCase;
        u32 index;

        EnumCase(u32 index) : index{index} {}
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

    static MemberResolution structField(u32 index) {
        return StructField(index);
    }

    static MemberResolution structMethod(u32 index) {
        return StructMethod(index);
    }

    static MemberResolution enumCase(u32 index) {
        return EnumCase(index);
    }
};

static_assert(sizeof(MemberResolution) <= 8);

#endif // LANG_resolution_member_h
