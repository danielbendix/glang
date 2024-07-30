#ifndef LANG_typeconstraint_h
#define LANG_typeconstraint_h

#include "common.h"

enum class TypeConstraintKind : uint8_t {
    Numeric,
    Floating,
    Optional,
};

class alignas(8) TypeConstraint {
protected:
    TypeConstraintKind kind;

public:

    TypeConstraint(TypeConstraintKind kind) : kind{kind} {}

    TypeConstraintKind getKind() const {
        return kind;
    }

    static TypeConstraint *Optional; // = TypeConstraint(TypeConstraintKind::Optional);
    static TypeConstraint *Numeric;
    static TypeConstraint *Floating;
};

bool isIntegralConstraint(TypeConstraint *constraint);
bool isNumericConstraint(TypeConstraint *constraint);

#endif // LANG_typeconstraint_h
