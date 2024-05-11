#include "typeconstraint.h"
#include "type.h"

TypeConstraint _Optional = TypeConstraint(TypeConstraintKind::Optional);
TypeConstraint *TypeConstraint::Optional = &_Optional;

TypeConstraint _Floating = TypeConstraint(TypeConstraintKind::Floating);
TypeConstraint *TypeConstraint::Floating = &_Floating;

TypeConstraint _Numeric = TypeConstraint(TypeConstraintKind::Numeric);
TypeConstraint *TypeConstraint::Numeric = &_Numeric;

bool isNumericConstraint(TypeConstraint *constraint) {
    switch (constraint->getKind()) {
        case TypeConstraintKind::Numeric:
        case TypeConstraintKind::Floating:
            return true;
        case TypeConstraintKind::Optional:
            return false;
    }
}
