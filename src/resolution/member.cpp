#include "member.h"

void MemberResolution::deleteValue(MemberResolution *value) {
    switch (value->getKind()) {
        case MRK_Struct_Field: return delete static_cast<StructFieldResolution *>(value);
        case MRK_Struct_Method: return delete static_cast<StructMethodResolution *>(value);
        case MRK_Enum_Kind: return delete static_cast<EnumCaseResolution *>(value);
    }
}
