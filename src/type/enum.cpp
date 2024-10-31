#include "enum.h"

std::pair<MemberResolution, Type *> EnumType::resolveMember(const Symbol& name) {

    // Look up methods.

    assert(false);

    return {};
}


std::pair<MemberResolution, Type *> EnumType::resolveStaticMember(const Symbol& name) {
    if (auto caseIndex = caseMap.lookup(name)) {
        Case& enumCase = cases[*caseIndex];
        return {MemberResolution::enumCase(*caseIndex), enumCase.type};
    }

    // Lookup static members.
    
    assert(false);

    return {};
}
