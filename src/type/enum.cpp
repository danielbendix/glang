#include "enum.h"

std::pair<unique_ptr_t<MemberResolution>, Type *> EnumType::resolveMember(const std::string& name) {

    // Look up methods.

    assert(false);

    return {};
}


std::pair<unique_ptr_t<MemberResolution>, Type *> EnumType::resolveStaticMember(const std::string& name) {
    if (auto caseIndex = caseMap.lookup(name)) {
        Case& enumCase = cases[*caseIndex];
        return {EnumCaseResolution::create(this, *caseIndex), enumCase.type};
    }

    // Lookup static members.
    
    assert(false);

    return {};
}
