#include "enum.h"
#include "context.h"
#include "diagnostic.h"

const auto valueSymbol = LazySymbol{"value"};

std::pair<MemberResolution, Type *> EnumType::resolveMember(const Symbol& name, AST::Node& node) {
    if (name == valueSymbol.get()) {
        if (rawType) {
            return {MemberResolution::enumValue(), rawType};
        } else {
            Diagnostic::error(node, "Cannot get value from enum without underlying type.");
            return {};
        }
    }

    // TODO: Look up methods.

    Diagnostic::error(node, "Enum members beside value not implemented");

    return {};
}


std::pair<MemberResolution, Type *> EnumType::resolveStaticMember(const Symbol& name, AST::Node& node) {
    if (auto caseIndex = caseMap.lookup(name)) {
        return {MemberResolution::enumCase(*caseIndex), this};
    }

    // TODO: Lookup static members.
    
    Diagnostic::error(node, "Enum members beside value not implemented");

    return {};
}
