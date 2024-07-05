#include "enum.h"

#include "containers/string_map.h"

#include <bit>

using Result = PassResult;
using enum PassResultKind;

PassResult populateCasesInEnumType(EnumType& enumType, TypeResolver& typeResolver)
{
    StringMap<size_t> indices;
    std::vector<EnumType::Case> cases;

    AST::EnumDeclaration& declaration = *enumType.getDeclaration();

    const size_t numberOfCases = declaration.getNumberOfCases();

    const int bitWidth = std::bit_width(numberOfCases);
    bool compressedNil = !std::has_single_bit(numberOfCases);
    Result result = OK;

    const size_t startingTag = compressedNil ? 1 : 0;

    if (numberOfCases > 0) {
        std::vector<EnumType::Case> cases;
        StringMap<size_t> caseMap;

        cases.reserve(numberOfCases);

        size_t currentTag = startingTag;

        for (auto& enumCase : declaration) {
            size_t index = cases.size();
            size_t tag = currentTag++;

            if (enumCase.hasMembers()) {
                std::vector<EnumType::Case::Member> members;
                std::vector<Type *> memberTypes;
                members.reserve(enumCase.getMemberCount());
                memberTypes.reserve(enumCase.getMemberCount());
                for (auto& member : enumCase) {
                    auto type = typeResolver.resolveType(member.getType());
                    if (!type) {
                        result = ERROR;
                    }

                    members.emplace_back(member.getName(), type);
                    memberTypes.push_back(type);
                }

                FunctionType *functionType = new FunctionType{&enumType, std::move(memberTypes)};
                
                cases.emplace_back(tag, enumCase.getName(), functionType, std::move(members));
            } else {
                cases.emplace_back(tag, enumCase.getName(), &enumType);
            }

            if (!caseMap.insert(enumCase.getName(), index)) {
                // TODO: Make cases locatable.
                Diagnostic::error(declaration, "Duplicate case name in enum.");
                result = ERROR;
            }
        }

        enumType.setCases(
            bitWidth, 
            std::make_pair(startingTag, currentTag - 1), 
            std::move(cases), 
            std::move(caseMap)
        );
    } else {
        enumType.setEmpty();
    }

    return result;
}
