#include "enum.h"
#include "expression.h"

#include "containers/string_map.h"

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallString.h"

#include <bit>

using llvm::dyn_cast;

using Result = PassResult;
using enum PassResultKind;

enum class Test {
    a = 0x7FFFFFFF,
};

PassResult typeCheckEnumWithoutRawType(EnumType& type, AST::EnumDeclaration& declaration, TypeResolver& typeResolver)
{
    u32 numberOfCases = declaration.getNumberOfCases();

    u32 bitWidth = std::bit_width(numberOfCases);

    llvm::APInt tag = {bitWidth, u64(0)};

    std::vector<EnumType::Case> cases = {};
    cases.reserve(numberOfCases);
    SymbolMap<EnumType::CaseID> caseMap = {};
    caseMap.reserve(numberOfCases);

    for (const auto& case_ : declaration) {
        if (auto canonical = caseMap.lookup(case_.name)) {
            Diagnostic::error({case_.offset, 4}, "Duplicate case name '" + case_.name.string() + "' in enum.");

            auto const& astCase = declaration.getCase(*canonical);
            AST::FileLocation location = {astCase.offset, 4};
            FileID fileID = ThreadContext::get()->currentFile;
            Diagnostic::note(location, "Canonical declaration of case with name `" + case_.name.string() + "` declared here.", fileID, fileID, case_.offset);
            return ERROR;
        }

        if (case_.value != nullptr) {
            Diagnostic::error(*case_.value, "Case in enum with no raw type cannot have a specified value.");
            return ERROR;
        }

        caseMap.insert(case_.name, EnumType::CaseID(cases.size()));
        cases.emplace_back(tag, case_.name);

        tag += 1;
    }

    type.setCases(bitWidth, std::move(cases), std::move(caseMap));

    return OK;
}

std::string stringFromAPInt(llvm::APInt& integer) {
    llvm::SmallString<32> string;
    integer.toStringSigned(string);
    return (std::string) string;
}


PassResult typeCheckEnumWithRawType(EnumType& type, AST::EnumDeclaration& declaration, TypeResolver& typeResolver)
{
    u32 numberOfCases = declaration.getNumberOfCases();
    IntegerType& rawType = *type.getRawType();
    assert(&rawType);

    llvm::APInt tag = {rawType.bitWidth, 0};

    std::vector<EnumType::Case> cases = {};
    cases.reserve(numberOfCases);
    SymbolMap<EnumType::CaseID> caseMap = {};
    caseMap.reserve(numberOfCases);
    llvm::DenseSet<llvm::APInt> seenValues;
    seenValues.reserve(numberOfCases);

    auto findCase = [&](llvm::APInt& tag) {
        for (u32 i = 0; i < cases.size(); ++i) {
            auto& previousCase = cases[i];

            if (previousCase.tag == tag) {
                return declaration.getCase(i);
            }
        }
        llvm_unreachable("[Consistency error]: Did not find duplicate case.");
    };

    for (const auto& case_ : declaration) {
        if (auto canonical = caseMap.lookup(case_.name)) {
            Diagnostic::error({case_.offset, 4}, "Duplicate case name '" + case_.name.string() + "' in enum.");

            auto const& astCase = declaration.getCase(*canonical);
            AST::FileLocation location = {astCase.offset, 4};
            FileID fileID = ThreadContext::get()->currentFile;
            Diagnostic::note(location, "Canonical declaration of case with name `" + case_.name.string() + "` declared here.", fileID, fileID, case_.offset);
            return ERROR;
        }

        if (auto *value = case_.value) {
            // TODO: Add an untyped fold here to allow expressions.
            if (auto *literal = dyn_cast<AST::IntegerLiteral>(value)) {
                tag  = literal->getValue();

            } else {
                Diagnostic::error(*value, "Enum case value must be an integer literal.");
                return ERROR;
            }
        }

        if (seenValues.contains(tag)) {
            auto tagString = stringFromAPInt(tag);
            Diagnostic::error({case_.offset, 4}, "Duplicate tag with value `" + tagString + "` in enum type.");
            auto const& astCase = findCase(tag);
            AST::FileLocation location = {astCase.offset, 4};
            FileID fileID = ThreadContext::get()->currentFile;
            Diagnostic::note(location, "Canonical declaration of case with value `" + tagString + "` declared here.", fileID, fileID, case_.offset);

            return ERROR;
        }

        caseMap.insert(case_.name, EnumType::CaseID(cases.size()));
        cases.emplace_back(tag, case_.name);
        seenValues.insert(tag);

        tag += 1;
    }

    type.setCases(rawType.bitWidth, std::move(cases), std::move(caseMap));

    return OK;
}

PassResult typeCheckEnumType(EnumType& type, AST::EnumDeclaration& declaration, TypeResolver& typeResolver)
{
    if (auto *typeNode = declaration.getTypeAnnotation()) {
        if (auto *rawType = typeResolver.resolveType(*typeNode)) {
            if (auto *integerType = dyn_cast<IntegerType>(rawType)) {
                type.setRawType(integerType);
                return typeCheckEnumWithRawType(type, declaration, typeResolver);
            } else {
                Diagnostic::error(*typeNode, "Enums can only have integer types as their underlying type.");
                return ERROR;
            }
        } else {
            return ERROR;
        }
    } else {
        return typeCheckEnumWithoutRawType(type, declaration, typeResolver);
    }
}

//class EnumTypeChecker {
//    Module& module;
//    TypeResolver& typeResolver;
//
//    EnumTypeChecker(Module& module, TypeResolver& typeResolver)
//        : module{module}, typeResolver{typeResolver} {}
//
//    PassResult typeCheckEnum(EnumType& type, AST::EnumDeclaration& declaration) {
//        IntegerType *underlyingType = nullptr;
//        if (auto *typeNode = declaration.getTypeAnnotation()) {
//            if (auto *type = typeResolver.resolveType(*typeNode)) {
//                if (auto *integerType = dyn_cast<IntegerType>(type)) {
//                    underlyingType = integerType;
//                } else {
//                    Diagnostic::error(*typeNode, "Enums can only have integer types as their underlying type.");
//                    return ERROR;
//                }
//            } else {
//                return ERROR;
//            }
//        }
//
//        for (const auto& case_ : declaration) {
//            // TODO: Typecheck expression.
//            // This may require more complex constant folding.
//
//
//
//        }
//    }
//};

PassResult typeCheckEnums(std::vector<EnumType *>& enums,
                          std::vector<AST::EnumDeclaration *>& declarations,
                          Module& module,
                          TypeResolver& typeResolver)
{
    ScopeManager scopeManager{module};
    ExpressionTypeChecker typeChecker{scopeManager, typeResolver};

    PassResult result = OK;
    for (auto [type, declaration] : llvm::zip(enums, declarations)) {
        ThreadContext::get()->withFile(type->file, [&]() {
            result |= typeCheckEnumType(*type, *declaration, typeResolver);
        });
    }

    return result;
}
