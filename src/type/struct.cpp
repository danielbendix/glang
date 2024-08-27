#include "struct.h"

#include "resolution/member.h"

#include "llvm/Support/Casting.h"

std::pair<unique_ptr_t<MemberResolution>, StructType::MemberType> StructType::resolveMember(const Symbol& name) 
{
    if (auto property = properties.lookup(name)) {
        if (auto field = llvm::dyn_cast<AST::VariableDeclaration *>(*property)) {
            int index = std::find(fields.begin(), fields.end(), field) - fields.begin();
            MemberType memberType{field->getType(), field->getIsMutable()};
            return {StructFieldResolution::create(*field, index), memberType};

        }
        if (auto method = llvm::dyn_cast<AST::FunctionDeclaration *>(*property)) {
            MemberType memberType{method->getType(), false};
            return {StructMethodResolution::create(*method), memberType};
        }
        llvm_unreachable("Unsupported property type in struct.");
    }
    return {nullptr, MemberType{nullptr}};
}

std::pair<unique_ptr_t<MemberResolution>, StructType::MemberType> StructType::resolveStaticMember(const Symbol& name)
{
    assert(false);

    return {nullptr, MemberType{nullptr}};
}

llvm::StructType *StructType::getStructType(llvm::LLVMContext& context) const
{
    llvm::Type *children[fields.size()];

    for (size_t i = 0; i < fields.size(); i++) {
        children[i] = fields[i]->getType()->getLLVMType(context);
        if (!children[i]) {
            return nullptr;
        }
    }

    return llvm::StructType::create(context, {children, fields.size()}, name.string_view());
}
