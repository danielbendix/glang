#include "struct.h"

#include "resolution/member.h"

#include "llvm/Support/Casting.h"

std::pair<unique_ptr_t<MemberResolution>, Type *> StructType::resolveMember(const std::string& name) 
{
    if (auto property = properties.lookup(name)) {
        if (auto field = llvm::dyn_cast<AST::VariableDeclaration *>(*property)) {
            int index = std::find(fields.begin(), fields.end(), field) - fields.begin();
            return {StructFieldResolution::create(*field, index), field->getType()};

        }
        if (auto method = llvm::dyn_cast<AST::FunctionDeclaration *>(*property)) {
            return {StructMethodResolution::create(*method), method->getType()};
        }
        llvm_unreachable("Unsupported property type in struct.");
    }
    return {nullptr, nullptr};
}

std::pair<unique_ptr_t<MemberResolution>, Type *> StructType::resolveStaticMember(const std::string& name)
{
    assert(false);

    return {nullptr, nullptr};
}

llvm::StructType *StructType::getStructType(llvm::LLVMContext& context) 
{
    llvm::Type *children[fields.size()];

    for (size_t i = 0; i < fields.size(); i++) {
        children[i] = fields[i]->getType()->getLLVMType(context);
        if (!children[i]) {
            return nullptr;
        }
    }

    return llvm::StructType::create(context, {children, fields.size()});
}
