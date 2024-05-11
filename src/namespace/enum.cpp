#include "enum.h"

#include "AST_Visitor.h"

/**
 * TODO:
 * - Allow declarations in enum declarations, among them:
 *   - Functions
 *   - Static variables.
 */

class EnumChildVisitor : public AST::DeclarationVisitorT<EnumChildVisitor, PassResult> {


};

unique_ptr_t<EnumType> resolveEnumType(AST::EnumDeclaration& declaration) 
{
    unique_ptr_t<EnumType> type = unique_ptr_t<EnumType>{new EnumType{declaration.getName(), &declaration}};

    return type;
}
