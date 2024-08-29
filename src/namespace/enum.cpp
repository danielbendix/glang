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

EnumType *resolveEnumType(AST::EnumDeclaration& declaration) 
{
    return EnumType::create(declaration.getName(), declaration);
}
