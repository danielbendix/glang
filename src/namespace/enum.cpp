#include "enum.h"

#include "AST_Visitor.h"

/**
 * TODO:
 * - Allow declarations in enum declarations, among them:
 *   - Functions
 *   - Static variables.
 */

EnumType *createEnumType(AST::EnumDeclaration& declaration, u32 file) 
{
    return EnumType::create(declaration.getName(), declaration, file);
}
