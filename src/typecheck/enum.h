#ifndef LANG_typecheck_enum_h
#define LANG_typecheck_enum_h

#include "typecheck/resolver.h"

#include "namespace.h"
#include "common.h"
#include "type/enum.h"

PassResult typeCheckEnums(std::vector<EnumType *>& enums,
                          std::vector<AST::EnumDeclaration *>& declarations,
                          Module& module,
                          TypeResolver& typeResolver);

PassResult typeCheckEnumType(EnumType& type, AST::EnumDeclaration& declaration, TypeResolver& typeResolver);

#endif // LANG_typecheck_enum_h
