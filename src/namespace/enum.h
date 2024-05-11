#ifndef LANG_namespace_enum_h
#define LANG_namespace_enum_h

#include "common.h"
#include "AST.h"
#include "diagnostic.h"

#include "namespace.h"

#include "type/enum.h"

unique_ptr_t<EnumType> resolveEnumType(AST::EnumDeclaration& enumDeclaration);

#endif // LANG_namespace_enum_h
