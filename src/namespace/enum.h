#ifndef LANG_namespace_enum_h
#define LANG_namespace_enum_h

#include "common.h"
#include "AST.h"
#include "diagnostic.h"

#include "namespace.h"

#include "type/enum.h"

EnumType *NONNULL createEnumType(AST::EnumDeclaration& enumDeclaration, FileID file);

#endif // LANG_namespace_enum_h
