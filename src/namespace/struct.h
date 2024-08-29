#ifndef LANG_namespace_struct_h
#define LANG_namespace_struct_h

#include "common.h"
#include "AST.h"
#include "diagnostic.h"

#include "namespace.h"

#include "type/struct.h"

StructType *NONNULL resolveStructType(AST::StructDeclaration& structDeclaration);

#endif // LANG_namespace_struct_h
