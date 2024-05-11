#ifndef LANG_namespace_struct_h
#define LANG_namespace_struct_h

#include "common.h"
#include "AST.h"
#include "diagnostic.h"

#include "namespace.h"

#include "type/struct.h"

unique_ptr_t<StructType> resolveStructType(AST::StructDeclaration& structDeclaration);

#endif // LANG_namespace_struct_h
