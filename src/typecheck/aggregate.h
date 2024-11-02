#ifndef LANG_typecheck_aggregate_h
#define LANG_typecheck_aggregate_h

#include "common.h"
#include "typecheck/resolver.h"

#include "AST.h"
#include "namespace.h"
#include "diagnostic.h"

#include "containers/string_map.h"

PassResult typeCheckStructs(std::vector<StructType *NONNULL>& structTypes, ModuleDef& moduleDefinition, TypeResolver& typeResolver);

#endif // LANG_typecheck_aggregate_h
