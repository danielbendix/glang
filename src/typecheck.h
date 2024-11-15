#ifndef LANG_typecheck_h
#define LANG_typecheck_h

#include "common.h"

#include "AST.h"
#include "namespace.h"
#include "diagnostic.h"

#include "containers/string_map.h"

PassResult typecheckModule(Module& module);

#endif // LANG_typecheck_h
