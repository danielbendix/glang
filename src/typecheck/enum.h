#ifndef LANG_typecheck_enum_h
#define LANG_typecheck_enum_h

#include "typecheck/resolver.h"

#include "namespace.h"
#include "common.h"
#include "type/enum.h"

PassResult populateCasesInEnumType(EnumType& enumType, TypeResolver& typeResolver);

#endif // LANG_typecheck_enum_h
