#ifndef LANG_builtins_h
#define LANG_builtins_h

#include "type.h"
#include "intrinsic.h"
#include "target/architecture.h"
#include "containers/symbol_map.h"

void setupBuiltins(SymbolTable& symbols, const Architecture& architecture);

class Builtins {
public:
    VoidType *voidType;
    IntegerType *defaultIntegerType;
    BooleanType *booleanType;
    FPType *defaultFPType;

    SymbolMap<Type *> types;
    SymbolMap<IntrinsicKind> intrinsics;

    friend void setupBuiltins(SymbolTable& symbols, const Architecture& architecture);
};

extern const Builtins& builtins;

#endif // LANG_builtins_h
