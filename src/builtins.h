#ifndef LANG_builtins_h
#define LANG_builtins_h

#include "type.h"
#include "intrinsic.h"
#include "containers/symbol_map.h"

void setupBuiltins(SymbolTable& symbols);

class Builtins {
    std::vector<Type *> all;
public:
    VoidType *voidType;
    IntegerType *defaultIntegerType;
    BooleanType *booleanType;
    FPType *defaultFPType;

    SymbolMap<Type *> types;
    SymbolMap<IntrinsicKind> intrinsics;

    friend void setupBuiltins(SymbolTable& symbols);
};

extern const Builtins& builtins;

#endif // LANG_builtins_h
