#ifndef LANG_builtins_h
#define LANG_builtins_h

#include "type.h"
#include "containers/symbol_map.h"

void setupBuiltins(SymbolTable& symbols);

class Builtins {
    std::vector<Type *> allTypes;
public:
    VoidType *voidType;
    IntegerType *defaultIntegerType;
    BooleanType *booleanType;
    FPType *defaultFPType;

    SymbolMap<Type *> types;

    friend void setupBuiltins(SymbolTable& symbols);
};

extern const Builtins& builtins;

#endif // LANG_builtins_h
