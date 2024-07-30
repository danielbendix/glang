#ifndef LANG_builtins_h
#define LANG_builtins_h

#include "type.h"

void setupBuiltins();

class Builtins {
    std::vector<Type *> allTypes;
public:
    VoidType *voidType;
    IntegerType *defaultIntegerType;
    BooleanType *booleanType;
    FPType *defaultFPType;

    StringMap<Type *> types;

    friend void setupBuiltins();
};

extern const Builtins& builtins;

#endif // LANG_builtins_h
