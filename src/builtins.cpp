#include "builtins.h"

Builtins _builtins;

void setupNumericTypes(StringMap<Type *>& table, std::vector<Type *>& owner)
{
    VoidType *voidType = new VoidType;
    table.insert("void", voidType);
    owner.push_back(voidType);

    BooleanType *booleanType = new BooleanType;
    table.insert("bool", booleanType);
    owner.push_back(booleanType);

    FPType *f32Type = new FPType{FPType::Precision::Single};
    table.insert("f32", f32Type);
    table.insert("fp32", f32Type);
    owner.push_back(f32Type);

    FPType *f64Type = new FPType{FPType::Precision::Double};
    table.insert("f64", f64Type);
    table.insert("fp64", f64Type);
    owner.push_back(f64Type);

    // TODO: Add [ui]size, [ui]ptr
   
    IntegerType *defaultIntegerType;

#define INT_TYPE(bits) { \
    IntegerType *type = new IntegerType{bits, true}; \
    table.insert("i" #bits, type); \
    if constexpr (bits == 32) { defaultIntegerType = type; } \
    table.insert("int" #bits, type); \
    owner.push_back(type); }
    INT_TYPE(8);
    INT_TYPE(16);
    INT_TYPE(32);
    INT_TYPE(64);
#undef INT_TYPE

#define UINT_TYPE(bits) { \
    Type *type = new IntegerType{bits, false}; \
    table.insert("u" #bits, type); \
    table.insert("uint" #bits, type); \
    owner.push_back(type); }
    UINT_TYPE(8);
    UINT_TYPE(16);
    UINT_TYPE(32);
    UINT_TYPE(64);
#undef INT_TYPE

    assert(defaultIntegerType && "The default integer type should not be NULL.");

    _builtins.voidType = voidType;
    _builtins.booleanType = booleanType;
    _builtins.defaultFPType = f64Type;
    _builtins.defaultIntegerType = defaultIntegerType;
}

void setupBuiltins() 
{
    setupNumericTypes(_builtins.types, _builtins.allTypes);
}

const Builtins& builtins = _builtins;
