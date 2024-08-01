#include "builtins.h"

Builtins _builtins;

void setupNumericTypes(SymbolTable& symbols, SymbolMap<Type *>& table, std::vector<Type *>& owner)
{
    Symbol& voidName = symbols.getSymbol("void");
    VoidType *voidType = new VoidType;
    table.insert(voidName, voidType);
    owner.push_back(voidType);

    Symbol& boolName = symbols.getSymbol("bool");
    BooleanType *booleanType = new BooleanType;
    table.insert(boolName, booleanType);
    owner.push_back(booleanType);

    Symbol& f32Name = symbols.getSymbol("f32");
    FPType *f32Type = new FPType{FPType::Precision::Single};
    table.insert(f32Name, f32Type);
    owner.push_back(f32Type);

    Symbol& f64Name = symbols.getSymbol("f64");
    FPType *f64Type = new FPType{FPType::Precision::Double};
    table.insert(f64Name, f64Type);
    owner.push_back(f64Type);

    // TODO: Add [ui]size, [ui]ptr
   
    IntegerType *defaultIntegerType;

#define INT_TYPE(bits) { \
    IntegerType *type = new IntegerType{bits, true}; \
    Symbol& name = symbols.getSymbol("i" #bits); \
    table.insert(name, type); \
    if constexpr (bits == 32) { defaultIntegerType = type; } \
    owner.push_back(type); }
    INT_TYPE(8);
    INT_TYPE(16);
    INT_TYPE(32);
    INT_TYPE(64);
#undef INT_TYPE

#define UINT_TYPE(bits) { \
    Type *type = new IntegerType{bits, false}; \
    Symbol& name = symbols.getSymbol("u" #bits); \
    table.insert(name, type); \
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

void setupBuiltins(SymbolTable& symbols) 
{
    setupNumericTypes(symbols, _builtins.types, _builtins.allTypes);
}

const Builtins& builtins = _builtins;
