#include "builtins.h"
#include "context.h"

template <typename T, Allocator Allocator, typename... Args>
T *createType(Allocator& allocator, Args&&... args) {
    return allocate(allocator, [&](void *space) {
        return new(space) T(std::forward<Args>(args)...);
    });
}

Builtins _builtins;

void setupNumericTypes(SymbolTable& symbols, SymbolMap<Type *>& table, std::vector<Type *>& owner)
{
    auto& allocator = typeAllocator();

    Symbol& voidName = symbols.getSymbol("void");
    auto voidType = createType<VoidType>(allocator, voidName);
    table.insert(voidName, voidType);
    owner.push_back(voidType);
    
    Symbol& boolName = symbols.getSymbol("bool");
    auto booleanType = createType<BooleanType>(allocator, boolName);
    table.insert(boolName, booleanType);
    owner.push_back(booleanType);

    Symbol& f32Name = symbols.getSymbol("f32");
    auto f32Type = createType<FPType>(allocator, f32Name, FPType::Precision::Single);
    table.insert(f32Name, f32Type);
    owner.push_back(f32Type);

    Symbol& f64Name = symbols.getSymbol("f64");
    auto f64Type = createType<FPType>(allocator, f64Name, FPType::Precision::Double);
    table.insert(f64Name, f64Type);
    owner.push_back(f64Type);

    // TODO: Add [ui]size, [ui]ptr
   
    IntegerType *defaultIntegerType;

#define INT_TYPE(bits) { \
    Symbol& name = symbols.getSymbol("i" #bits); \
    auto type = createType<IntegerType>(allocator, name, bits, true); \
    table.insert(name, type); \
    if constexpr (bits == 64) { defaultIntegerType = type; } \
    owner.push_back(type); }
    INT_TYPE(8);
    INT_TYPE(16);
    INT_TYPE(32);
    INT_TYPE(64);
#undef INT_TYPE

#define UINT_TYPE(bits) { \
    Symbol& name = symbols.getSymbol("u" #bits); \
    auto type = createType<IntegerType>(allocator, name, bits, false); \
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

void setupIntrinsics(SymbolTable& symbols, SymbolMap<IntrinsicKind>& intrinsics)
{
    auto& truncateName = symbols.getSymbol("truncate");
    intrinsics.insert(truncateName, IntrinsicKind::Truncate);
}

void setupBuiltins(SymbolTable& symbols) 
{
    setupNumericTypes(symbols, _builtins.types, _builtins.all);
    setupIntrinsics(symbols, _builtins.intrinsics);
}

const Builtins& builtins = _builtins;
