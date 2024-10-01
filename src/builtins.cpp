#include "builtins.h"
#include "context.h"

template <typename T, Allocator Allocator, typename... Args>
T *createType(Allocator& allocator, Args&&... args) {
    return allocate(allocator, [&](void *space) {
        return new(space) T(std::forward<Args>(args)...);
    });
}

Builtins _builtins;

void setupNumericTypes(SymbolTable& symbols, SymbolMap<Type *>& table, const Architecture& architecture)
{
    auto& allocator = typeAllocator();

    Symbol& voidName = symbols.getSymbol("void");
    auto voidType = createType<VoidType>(allocator, voidName);
    table.insert(voidName, voidType);
    
    Symbol& boolName = symbols.getSymbol("bool");
    auto booleanType = createType<BooleanType>(allocator, boolName, architecture.int8);
    table.insert(boolName, booleanType);

    Symbol& f32Name = symbols.getSymbol("f32");
    auto f32Type = createType<FPType>(allocator, f32Name, FPType::Precision::Single, architecture.fpSingle);
    table.insert(f32Name, f32Type);

    Symbol& f64Name = symbols.getSymbol("f64");
    auto f64Type = createType<FPType>(allocator, f64Name, FPType::Precision::Double, architecture.fpDouble);
    table.insert(f64Name, f64Type);

    // TODO: Add [ui]size, [ui]ptr
   
    IntegerType *defaultIntegerType;

#define INT_TYPE(bits) { \
    Symbol& name = symbols.getSymbol("i" #bits); \
    auto type = createType<IntegerType>(allocator, name, bits, true, architecture.int ## bits); \
    table.insert(name, type); \
    if constexpr (bits == 64) { defaultIntegerType = type; } \
}
    INT_TYPE(8);
    INT_TYPE(16);
    INT_TYPE(32);
    INT_TYPE(64);
#undef INT_TYPE

#define UINT_TYPE(bits) { \
    Symbol& name = symbols.getSymbol("u" #bits); \
    auto type = createType<IntegerType>(allocator, name, bits, false, architecture.int ## bits); \
    table.insert(name, type); \
}
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

    auto& printName = symbols.getSymbol("print");
    intrinsics.insert(printName, IntrinsicKind::Print);

    auto& assertName = symbols.getSymbol("assert");
    intrinsics.insert(assertName, IntrinsicKind::Assert);

    auto& bitcastName = symbols.getSymbol("bitcast");
    intrinsics.insert(bitcastName, IntrinsicKind::Bitcast);
}

void setupBuiltins(SymbolTable& symbols, const Architecture& architecture)
{
    setupNumericTypes(symbols, _builtins.types, architecture);
    setupIntrinsics(symbols, _builtins.intrinsics);
}

const Builtins& builtins = _builtins;
