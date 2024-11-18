#ifndef LANG_containers_symbol_table_h
#define LANG_containers_symbol_table_h

#include "common.h"

#include "llvm/Support/Allocator.h"
#include "llvm/ADT/DenseMap.h"

/* We could have local buffer optimization on Symbol, but this would only yield 
 * a benefit when comparing symbols, which mainly occurs inside the symbol table
 * itself, as symbols from the same table can be tested for equality with pointer
 * equality.
 * It would also save a pointer indirection on data access, i.e. printing the symbol.
 */


class Symbol {
private:
    u64 hash;
    size_t size;
    char *data;

    Symbol(u64 hash, size_t size, char *data) : hash{hash}, size{size}, data{data} {}
    Symbol(const Symbol&) = delete;
    Symbol operator=(const Symbol&) = delete;
    Symbol(Symbol&&) = delete;
    Symbol operator=(Symbol&&) = delete;

    friend class SymbolTable;
    friend class llvm::DenseMapInfo<Symbol*>;
public:
    bool operator==(const Symbol& other) const {
        return this == &other;
    }

    operator std::string_view() const {
        return string_view();
    }

    const std::string_view string_view() const {
        return {data, size};
    }

    std::string string() const {
        return std::string{string_view()};
    }
};


class SymbolTable {
private:
    static constexpr size_t InitialCapacity = 16;
    static constexpr double MaxLoadFactor = 0.7;
    size_t nextResize = 0;

    struct Slot {
        Symbol *symbol;
    };

    llvm::BumpPtrAllocator symbolAllocator;
    llvm::BumpPtrAllocator stringAllocator;

    Slot *slots = nullptr;
    size_t capacity = 0;
    size_t count = 0;

    SymbolTable(const SymbolTable&) = delete;
    SymbolTable& operator=(const SymbolTable&) = delete;
    SymbolTable(SymbolTable&&) = delete;
    SymbolTable& operator=(SymbolTable&&) = delete;

    void growSlots();
    inline void insert(Symbol *symbol, u64 hash);

    Symbol *findSymbol(const std::string_view string, u64 hash);
    Symbol *insertSymbol(const std::string_view string, u64 hash);

public:
    SymbolTable() {}

    Symbol& getSymbol(const std::string_view string);
    Symbol *getSymbolIfExists(const std::string_view string);
};

#endif // LANG_containers_symbol_table_h
