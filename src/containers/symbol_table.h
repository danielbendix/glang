#include "common.h"

#include "llvm/Support/Allocator.h"

/* We could have local buffer optimization on Symbol, but this would only yield 
 * a benefit when comparing symbols, which mainly occurs inside the symbol table
 * itself, as symbols from the same table can be tested for equality with pointer
 * equality.
 */


class Symbol {
private:
    uint64_t hash;
    size_t size;
    char *data;

    Symbol(uint64_t hash, size_t size, char *data) : hash{hash}, size{size}, data{data} {}
    Symbol(const Symbol&) = delete;
    Symbol operator=(const Symbol&) = delete;

    friend class SymbolTable;
};

class SymbolTable {
private:
    const size_t InitialCapacity = 16;
    const double MaxLoadFactor = 0.7;
    size_t nextResize = 0;

    struct Slot {
        Symbol *symbol;
    };

    llvm::BumpPtrAllocator symbolAllocator;
    llvm::BumpPtrAllocator stringAllocator;

    Slot *slots = nullptr;
    size_t capacity = 0;
    size_t count = 0;

    void growSlots();
    inline void insert(Symbol *symbol, uint64_t hash);

    Symbol *findSymbol(std::string& string, uint64_t hash);
    Symbol *insertSymbol(std::string& string, uint64_t hash);

public:
    Symbol *getSymbol(std::string& string);
    Symbol *getSymbolIfExists(std::string& string);
};
