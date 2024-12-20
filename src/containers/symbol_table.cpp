#include "symbol_table.h"
#include "hash.h"

void SymbolTable::growSlots() {
    size_t oldCapacity = capacity;
    Slot *oldSlots = slots;

    if (capacity == 0) {
        capacity = InitialCapacity;
    } else {
        capacity = capacity << 1;
    }

    const u64 mask = capacity - 1;
    slots = (Slot *) calloc(capacity, sizeof(Slot));
    if (oldSlots) {
        const Slot *end = oldSlots + oldCapacity;
        for (Slot *it = oldSlots; it != end; ++it) {
            if (!it->symbol) continue;
            insert(it->symbol, it->symbol->hash);
        }
        free(oldSlots);
    }

    nextResize = capacity * MaxLoadFactor;
}

inline void SymbolTable::insert(Symbol *symbol, u64 hash) {
    const u64 mask = capacity - 1;
    u64 index = symbol->hash & mask;
    
    for (;;) {
        Slot& slot = slots[index];

        if (!slot.symbol) {
            slot.symbol = symbol;
            return;
        }

        index = (index + 1) & mask;
    }
}

Symbol *SymbolTable::findSymbol(const std::string_view string, u64 hash) {
    if (capacity == 0) {
        return nullptr;
    }

    const u64 mask = capacity - 1;
    u64 index = hash & mask;

    for (;;) {
        Slot slot = slots[index];

        if (!slot.symbol) {
            return nullptr;
        }
        if (slot.symbol->size == string.size() && slot.symbol->data == string) {
            return slot.symbol;
        }

        index = (index + 1) & mask;
    }
}

Symbol *SymbolTable::insertSymbol(const std::string_view string, u64 hash) {
    Symbol *symbol = symbolAllocator.Allocate<Symbol>();
    symbol->hash = hash;
    symbol->size = string.size();
    size_t allocationSize = string.size() + 1;
    symbol->data = (char *) stringAllocator.Allocate(allocationSize, 8);
    memcpy(symbol->data, string.data(), string.size());
    symbol->data[string.size()] = '\0';

    const size_t newCount = count + 1;

    if (newCount > nextResize) {
        growSlots();
    }

    const u64 mask = capacity - 1;
    u64 index = hash & mask;

    insert(symbol, hash);

    count += 1;

    return symbol;
}

Symbol& SymbolTable::getSymbol(const std::string_view string) {
    u64 hash = hashString(string);

    Symbol *symbol = findSymbol(string, hash);

    if (symbol) {
        return *symbol;
    }

    return *insertSymbol(string, hash);
}

Symbol *SymbolTable::getSymbolIfExists(const std::string_view string) {
    u64 hash = hashString(string);
    return findSymbol(string, hash);
}
