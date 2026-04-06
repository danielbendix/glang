#include "symbol_table.h"
#include "hash.h"

void SymbolTable::growSlots() {
    size_t oldCapacity = capacity;
    Slot *oldSlots = slots;

    assert(oldSlots != nullptr);
    assert(capacity != 0);
    capacity = capacity << 1;

    const u64 mask = capacity - 1;
    slots = (Slot *) calloc(capacity, sizeof(Slot));

    const Slot *end = oldSlots + oldCapacity;
    for (Slot *it = oldSlots; it != end; ++it) {
        if (!it->symbol) continue;
        insert(it->symbol, it->symbol->hash);
    }
    free(oldSlots);

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

std::pair<Symbol *, SymbolTable::Slot *> SymbolTable::findSymbol(const std::string_view string, u64 hash) {
    assert(capacity != 0);

    const u64 mask = capacity - 1;
    u64 index = hash & mask;

    for (;;) {
        Slot slot = slots[index];

        if (!slot.symbol) {
            return {nullptr, &slots[index]};
        }
        u32 length = slot.symbol->size;
        if (length == string.size() && string == std::string_view{slot.symbol->data, length}) {
            return {slot.symbol, &slots[index]};
        }

        index = (index + 1) & mask;
    }
}

Symbol *SymbolTable::insertSymbol(const std::string_view string, u64 hash, Slot *slot) {
    Symbol *symbol = symbolAllocator.Allocate<Symbol>();
    symbol->hash = hash;
    symbol->size = string.size();
    size_t allocationSize = string.size() + 1;
    symbol->data = (char *)stringAllocator.Allocate(allocationSize, 8);
    memcpy(symbol->data, string.data(), string.size());
    symbol->data[string.size()] = '\0';

    assert(slot->symbol == nullptr);
    slot->symbol = symbol;

    count += 1;

    if (count > nextResize) {
        growSlots();
    }

    return symbol;
}

Symbol& SymbolTable::getSymbol(const std::string_view string) {
    u64 hash = hashString(string);

    auto [symbol, slot] = findSymbol(string, hash);

    assert(slot != nullptr);

    if (symbol) [[likely]] {
        return *symbol;
    } else {
        return *insertSymbol(string, hash, slot);
    }
}

Symbol *SymbolTable::getSymbolIfExists(const std::string_view string) {
    u64 hash = hashString(string);
    return findSymbol(string, hash).first;
}
