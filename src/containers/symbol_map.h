#ifndef LANG_containers_symbol_map
#define LANG_containers_symbol_map

#include "templates.h"

#include "symbol_table.h"

#include <llvm/ADT/DenseMap.h>

namespace llvm {
    template<>
    struct DenseMapInfo<Symbol*> {
        static inline Symbol *getEmptyKey() {
            return nullptr;
        }

        static inline Symbol *getTombstoneKey() {
            return (Symbol *) -1;
        }

        static unsigned getHashValue(const Symbol *symbol) {
            return symbol->hash;
        }

        static bool isEqual(const Symbol *lhs, const Symbol *rhs) {
            return lhs == rhs;
        }
    };
}

template <typename T>
class SymbolMap {
    llvm::DenseMap<const Symbol *, T> internal;

    // Delete copy and move to be safe for now
    SymbolMap(const SymbolMap&) = delete;
    SymbolMap& operator=(const SymbolMap&) = delete;

public:
    SymbolMap() {}
    SymbolMap(SymbolMap&& from) : internal{std::move(from.internal)} {}
    SymbolMap& operator=(SymbolMap&& from) {
        internal.swap(from.internal);
        return *this;
    }

    T operator[](const Symbol& key) const {
        return internal.at(&key);
    }

    std::optional<T> lookup(const Symbol& key) const {
        auto it = internal.find(&key);

        if (it == internal.end()) {
            return {};
        }
        return it->second;
    }

    bool containsKey(const Symbol& key) const {
        auto it = internal.find(&key);

        return it != internal.end();
    }

    bool insert(const Symbol& key, T value) {
        const std::pair<const Symbol *, T> pair = std::make_pair(&key, value);
        auto it = internal.insert(pair);
        return it.second;
    }
};

#endif // LANG_containers_symbol_map
