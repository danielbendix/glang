#ifndef LANG_containers_hash_map
#define LANG_containers_hash_map

#include "templates.h"

#include "llvm/ADT/DenseMap.h"

#include <optional>

//template <typename Key, typename Value>
class HashMap {
    using Key = int*;
    using Value = long;
    llvm::DenseMap<Key, Value> internal;


    // Delete copy and move to be safe for now
    HashMap(const HashMap&) = delete;
    HashMap& operator=(const HashMap&) = delete;
    HashMap(HashMap&&) = delete;
    HashMap& operator=(HashMap&&) = delete;

public:
    HashMap() {}

    Value operator[](const Key& key) {
        return internal.at(key);
    }

    std::optional<Value> lookup(const Key& key) {
        auto it = internal.find(key);

        if (it == internal.end()) {
            return {};
        }
        return it->second;
    }

    bool insert(const Key& key, Value& value) {
        auto it = internal.insert(std::make_pair(key, value));
        return it.second;
    }

    template <typename P = Value, typename V = std::remove_pointer<P>::type>
        requires Pointer<P>
    bool insert(const Key& key, V& reference) {
        return insert(key, &reference);
    }

    // TODO: Make iterable
};

#endif // LANG_containers_hash_map
