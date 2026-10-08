#ifndef LANG_containers_string_map
#define LANG_containers_string_map

#include "templates.h"

#include <llvm/ADT/StringMap.h>

template <typename T>
class StringMap {
    llvm::StringMap<T> internal;

    // Delete copy and move to be safe for now
    StringMap(const StringMap&) = delete;
    StringMap& operator=(const StringMap&) = delete;

public:
    StringMap() {}
    StringMap(StringMap&& from) : internal{std::move(from.internal)} {}
    StringMap& operator=(StringMap&& from) {
        internal.swap(from.internal);
        return *this;
    }

    T operator[](const std::string& key) const {
        return internal.at(key);
    }

    std::optional<T> lookup(const std::string& key) const {
        auto it = internal.find(key);

        if (it == internal.end()) {
            return {};
        }
        return it->second;
    }

    bool containsKey(const std::string& key) const {
        auto it = internal.find(key);

        return it != internal.end();
    }

    bool insert(const std::string& key, T value) {
        auto it = internal.insert(std::make_pair(key, value));
        return it.second;
    }
};

#endif // LANG_containers_string_map
