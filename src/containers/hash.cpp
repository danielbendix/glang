#include "hash.h"

uint64_t hashString(const std::string& string) {
    uint64_t hash = 14695981039346656037UL;
    for (uint8_t c : string) {
        hash ^= c;
        hash *= 1099511628211UL;
    }
    return hash;
}

uint64_t hashString(const std::string_view string) {
    uint64_t hash = 14695981039346656037UL;
    for (uint8_t c : string) {
        hash ^= c;
        hash *= 1099511628211UL;
    }
    return hash;
}

constexpr uint64_t hashString(const char *string) {
    uint64_t hash = 14695981039346656037UL;
    uint8_t c;
    while ((c = *string++)) {
        hash ^= c;
        hash *= 1099511628211UL;
    }
    return hash;
}

constexpr uint64_t hashString(const char *string, size_t length) {
    uint64_t hash = 14695981039346656037UL;
    uint8_t c;
    while ((c = *string++)) {
        hash ^= c;
        hash *= 1099511628211UL;
    }
    return hash;
}
