#include "hash.h"

u64 hashString(const std::string& string) {
    u64 hash = 14695981039346656037UL;
    for (u8 c : string) {
        hash ^= c;
        hash *= 1099511628211;
    }
    return hash;
}

u64 hashString(const std::string_view string) {
    u64 hash = 14695981039346656037UL;
    for (u8 c : string) {
        hash ^= c;
        hash *= 1099511628211;
    }
    return hash;
}

constexpr u64 hashString(const char *string) {
    u64 hash = 14695981039346656037UL;
    u8 c;
    while ((c = *string++)) {
        hash ^= c;
        hash *= 1099511628211;
    }
    return hash;
}

constexpr u64 hashString(const char *string, size_t length) {
    u64 hash = 14695981039346656037UL;
    u8 c;
    while ((c = *string++)) {
        hash ^= c;
        hash *= 1099511628211;
    }
    return hash;
}
