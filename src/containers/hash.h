#include <string>

/// Hash a null-terminated string
constexpr uint64_t hashString(const char *string);
constexpr uint64_t hashString(const char *string, size_t length);
uint64_t hashString(const std::string_view string);
uint64_t hashString(const std::string& string);


class StringHasher {
    uint64_t _hash = 14695981039346656037UL;
public:
    void add(char c) {
        _hash ^= c;
        _hash *= 1099511628211UL;
    }
    
    uint64_t hash() {
        return _hash;
    }
};
