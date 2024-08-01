#include <string>

/// Hash a null-terminated string
uint64_t hashString(const char *string);
uint64_t hashString(const char *string, size_t length);
uint64_t hashString(const std::string_view string);
uint64_t hashString(const std::string& string);
