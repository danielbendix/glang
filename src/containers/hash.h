#include "common.h"

#include <string>

/// Hash a null-terminated string
constexpr u64 hashString(const char *string);
constexpr u64 hashString(const char *string, size_t length);
u64 hashString(const std::string_view string);
u64 hashString(const std::string& string);
