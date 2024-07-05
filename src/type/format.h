#include "type.h"
#include "type/struct.h"
#include "type/enum.h"

#include <format>

template <>
struct std::formatter<Type> {
    template <class ParseContext>
    constexpr ParseContext::iterator parse(ParseContext& context) { return context.begin(); }

    template <typename FormatContext>
    auto format(const Type& type, FormatContext& context) {
        return std::format_to(context.out(), "[TYPE]: {}", type.getKind());
    }
};
