#ifndef LANG_templates_h
#define LANG_templates_h

#include <concepts>
#include <functional>

template<class>
inline constexpr bool always_false_v = false;

template <typename T>
concept Pointer = std::is_pointer_v<T>;

template <typename... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

#endif // LANG_templates_h
