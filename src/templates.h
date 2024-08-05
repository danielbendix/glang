#ifndef LANG_templates_h
#define LANG_templates_h

#include <concepts>
#include <functional>

template<class>
inline constexpr bool always_false_v = false;

template <typename T>
concept Pointer = std::is_pointer_v<T>;

namespace Templates {

    template<typename T, typename = void>
    struct DeleteValueArgType;

    template<typename Ret, typename Arg>
    struct DeleteValueArgType<Ret(*)(Arg*)> {
        using type = std::remove_pointer_t<Arg>;
    };

    template<typename Ret, typename Arg>
    struct DeleteValueArgType<std::function<Ret(Arg*)>> {
        using type = std::remove_pointer_t<Arg>;
    };

    template<typename T>
    struct DeleteValueArgType<T, std::enable_if_t<std::is_member_pointer_v<T>>> {
        using type = typename DeleteValueArgType<decltype(&T::operator())>::type;
    };

    template<typename T>
    struct DeleteValueArgType<T, std::enable_if_t<std::is_function_v<T>>> {
        using type = typename DeleteValueArgType<decltype(&T::operator())>::type;
    };

    template<typename T>
    struct DeleteValueArgType<T, std::void_t<decltype(&T::deleteValue)>> {
        using type = typename DeleteValueArgType<decltype(&T::deleteValue)>::type;
    };

    // Helper type alias to get the pointee type of the static method's argument
    template<typename T>
    using DeleteValueArgType_t = typename DeleteValueArgType<T>::type;

}

template <typename... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

#endif // LANG_templates_h
