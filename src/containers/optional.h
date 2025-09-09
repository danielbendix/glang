#ifndef LANG_optional_h
#define LANG_optional_h

#include <cstddef>
#include <cassert>
#include <utility>
#include <bit>
#include <cstdint>

namespace OptionalTemplateHelpers {
    template <typename T>
    struct member_pointer_traits {};

    template <typename MemberType, typename ClassType>
    struct member_pointer_traits<MemberType ClassType::*> {
        using class_type = ClassType;
        using member_type = MemberType;
    };

    template <typename MemberType, typename ClassType>
    struct member_pointer_traits<MemberType ClassType::*const> {
        using class_type = ClassType;
        using member_type = MemberType;
    };

    template <typename T>
    using remove_pointer_modifiers = std::conditional_t<
        std::is_pointer_v<T>,
        std::remove_cv_t<T>,
        T
    >;

    // A template should remove any clang nullability annotations.
    // TODO: Check whether this is necessary for the setup.
    template <typename T>
    using remove_clang_nullability = T;

    template <typename ClassType, typename MemberType>
    consteval size_t get_offset(MemberType const ClassType::*const memberPointer) {
        constexpr ClassType data = {};
        auto *base = &data;
        auto *member = &data.*memberPointer;
        return member - base;
    }

    template <typename T, typename U>
        constexpr std::size_t offset_of_impl(U T::*member) {
        // Create a properly aligned storage for T
        alignas(T) std::byte buffer[sizeof(T)] = {};
        // Make a properly aligned T object pointer
        const T* obj = reinterpret_cast<const T*>(buffer);
        // Get the address of the member
        const auto member_addr = &(obj->*member);
        // Get the difference in bytes
        return static_cast<std::size_t>(
            reinterpret_cast<const std::byte*>(member_addr) - buffer
        );
    }
}

template <typename T>
struct OptionalDiscriminant {
    using OptionalDiscriminantType = void;
};

/// An "inline optional" storage, which uses a member to discriminate between none and some.
template <typename T>
struct Optional {
    union Internal {
        T data;
        alignas(T) std::byte bytes[sizeof(T)];

        Internal() {};
        Internal(T data) : data{data} {}
    } internal;

    using MemberType = typename OptionalDiscriminant<T>::OptionalDiscriminantType;
    using ConstMemberType = std::add_const_t<MemberType>;

    static constexpr size_t MemberOffset = OptionalDiscriminant<T>::OptionalDiscriminantOffset;

    MemberType *pointerToDiscriminant() {
        MemberType *pointer = reinterpret_cast<MemberType *>(internal.bytes + MemberOffset);
        return pointer;
    }

    ConstMemberType *pointerToDiscriminant() const {
        ConstMemberType *pointer = reinterpret_cast<ConstMemberType *>(internal.bytes + MemberOffset);
        return pointer;
    }

    Optional() {
        internal = {};
        *pointerToDiscriminant() = {};
    }

    explicit Optional(T data) : internal{data} {}

    template<typename... Args>
    Optional(Args... args) {
        internal.data = T(args...);
    }

    operator bool() const {
        return bool(*pointerToDiscriminant());
    }

    operator T() const {
        assert(bool(*this));
        return internal.data;
    }
    
    T operator*() const {
        return internal.data;
    }
};

#endif // LANG_optional_h
