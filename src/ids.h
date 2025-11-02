#ifndef LANG_ids_h
#define LANG_ids_h

#include "common.h"


template <typename DerivedID>
class IDBase {
    u32 value;
public:
    static constexpr u32 invalidID = ~0U;

    constexpr explicit IDBase() : value{invalidID} {}
    constexpr explicit IDBase(u32 value) : value{value} {}
    constexpr IDBase(IDBase const&) = default;
    constexpr IDBase(IDBase &&) = default;
    constexpr IDBase& operator=(IDBase const& other) = default;
    constexpr IDBase& operator=(IDBase&& other) = default;

    constexpr u32 index() const {
        return value;
    }

    constexpr operator u32() const {
        return value;
    }

    friend bool operator==(IDBase lhs, IDBase rhs) {
        return lhs.value == rhs.value;
    }

    friend bool operator!=(IDBase lhs, IDBase rhs) {
        return lhs.value != rhs.value;
    }

    friend bool operator<(IDBase lhs, IDBase rhs) {
        return lhs.value < rhs.value;
    }

    friend std::strong_ordering operator<=>(IDBase lhs, IDBase rhs) {
        return lhs.value <=> rhs.value;
    }


    // Explicitly disable comparison between different ID types:

    template <typename OtherDerivedID>
    requires (!std::is_same_v<OtherDerivedID, DerivedID>)
    friend bool operator==(IDBase, IDBase<OtherDerivedID>) {
        // TODO: Fix with false in C++23. For now this yields false when instantiated.
        static_assert(std::is_same_v<OtherDerivedID, DerivedID>, "You are trying to compare different ID types.");
    }

    template <typename OtherDerivedID>
    requires (!std::is_same_v<OtherDerivedID, DerivedID>)
    friend bool operator!=(IDBase, IDBase<OtherDerivedID>) {
        // TODO: Fix with false in C++23. For now this yields false when instantiated.
        static_assert(std::is_same_v<OtherDerivedID, DerivedID>, "You are trying to compare different ID types.");
    }

    template <typename OtherDerivedID>
    requires (!std::is_same_v<OtherDerivedID, DerivedID>)
    friend bool operator<(IDBase, IDBase<OtherDerivedID>) {
        // TODO: Fix with false in C++23. For now this yields false when instantiated.
        static_assert(std::is_same_v<OtherDerivedID, DerivedID>, "You are trying to compare different ID types.");
    }

    template <typename OtherDerivedID>
    requires (!std::is_same_v<OtherDerivedID, DerivedID>)
    friend std::strong_ordering operator<=>(IDBase lhs, IDBase rhs) {
        // TODO: Fix with false in C++23. For now this yields false when instantiated.
        static_assert(std::is_same_v<OtherDerivedID, DerivedID>, "You are trying to compare different ID types.");
    }
};

struct FunctionID final : public IDBase<FunctionID> {
    using IDBase<FunctionID>::IDBase;
};

struct GlobalID final : public IDBase<GlobalID> {
    using IDBase<GlobalID>::IDBase;
};

struct FileID final : public IDBase<FileID> {
    using IDBase<FileID>::IDBase;
};

#endif // LANG_ids_h
