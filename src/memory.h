#ifndef LANG_memory_h
#define LANG_memory_h

#include "common.h"

#include <memory>
#include <cassert>
#include <bit>

#include "llvm/Support/Allocator.h"

namespace Memory {
template <size_t alignment>
    constexpr static inline void *alignAddress(void *address) {
        static_assert(std::has_single_bit(alignment), "Alignment must be a power of two.");
        constexpr uintptr_t mask = alignment - 1;

        const auto value = reinterpret_cast<uintptr_t>(address);
        const auto aligned = (value + mask) & ~mask;
        return reinterpret_cast<void *>(aligned);
    }

    template <size_t alignment>
    constexpr static inline size_t alignAllocationSize(size_t allocationSize) {
        static_assert(std::has_single_bit(alignment), "Alignment must be a power of two.");
        constexpr size_t mask = alignment - 1;

        return (allocationSize + mask) & ~mask;
    }

    template <typename T>
    constexpr static inline bool isValidAlignment(T alignment) {
        return std::has_single_bit(alignment);
    }
}

template<typename T>
concept Allocator = requires(T allocator, size_t size, size_t alignment) {
    { allocator.allocate(size, alignment) } -> std::convertible_to<void *>;
};

struct BumpAllocator {
    llvm::BumpPtrAllocator allocator;

    BumpAllocator() {}
    BumpAllocator(BumpAllocator&& other) : allocator{std::move(other.allocator)} {}

    template<typename T>
    constexpr void *allocate() {
        return allocate(sizeof(T), alignof(T));
    }

    constexpr void *allocate(size_t size, size_t alignment) {
        return allocator.Allocate(size, alignment);
    }
};

template <Allocator Allocator, typename F, typename T = std::remove_pointer_t<std::invoke_result_t<F, void *>>>
T *NONNULL allocate(Allocator& allocator, F f) {
    void *NONNULL space = allocator.allocate(sizeof(T), alignof(T));
    return f(space);
}

#endif // LANG_memory_h
