#ifndef LANG_memory_h
#define LANG_memory_h

#include <memory>
#include <cassert>

#include "mimalloc.h"
#include "llvm/Support/Allocator.h"

template <typename T>
struct ArrayAllocator {
    using value_type = T;

    ArrayAllocator(mi_heap_t *heap) : heap{heap} {
        assert(heap);
    }

    constexpr T *allocate(std::size_t n) {
        return mi_heap_malloc_aligned(heap, n * sizeof(T), alignof(T));
    }

    constexpr void deallocate(T *ptr, std::size_t n) {
        return mi_free((void *) ptr);
    }

    friend bool operator==(ArrayAllocator lhs, ArrayAllocator rhs);
    friend bool operator!=(ArrayAllocator lhs, ArrayAllocator rhs);

private:
    mi_heap_t *heap;
};

template <typename T, typename U>
bool operator==(const ArrayAllocator<T> lhs, const ArrayAllocator<U> rhs) { 
    return lhs.heap == rhs.heap;
}

template <typename T, typename U>
bool operator!=(const ArrayAllocator<T> lhs, const ArrayAllocator<U> rhs) { 
    return lhs.heap != rhs.heap;
}

template<typename T>
concept Allocator = requires(T allocator, size_t size, size_t alignment) {
    { allocator.allocate(size, alignment) } -> std::convertible_to<void *>;
};


struct BumpAllocator {
    llvm::BumpPtrAllocator allocator;

    template<typename T>
    constexpr void *allocate() {
        return allocate(sizeof(T), alignof(T));
    }

    constexpr void *allocate(size_t size, size_t alignment) {
        return allocator.Allocate(size, alignment);
    }
};

/// A heap with the following properties:
/// - Supports arbitrary allocation.
/// - Supports deallocation.
/// - Allows removing all allocations at once.
/// - On destruction will preserve mappings, not directly calling mmap.
class Heap {
    mi_heap_t *heap;

public: 
    Heap() {
        heap = mi_heap_new();
        assert(heap);
    }

    Heap(const Heap&) = delete;
    Heap& operator=(const Heap&) = delete;
    Heap(Heap&&) = delete;
    Heap& operator=(Heap&&) = delete;

    ~Heap() {
        mi_heap_destroy(heap);
    }

    template <typename T>
    ArrayAllocator<T> allocator() {
        return ArrayAllocator<T>(heap);
    }
};


#endif // LANG_memory_h
