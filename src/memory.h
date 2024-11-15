#ifndef LANG_memory_h
#define LANG_memory_h

#include "common.h"
#include <memory>
#include <cassert>

#include "mimalloc.h"
#include "llvm/Support/Allocator.h"

template <typename T>
struct ArrayAllocator {
    using value_type = T;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using propagate_on_container_move_assignment = std::true_type;

    ArrayAllocator(mi_heap_t *heap) : heap{heap} {
        assert(heap);
    }

    constexpr T *allocate(std::size_t n) {
        return static_cast<T*>(mi_heap_malloc_aligned(heap, n * sizeof(T), alignof(T)));
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
    Heap& operator=(Heap&&) = delete;

    Heap(Heap&& other) {
        heap = other.heap;
        other.heap = nullptr;
    }

    ~Heap() {
        if (heap) {
            mi_heap_destroy(heap);
        }
    }

    template <typename T>
    ArrayAllocator<T> allocator() {
        assert(heap);
        return ArrayAllocator<T>(heap);
    }
};

template <Allocator Allocator, typename F, typename T = std::remove_pointer_t<std::invoke_result_t<F, void *>>>
T *NONNULL allocate(Allocator& allocator, F f) {
    void *NONNULL space = allocator.allocate(sizeof(T), alignof(T));
    return f(space);
}

/// Represents the memory resources of the AST from a single file.
struct ASTHandle {
    Heap heap;
    BumpAllocator nodeAllocator;

    ASTHandle(Heap&& heap, BumpAllocator&& nodeAllocator) : heap{std::move(heap)}, nodeAllocator{std::move(nodeAllocator)} {}
};

#endif // LANG_memory_h
