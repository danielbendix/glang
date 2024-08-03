#ifndef LANG_context_h
#define LANG_context_h

#include "memory.h"
#include "containers/symbol_table.h"

struct ThreadContext {
    static thread_local ThreadContext *NULLABLE instance;

    ThreadContext(SymbolTable *symbols) : symbols{symbols} {
        assert(this);
        instance = this;
    }

    ThreadContext(const ThreadContext&) = delete;
    ThreadContext& operator=(const ThreadContext&) = delete;

    ~ThreadContext() {
        instance = nullptr;
    }

    static ThreadContext *get() {
        assert(instance);
        return instance;
    }

    SymbolTable *NONNULL symbols;
    Heap heap;
    BumpAllocator nodeAllocator;

    template <typename T>
    ArrayAllocator<T> allocator() {
        return ArrayAllocator<T>{heap};
    }
};

template <typename T>
ArrayAllocator<T> allocator() {
    return ThreadContext::instance->allocator<T>();
}

BumpAllocator& nodeAllocator();

#endif // LANG_context_h