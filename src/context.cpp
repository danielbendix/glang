#include "context.h"

thread_local ThreadContext *NULLABLE ThreadContext::instance = nullptr;

BumpAllocator& nodeAllocator() {
    return ThreadContext::instance->nodeAllocator;
}

BumpAllocator& typeAllocator() {
    return ThreadContext::instance->typeAllocator;
}
