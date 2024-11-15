#include "context.h"

GlobalContext globalContext;

thread_local ThreadContext *NULLABLE ThreadContext::instance = nullptr;

BumpAllocator& nodeAllocator() {
    return ThreadContext::instance->nodeAllocator;
}

BumpAllocator& typeAllocator() {
    return ThreadContext::instance->typeAllocator;
}
