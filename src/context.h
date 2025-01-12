#ifndef LANG_context_h
#define LANG_context_h

#include "memory.h"
#include "array_allocator.h"
#include "containers/symbol_table.h"

#include <filesystem>

/// Represents the memory resources of the AST from a single file.
struct ASTHandle {
    BumpAllocator nodeAllocator;
    ArrayArenaAllocator arrayAllocator;

    ASTHandle(BumpAllocator&& nodeAllocator, ArrayArenaAllocator&& arrayAllocator) 
        : nodeAllocator{std::move(nodeAllocator)}, arrayAllocator{std::move(arrayAllocator)} {}
};

struct File {
    const char *path;
    const char *name;
    const u32 pathSize;
    const u32 nameSize;
    u32 size = 0;
    std::unique_ptr<ASTHandle> astHandle = nullptr;
    std::vector<u32> lineBreaks;

    File(const char *path, u32 pathSize, const char *name, u32 nameSize)
        : path{path}, pathSize{pathSize}, name{name}, nameSize{nameSize} {}
};

struct GlobalContext {
    std::vector<File> files;
    BumpAllocator filenameAllocator;
    std::mutex lock;

    u32 addFile(const char *filePath) {
        auto path = std::filesystem::path{filePath};
        auto pathString = path.string();
        auto filenameString = path.filename().string();

        std::scoped_lock _{lock};

        char *filepath = (char *) filenameAllocator.allocate(pathString.size() + 1, 1);
        char *filename = (char *) filenameAllocator.allocate(filenameString.size() + 1, 1);
        std::memcpy(filepath, pathString.data(), pathString.size());
        filepath[pathString.size()] = '\0';
        std::memcpy(filename, filenameString.data(), filenameString.size());
        filename[filenameString.size()] = '\0';

        assert(files.capacity() > files.size());
        auto index = files.size();
        files.emplace_back(filepath, pathString.size(), filename, filenameString.size());

        return index;
    }
};

extern GlobalContext globalContext;

struct ThreadContext {
    static thread_local ThreadContext *NULLABLE instance;

    ThreadContext(SymbolTable *NONNULL symbols) : symbols{symbols} {
        assert(this);
        instance = this;
    }

    ThreadContext(const ThreadContext&) = delete;
    ThreadContext& operator=(const ThreadContext&) = delete;

    ~ThreadContext() {
        instance = nullptr;
    }

    static ThreadContext *NONNULL get() {
        assert(instance);
        return instance;
    }

    static void setCurrentFile(u32 fileHandle) {
        instance->currentFile = fileHandle;
    }

    u32 currentFile;
    SymbolTable *NONNULL symbols;
    // This can be removed once the Sema phase is emitting GIR, instead of decorating nodes.
    BumpAllocator nodeAllocator;
    // We may be able to get rid of this, or reduce its usage by using arrays of different kinds of types.
    // This would allow for the following:
    // - Referring to types by u32.
    // - Existential processing of types.
    // - Parallel arrays of LLVM types that exist only during codegen, reducing sizeof(Type).
    BumpAllocator typeAllocator;
};

BumpAllocator& nodeAllocator();

BumpAllocator& typeAllocator();

#endif // LANG_context_h
