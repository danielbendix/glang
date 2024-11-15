#ifndef LANG_context_h
#define LANG_context_h

#include "memory.h"
#include "containers/symbol_table.h"

#include <filesystem>

struct File {
    const char *path;
    const char *name;
    uint32_t pathSize;
    uint32_t nameSize;
    std::unique_ptr<ASTHandle> astHandle = nullptr;
    std::vector<uint32_t> lineBreaks;
};

struct GlobalContext {
    std::vector<File> files;
    BumpAllocator filenameAllocator;
    std::mutex lock;

    uint32_t addFile(const char *filePath) {
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
        files.push_back({.name = filename, .path = filepath });

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

    SymbolTable *NONNULL symbols;
    // This can be removed once the Sema phase is emitting GIR, instead of decorating nodes.
    BumpAllocator nodeAllocator;
    BumpAllocator typeAllocator;
};

BumpAllocator& nodeAllocator();

BumpAllocator& typeAllocator();

#endif // LANG_context_h
