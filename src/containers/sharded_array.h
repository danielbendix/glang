#ifndef LANG_sharded_array
#define LANG_sharded_array

#include "common.h"

#include <mutex>
#include <atomic>

namespace ShardedArrayHelpers {
    template<size_t Size>
    consteval u32 calculateDefaultBlockSize() {
        constexpr u32 MaxItems = 256;
        constexpr u32 MaxSize = 4096;
        if constexpr (Size <= (MaxSize / MaxItems)) {
            return MaxItems;
        } else {
            // TODO: Consider rounding down to power of two.
            // Performance will depend on how large blocks are allocated.
            return MaxSize / Size;
        }
    }

    template <size_t Size>
    constexpr u32 defaultBlockSize = calculateDefaultBlockSize<Size>();

    template <typename T>
    constexpr u32 defaultBlockSizeForType = defaultBlockSize<sizeof(T)>;
};

/**
 * This type is an array that consists of smaller blocks or "shards".
 * Blocks are not guaranteed to be full even if there is a block ahead of them.
 * This gives the array the following properties:
 * - Insertion is trivially parallel - Each thread can have its own block that it
 *   alone can append to.
 * - Addresses are stable, allowing for both indices and pointers to refer to
 *   values in the array.
 * - Iteration is possible, but not as trivial as e.g. vector or deque.
 */
//template <typename T, u32 BlockSize = ShardedArrayHelpers::defaultBlockSizeForType<T>>
class ShardedArray {
    using T = u32;

    /// Block size in number of elements
    static constexpr u32 BlockSize = ShardedArrayHelpers::defaultBlockSizeForType<T>;

    static_assert(BlockSize > 0);

    u32 blockCount = 0;
    u32 blockCapacity = 0;
    u32 count;

    T **blocks;
    std::mutex lock;

    constexpr static u32 calculateNewBlockCapacity(u32 capacity) {
        if (capacity == 0) {
            return 8;
        } else {
            return 16;
        }
    }

    T *makeBlock() {
        T *newBlock = (T *) malloc(sizeof(T) * BlockSize);
        T **oldBlocks = nullptr;
        
        std::scoped_lock _{lock};

        if (blockCount < blockCapacity) {
            blocks[blockCount] = newBlock;
            blockCount += 1;
        } else {
            u32 newBlockCapacity = calculateNewBlockCapacity(blockCapacity);
            T **newBlocks = (T **) malloc(sizeof(T *) * newBlockCapacity);

            memcpy(newBlocks, blocks, sizeof(T *) * blockCount);
            memset(&blocks[blockCount], 0, sizeof(T *) * (newBlockCapacity - blockCount));
            oldBlocks = blocks;
            blocks = newBlocks;
        }

        if (oldBlocks) {
            // TODO: Put oldBlocks in a deferred deallocation queue.

        }

        return newBlock;
    }
public:
    class Inserter {
        ShardedArray& array;
        T *block;
        u32 count;

        Inserter(ShardedArray& array, T *block, u32 count)
            : array{array}, block{block}, count{count} {}

        void insert(T& value) {
            if (count == BlockSize) {
                block = array.makeBlock();
                count = 0;
            }
            block[count] = value;
            count += 1;
        }
    };

    T& operator[](u32 index) const { 
        u32 blockIndex = index / BlockSize;
        u32 blockOffset = index % BlockSize;
        return blocks[blockIndex][blockOffset];
    }

    friend class Inserter;
};

#endif // LANG_block_array
