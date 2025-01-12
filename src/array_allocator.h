#ifndef LANG_array_allocator_h
#define LANG_array_allocator_h

#include "common.h"
#include "memory.h"

#include "llvm/ADT/PointerIntPair.h"

#include <bit>
#include <vector>

/** - Dynamic arrays - Synopsis
 * - Implement dynamic arrays, in order to:
 *   1. remove mimalloc
 *   2. reduce memory footprint of dynamic arrays, by not having capacity, as arrays can be "frozen".
 *   3. potentially reduce fragmentation.
 *
 * Arrays and strings will use the same AST-level allocator.
 * The mutable containers will be friends of the immutable ones, and can emit the immutable ones, along with leftover memory, on "freezing" them.
 * The allocator uses size classes.
 * The mutable container keeps track of the allocation 
 *
 */

/// Arena allocator intended for use with dynamically growing arrays, not single nodes.
class ArrayArenaAllocator final {
    struct Stats {
#ifdef ARRAY_ARENA_ALLOCATOR_STATS
        size_t allocationsFromSystem = 0;
        size_t allocatedFromSystem = 0;
        size_t allocationsInUse = 0;
        size_t allocatedInUse = 0;

        void countAllocated(size_t n) {
            allocationsFromSystem += 1;
            allocatedFromSystem += n;
        }

        void countFreed(size_t n) {
            allocationsFromSystem -= 1;
            allocatedFromSystem -= n;
        }
        
        void countUsed(size_t n) {
            allocationsInUse += 1;
            allocatedInUse += n;
        }

        void countReclaimed(size_t n) {
            allocatedInUse -= n;
        }

        void countDeallocated(size_t n) {
            allocationsInUse -= 1;
            allocatedInUse -= n;
        }

        void print() {
            if (allocationsFromSystem == 0 && allocatedFromSystem == 0 && allocationsInUse == 0 && allocatedInUse == 0) return;
            printf("--- Arena Allocator Stats Begin ---\n");
            printf("Total system allocations: %'lu\n", allocationsFromSystem);
            printf("Allocated from system: %'lu bytes\n", allocatedFromSystem);
            printf("Total allocations in use: %lu\n", allocationsInUse);
            printf("In use by containers: %'lu bytes\n", allocatedInUse);
            printf("--- Arena Allocator Stats End ---\n");

        }
#else
        void countAllocated(size_t n) {}

        void countFreed(size_t n) {}
        
        void countUsed(size_t n) {}

        void countReclaimed(size_t n) {}

        void countDeallocated(size_t n) {}

        void print() {}
#endif
    };

    struct Block {
        Block *next;
        u32 size;
    };
public:
    template <typename T>
    struct Allocation {
        static constexpr u32 INVALID_ALLOCATION_INDEX = ~0U;

        T *space;
        u32 size;
        u32 index;

        Allocation(T *pointer, u32 size)
            : space{pointer}, size{size}, index{INVALID_ALLOCATION_INDEX} {}

        Allocation(T *pointer, u32 size, u32 index)
            : space{pointer}, size{size}, index{index} {}

        template <typename U>
        Allocation<U> as() const {
            return Allocation<U>{(U *) space, size, index};
        }
    };

private:
    /// The guaranteed alignment of allocations from the parent allocator.
    static constexpr size_t MIN_ALLOC_ALIGN = 16;
    static_assert(Memory::isValidAlignment(MIN_ALLOC_ALIGN));
    /// The size of the smallest size class.
    static constexpr size_t BASE_SIZE = 32;
    static constexpr size_t BASE_SIZE_CLASS = std::countr_zero(BASE_SIZE);
    static_assert(std::has_single_bit(BASE_SIZE), "Class sizes must be powers of two.");
    static_assert(BASE_SIZE % MIN_ALLOC_ALIGN == 0, "Base allocation size must be aligned.");
    /// Number of size classes to use, with the last capturing all sizes beyond that.
    static constexpr size_t CLASSES_COUNT = 9; // 32 ... 8192
    /// The minimum size we allocate from the parent allocator.
    static constexpr size_t MIN_ALLOCATION_SIZE = 4096;
    static constexpr size_t MIN_ALLOCATION_SIZE_CLASS = std::countr_zero(MIN_ALLOCATION_SIZE) - BASE_SIZE_CLASS;
    static_assert(std::has_single_bit(MIN_ALLOCATION_SIZE));

    static constexpr auto alignAddress(auto address) {
        return Memory::alignAddress<MIN_ALLOC_ALIGN>(address);
    }

    static constexpr auto alignSize(auto allocationSize) {
        return Memory::alignAllocationSize<MIN_ALLOC_ALIGN>(allocationSize);
    }

    static std::byte *bytewiseIncrementPointer(void *pointer, size_t increment) {
        auto *bytePointer = (std::byte *) pointer;
        return bytePointer + increment;
    }

    static constexpr size_t normalizeAllocationSize(size_t size) {
        if (size < BASE_SIZE) {
            return BASE_SIZE;
        } else {
            return alignSize(size);
        }
    }

    /// These are all singular allocations, and can be passed to free().
    /// Some elements may have been set to NULL if they were freed.
    std::vector<void *> allocations;
    /// Size classes from BASE_SIZE up to BASE_SIZE * 2^(CLASSES_COUNT - 1).
    Block *classes[CLASSES_COUNT];
    /// Catch-all size class.
    Block *large = nullptr;

    Stats stats;

    Allocation<std::byte> allocateFromParent(size_t n) {
        stats.countAllocated(n);

        u32 index = allocations.size();
        auto *space = (std::byte *) malloc(n);
        allocations.push_back(space);
        return {space, u32(n), index};
    }

    Allocation<std::byte> allocateNewChunk() {
        return allocateFromParent(MIN_ALLOCATION_SIZE);
    }

    void addBlock(void *space, u32 size, size_t sizeClass) {
        Block *block = (Block *) space;
        block->size = size;
        block->next = classes[sizeClass];
        classes[sizeClass] = block;

        assert(sizeClass < CLASSES_COUNT);
    }

    void addLargeBlock(void *space, u32 size) {
        Block *block = (Block *) space;
        block->size = size;
        block->next = large;
        large = block;
    }

    void sliceRemainderFromBlock(void *space, u32 blockSize, u32 size) {
        assert(size % MIN_ALLOC_ALIGN == 0);
        assert(blockSize >= size);
        auto *newBlock = bytewiseIncrementPointer(space, size);
        u32 newBlockSize = blockSize - size;
        assert(newBlockSize >= BASE_SIZE);
        size_t newBlockSizeClass = calculateSizeClassIncoming(newBlockSize);

        if (newBlockSizeClass < CLASSES_COUNT) [[likely]] {
            addBlock(newBlock, newBlockSize, newBlockSizeClass);
        } else {
            addLargeBlock(newBlock, newBlockSize);
        }
    }

public:
    ArrayArenaAllocator() {
        this->large = nullptr;
        constexpr size_t CLASSES_SIZE = sizeof(Block *) * CLASSES_COUNT;
        memset(this->classes, 0, CLASSES_SIZE);
    }

    ArrayArenaAllocator(ArrayArenaAllocator&& other) {
        constexpr size_t CLASSES_SIZE = sizeof(Block *) * CLASSES_COUNT;

        allocations = std::move(other.allocations);
        memcpy(classes, other.classes, CLASSES_SIZE);
        large = other.large;

        memset(other.classes, 0, CLASSES_SIZE);
        other.large = nullptr;
    }

    /// For a size that is being allocated.
    constexpr static size_t calculateSizeClassOutgoing(size_t n) {
        size_t sizeClass = std::countr_zero(std::bit_ceil(n));
        sizeClass = sizeClass <= BASE_SIZE_CLASS ? 0 : sizeClass - BASE_SIZE_CLASS;
        return sizeClass;
    }

    /// For a size that is being freed or added to the free list.
    constexpr static size_t calculateSizeClassIncoming(size_t n) {
        size_t sizeClass = std::countr_zero(std::bit_floor(n));
        assert(sizeClass >= BASE_SIZE_CLASS);
        sizeClass = sizeClass <= BASE_SIZE_CLASS ? 0 : sizeClass - BASE_SIZE_CLASS;
        return sizeClass;
    }

    template <typename T>
    Allocation<T> allocate(size_t n) {
        static_assert(alignof(T) <= MIN_ALLOC_ALIGN);
        auto allocation = allocateSpace(n * sizeof(T));
        stats.countUsed(allocation.size);
        return allocation.as<T>();
    }

    Allocation<std::byte> allocateSpaceLarge(size_t bytes) {
        // TODO: Check the `large` list.
        auto allocation = allocateFromParent(bytes);
        return allocation;
    }

    Allocation<std::byte> allocateSpaceSmall(size_t bytes, size_t sizeClass) {
        bytes = bytes <= MIN_ALLOCATION_SIZE ? std::bit_ceil(bytes) : bytes;
        Block *next = classes[sizeClass];

        if (next) {
            classes[sizeClass] = next->next;

            return Allocation{(std::byte *) next, next->size};
        } else {
            for (size_t i = sizeClass; i < CLASSES_COUNT; i++) {
                if (classes[i]) {
                    Block *block = classes[i];
                    classes[i] = block->next;
                    assert(block->size >= bytes);
                    sliceRemainderFromBlock(block, block->size, bytes);

                    return Allocation{(std::byte *) block, u32(bytes)};
                }
            }
            
            if (bytes >= MIN_ALLOCATION_SIZE) {
                return allocateSpaceLarge(bytes);
            }

            // TODO: At this point, we might want to increase allocation size after a given number of allocations.

            auto chunk = allocateNewChunk();

            if ((chunk.size - bytes) < BASE_SIZE) {
                return chunk;
            } else {
                sliceRemainderFromBlock(chunk.space, chunk.size, bytes);

                return Allocation{chunk.space, u32(bytes)};
            }
        }
    }

    Allocation<std::byte> allocateSpace(size_t bytes) {
        size_t alignedBytes = normalizeAllocationSize(bytes);
        size_t sizeClass = calculateSizeClassOutgoing(alignedBytes);
        Block *next = classes[sizeClass];

        if (sizeClass >= CLASSES_COUNT) [[unlikely]] {
            return allocateSpaceLarge(alignedBytes);
        } else {
            return allocateSpaceSmall(alignedBytes, sizeClass);
        }

    }

    template <typename T>
    void deallocate(Allocation<T> allocation) {
        stats.countDeallocated(allocation.size);
        if (allocation.index != ~0U) [[unlikely]] {
            stats.countFreed(allocation.size);

            free(allocation.space);
            allocations[allocation.index] = nullptr;
            return;
        } else {
            size_t sizeClass = calculateSizeClassIncoming(allocation.size);

            if (sizeClass < CLASSES_COUNT) [[likely]] {
                addBlock(allocation.space, allocation.size, sizeClass);
            } else {
                addLargeBlock(allocation.space, allocation.size);
            }
        }
    }

    void reclaim(std::byte *memory, size_t size) {
        std::byte *aligned = (std::byte *) alignAddress(memory);
        size_t lost = aligned - memory;

        if (lost >= size) {
            return;
        }

        size_t alignedSize = size - lost;

        if (alignedSize < BASE_SIZE) {
            return;
        }

        stats.countReclaimed(alignedSize);

        size_t sizeClass = calculateSizeClassIncoming(alignedSize);

        if (sizeClass < CLASSES_COUNT) [[likely]] {
            addBlock(aligned, alignedSize, sizeClass);
        } else {
            addLargeBlock(aligned, alignedSize);
        }
    }

    ~ArrayArenaAllocator() {
        stats.print();

        for (auto *allocation : allocations) {
            free(allocation);
        }
    }
};

#endif // LANG_array_allocator_h
