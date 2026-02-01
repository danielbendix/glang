#ifndef LANG_containers_bitmap_h
#define LANG_containers_bitmap_h

#include "common.h"

#include <cassert>
#include <cstring>
#include <span>
#include <ranges>

class Bitmap final {
    static constexpr size_t LBO = 1;

    u32 size;
    u32 blocks;

    u64 *data;
    u64 lbo[LBO];
public:
    static inline constexpr u32 BLOCK_SIZE = 64;
    static inline constexpr u32 BLOCK_MASK = BLOCK_SIZE - 1;

    [[nodiscard]]
    static constexpr u32 BLOCK_COUNT(u32 size) {
        return (size + BLOCK_MASK) / BLOCK_SIZE;
    }

    [[nodiscard]]
    static constexpr u32 BLOCK_INDEX(u32 index) {
        return index / BLOCK_SIZE;
    }

    [[nodiscard]]
    static constexpr u32 BLOCK_OFFSET(u32 index) {
        return index & BLOCK_MASK;
    }
private:
    [[nodiscard]]
    bool isLBO() const {
        return blocks < LBO;
    }

    [[nodiscard]]
    std::span<u64> as_span() const {
        return {data, blocks};
    }
public:
    Bitmap(u32 size) {
        this->size = size;
        blocks = BLOCK_COUNT(size);

        if (isLBO()) [[likely]] {
            std::memset(lbo, 0, sizeof(u64) * LBO);
            data = lbo;
        } else {
            data = new u64[blocks];
            std::memset(data, 0, sizeof(u64) * blocks);
        }
    }

    Bitmap(u64 *data, u32 size) {
        this->size = size;
        blocks = BLOCK_COUNT(size);

        if (isLBO()) [[likely]] {
            std::memcpy(this->data, data, sizeof(u64) * blocks);
            data = lbo;
        } else {
            this->data = new u64[blocks];
            std::memcpy(this->data, data, sizeof(u64) * blocks);
        }
    }

    Bitmap(const Bitmap&) = delete;
    Bitmap(Bitmap&&) = delete;
    Bitmap& operator=(const Bitmap&) = delete;
    Bitmap& operator=(Bitmap&& other) noexcept {
        this->~Bitmap();

        size = other.size;
        blocks = other.blocks;
        if (other.isLBO()) {
            memcpy(lbo, other.lbo, sizeof(u64) * LBO);
            data = lbo;
        } else {
            data = other.data;
        }
        other.size = 0;
        other.blocks = 0;
        other.data = nullptr;

        return *this;
    }

    ~Bitmap() {
        if (!isLBO()) {
            delete[] data;
        }
    }

    u32 count() const {
        return size;
    }

    void copyInto(u64 *buffer) const {
        memcpy(buffer, data, blocks * sizeof(u64));
    }

    bool set(size_t index) {
        size_t block = BLOCK_INDEX(index);
        size_t offset = BLOCK_OFFSET(index);

        assert(block < blocks);

        u8 current = (data[block] >> offset) & 1;
        data[block] |= 1 << offset;

        return current == 0;
    }

    void negate() {
        for (u64& segment : as_span()) {
            segment = ~segment;
        }
    }
    
    Bitmap& operator |=(const std::span<const u64> rhs) {
        assert(blocks == rhs.size());

        for (size_t i = 0; i < blocks; ++i) {
            data[i] |= rhs[i];
        }

        return *this;
    }

    Bitmap& operator &=(const std::span<const u64> rhs) {
        assert(blocks == rhs.size());

        for (size_t i = 0; i < blocks; ++i) {
            data[i] &= rhs[i];
        }

        return *this;
    }

    Bitmap& operator ^=(const std::span<const u64> rhs) {
        assert(blocks == rhs.size());

        for (size_t i = 0; i < blocks; ++i) {
            data[i] &= rhs[i];
        }

        return *this;
    }

    [[nodiscard]]
    u32 countr_ones() const {
        u32 result = 0;
        for (size_t i = 0; i < blocks; ++i) {
            u32 ones = std::countr_one(data[i]);
            result += ones;
            if (ones < BLOCK_SIZE) {
                break;
            }
        }

        return result;
    }

    template <typename Func>
    void iterate_zeros(Func&& func) {
        u32 i = 0;

        for (auto segment : as_span()) {
            u32 max = std::min(size, i + BLOCK_SIZE);
            for (u32 j = i; j < max; j++, segment >>= 1) {
                // TODO: use countr_one on segment to do fewer iterations.
                if ((segment & 1) == 0) {
                    func(j);
                }
            }

            i += BLOCK_SIZE;
        }
    }

    template <typename Func>
    void iterate_ones(Func&& func) {
        u32 i = 0;

        for (auto segment : as_span()) {
            u32 max = std::min(size, i + BLOCK_SIZE);
            for (u32 j = i; j < max; j++, segment >>= 1) {
                // TODO: use countr_zero on segment to do fewer iterations.
                if ((segment & 1) == 1) {
                    func(j);
                }
            }

            i += BLOCK_SIZE;
        }
    }
};

#endif // LANG_containers_bitmap_h
