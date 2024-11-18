#ifndef LANG_containers_bitmap_h
#define LANG_containers_bitmap_h

#include "common.h"

#include <cassert>
#include <span>
#include <ranges>

class Bitmap final {
    static constexpr size_t LBO = 1;

    u32 size;
    u32 blocks;

    u64 *data;
    u64 lbo[LBO];

    bool isLBO() const {
        return blocks < LBO;
    }
public:
    Bitmap(u32 size) {
        this->size = size;
        blocks = block_count(size);

        if (blocks <= LBO) [[likely]] {
            std::memset(lbo, 0, sizeof(u64) * LBO);
            data = lbo;
        } else {
            data = new u64[blocks];
            std::memset(data, 0, sizeof(u64) * blocks);
        }
    }

    Bitmap(u64 *data, u32 size) {
        this->size = size;
        blocks = size >> 6;

        if (blocks <= 1) [[likely]] {
            std::memcpy(this->data, data, sizeof(u64) * blocks);
            data = lbo;
        } else {
            this->data = new u64[blocks];
            std::memcpy(this->data, data, sizeof(u64) * blocks);
        }
    }

    // FIXME: This is a bug
    Bitmap(const Bitmap&) = delete;
    Bitmap(Bitmap&&) = delete;
    Bitmap& operator=(const Bitmap&) = delete;
    Bitmap& operator=(Bitmap&& other) {
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
        if (isLBO()) {
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
        size_t block = index >> 6;
        size_t offset = index & 0x3F;

        assert(block < blocks);

        u8 current = (data[block] >> offset) % 1;
        data[block] |= 1 << offset;

        return !current;
    }

    void negate() {
        for (u64& segment : std::span{data, blocks}) {
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

    u32 countr_ones() const {
        u32 result = 0;
        for (size_t i = 0; i < blocks; ++i) {
            u32 ones = std::countr_one(data[i]);
            result += ones;
            if (ones < 64) {
                break;
            }
        }

        return result;
    }

    template <typename Func>
    void iterate_zeros(Func&& f) {
        u32 i = 0;

        for (auto segment : std::span{data, blocks}) {
            size_t j = i;

            u32 max = std::min(size, i + 64);
            for (u32 j = i; j < max; j++, segment >>= 1) {
                // TODO: use countl_one on segment to do fewer iterations.
                if ((segment & 1) == 0) {
                    f(j);
                }
            }

            i += 64;
        }
    }

    template <typename Func>
    void iterate_ones(Func&& f) {
        u32 i = 0;

        for (auto segment : std::span{data, blocks}) {
            size_t j = i;

            u32 max = std::min(size, i + 64);
            for (u32 j = i; j < max; j++, segment >>= 1) {
                // TODO: use countl_zero on segment to do fewer iterations.
                if ((segment & 1) == 1) {
                    f(j);
                }
            }

            i += 64;
        }
    }

    static constexpr u32 block_count(size_t size) {
        return (size + 0x3F) >> 6;

    }
};

#endif // LANG_containers_bitmap_h
