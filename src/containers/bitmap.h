#ifndef LANG_containers_bitmap_h
#define LANG_containers_bitmap_h

#include "common.h"

#include <cassert>
#include <span>
#include <ranges>

class Bitmap final {
    static constexpr size_t LBO = 1;

    uint32_t size;
    uint32_t blocks;

    uint64_t *data;
    uint64_t lbo[LBO];

    bool isLBO() const {
        return blocks < LBO;
    }
public:
    Bitmap(uint32_t size) {
        this->size = size;
        blocks = block_count(size);

        if (blocks <= LBO) [[likely]] {
            std::memset(lbo, 0, sizeof(uint64_t) * LBO);
            data = lbo;
        } else {
            data = new uint64_t[blocks];
            std::memset(data, 0, sizeof(uint64_t) * blocks);
        }
    }

    Bitmap(uint64_t *data, uint32_t size) {
        this->size = size;
        blocks = size >> 6;

        if (blocks <= 1) [[likely]] {
            std::memcpy(this->data, data, sizeof(uint64_t) * blocks);
            data = lbo;
        } else {
            this->data = new uint64_t[blocks];
            std::memcpy(this->data, data, sizeof(uint64_t) * blocks);
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
            memcpy(lbo, other.lbo, sizeof(uint64_t) * LBO);
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

    bool set(size_t index) {
        size_t block = index >> 6;
        size_t offset = index & 0x3F;

        assert(block < blocks);

        uint8_t current = (data[block] >> offset) % 1;
        data[block] |= 1 << offset;

        return !current;
    }

    void negate() {
        for (uint64_t& segment : std::span{data, blocks}) {
            segment = ~segment;
        }
    }
    
    Bitmap& operator |=(const std::span<const uint64_t> rhs) {
        assert(blocks == rhs.size());

        for (size_t i = 0; i < blocks; ++i) {
            data[i] |= rhs[i];
        }

        return *this;
    }

    Bitmap& operator &=(const std::span<const uint64_t> rhs) {
        assert(blocks == rhs.size());

        for (size_t i = 0; i < blocks; ++i) {
            data[i] &= rhs[i];
        }

        return *this;
    }

    Bitmap& operator ^=(const std::span<const uint64_t> rhs) {
        assert(blocks == rhs.size());

        for (size_t i = 0; i < blocks; ++i) {
            data[i] &= rhs[i];
        }

        return *this;
    }

    uint32_t countr_ones() const {
        uint32_t result = 0;
        for (size_t i = 0; i < blocks; ++i) {
            uint32_t ones = std::countr_one(data[i]);
            result += ones;
            if (ones < 64) {
                break;
            }
        }

        return result;
    }

    template <typename Func>
    void iterate_zeros(Func&& f) {
        uint32_t i = 0;

        for (auto segment : std::span{data, blocks}) {
            size_t j = i;

            uint32_t max = std::min(size, i + 64);
            for (uint32_t j = i; j < max; j++, segment >>= 1) {
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
        uint32_t i = 0;

        for (auto segment : std::span{data, blocks}) {
            size_t j = i;

            uint32_t max = std::min(size, i + 64);
            for (uint32_t j = i; j < max; j++, segment >>= 1) {
                // TODO: use countl_zero on segment to do fewer iterations.
                if ((segment & 1) == 1) {
                    f(j);
                }
            }

            i += 64;
        }
    }

    static constexpr uint32_t block_count(size_t size) {
        return (size + 0x3F) >> 6;

    }
};

#endif // LANG_containers_bitmap_h
