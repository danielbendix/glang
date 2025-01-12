#include "common.h"
#include "array_allocator.h"

class String final {
    using char_type = char;
    char_type *_data;
    u32 _size;
public:
    String(char_type *data, u32 size)
        : _data{data}, _size{size} {}

    char_type *data() const {
        return _data;
    }

    u32 size() const {
        return _size;
    }

    std::string_view stringView() {
        return std::string_view{_data, _data + _size};
    }

    static String empty;
};

class GrowingString {
    using Allocator = ArrayArenaAllocator;

    using char_type = char;

    u32 size = 0;
    u32 capacity = 0;
    char_type *data = nullptr;
    Allocator& allocator;
    Allocator::Allocation<char_type> allocation{nullptr, 0};

    /// Copy elements into a buffer that is at least as large as elements.
    void copyDataInto(char_type *buffer) {
        std::memcpy(buffer, data, sizeof(char_type) * size);
    }

    void growToCapacity(u32 newCapacity) {
        assert(newCapacity > capacity);
        auto newAllocation = allocator.allocate<char_type>(newCapacity);

        if (data) {
            copyDataInto(newAllocation.space);
            allocator.deallocate(allocation);
        }

        allocation = newAllocation;

        assert((allocation.size / sizeof(char_type)) >= newCapacity);

        data = allocation.space;
        capacity = allocation.size / sizeof(char_type);
    }

    void growData() {
        auto newCapacity = capacity == 0 ? 8 : capacity * 2;
        growToCapacity(newCapacity);
    }

public:
    GrowingString(Allocator& allocator) : allocator{allocator} {}

    void append(char c) {
        if ((size + 1) > capacity) {
            growData();
        }

        data[size] = c;
        size += 1;
    }

    void reserve(u32 requestedCapacity) {
        if (capacity >= requestedCapacity) {
            return;
        }
        growToCapacity(requestedCapacity);
    }

    String freeze() {
        if (size == 0) {
            if (data) {
                allocator.deallocate(allocation);
            }
            return String::empty;
        } else {
            auto *dataEnd = (std::byte *)(data + size);
            auto *allocationEnd = ((std::byte *) allocation.space) + allocation.size;
            allocator.reclaim(dataEnd, allocationEnd - dataEnd);
            auto string = String{data, size};
            data = nullptr;
            size = 0;
            capacity = 0;
            return string;
        }
    }

    ~GrowingString() {
        if (data) {
            allocator.deallocate(allocation);
        }
    }
};
