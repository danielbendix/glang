#ifndef LANG_small_byte_array_h
#define LANG_small_byte_array_h

#include "common.h"
#include "memory.h"

#include <span>

/// Small read-only array.
/// Allows storing 8 bytes of information without indirection.
template <typename T>
requires (sizeof(T) == 1 && sizeof(T *) == sizeof(size_t))
class SmallByteArray {
    struct Array {
        T *data;
    };

    static constexpr size_t BYTES = sizeof(T *);
    static constexpr size_t LBO = BYTES / sizeof(T);

    struct Storage {
        size_t size;
        union {
            T array[LBO];
            T *ptr;
        } data;
    };
    Storage storage;

    bool isLBO() const {
        return storage.size <= LBO;
    }

public:
    SmallByteArray(std::vector<T>& data) {
        storage.size = data.size();
        if (data.size() <= LBO) {
            memcpy(storage.data.array, data.data(), sizeof(T) * data.size());
        } else {
            storage.data.ptr = new T[data.size()];
            memcpy(storage.data.ptr, data.data(), sizeof(T) * data.size());
        }
    }

    template <Allocator Allocator>
    SmallByteArray(Allocator& allocator, std::vector<T>& data) {
        storage.size = data.size();
        if (data.size() <= LBO) {
            memcpy(storage.data.array, data.data(), sizeof(T) * data.size());
        } else {
            storage.data.ptr = (T *) allocator.allocate(sizeof(T) * data.size(), alignof(T));
            memcpy(storage.data.ptr, data.data(), sizeof(T) * data.size());
        }
    }

    SmallByteArray(std::span<T> data) {
        storage.size = data.size();
        if (data.size() <= LBO) {
            memcpy(storage.data.array, data.data(), sizeof(T) * data.size());
        } else {
            storage.data.ptr = new T[data.size()];
            memcpy(storage.data.ptr, data.data(), sizeof(T) * data.size());
        }
    }

    ~SmallByteArray() {
        if (!isLBO()) {
            delete[] storage.data.ptr;
        }
    }

    size_t size() const {
        return storage.size;
    }

    T operator[](size_t i) const {
        if (isLBO()) {
            return storage.data.array[i];
        } else {
            return storage.data.ptr[i];
        }
    }

    class iterator {
        T *current;

        iterator(T *current) : current{current} {}
    public:
        T operator*() {
            return *current;
        }

        iterator& operator++() {
            ++current;
            return *this;
        }

        iterator operator++(int) {
            const iterator it = *this;
            current++;
            return it;
        }

        bool operator==(const iterator it) {
            return current == it.current;
        }

        bool operator!=(const iterator it) {
            return current != it.current;
        }

        friend class SmallByteArray<T>;
    };

    iterator begin() const {
        if (isLBO()) {
            return iterator((T *)storage.data.array);
        } else {
            return iterator(storage.data.ptr);
        }
    }

    iterator end() const {
        if (isLBO()) {
            return iterator((T *) storage.data.array + storage.size);
        } else {
            return iterator(storage.data.ptr + storage.size);
        }
    }
};

#endif // LANG_small_byte_array_h
