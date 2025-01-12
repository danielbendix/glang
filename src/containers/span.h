#ifndef LANG_container_span_h
#define LANG_container_span_h

#include "common.h"
#include "array_allocator.h"

namespace span_iterator {
    template <typename T>
    concept ShouldCopyWhenIterating =
        std::is_trivially_copyable_v<T> && (sizeof(T) <= sizeof(void*));

    template <typename T, bool Const>
    class DereferencingIterator;

    template <typename T, bool Const>
    class Iterator {
        T *it;
    public:
        using pointer = T*;
        using value_type = T;
        using difference_type = std::ptrdiff_t;
        using iterator_category = std::random_access_iterator_tag;
        using reference_type = std::conditional_t<Const, const T&, T&>;

        using dereference_type = std::conditional_t<
            ShouldCopyWhenIterating<T>,
            T,
            reference_type
        >;

        using dereferencing_iterator = DereferencingIterator<typename std::remove_pointer_t<T>, Const>;

        Iterator(T *it) : it{it} {}

        dereference_type operator*() const {
            return *it;
        }

        Iterator operator++() {
            ++it;
            return *this;
        }

        Iterator operator--() {
            --it;
            return *this;
        }

        Iterator operator+=(difference_type n) {
            it += n;
            return *this;
        }

        Iterator operator-=(difference_type n) {
            it -= n;
            return *this;
        }

        Iterator operator+(difference_type n) const {
            return Iterator(it + n);
        }

        Iterator operator-(difference_type n) const {
            return Iterator(it - n);
        }

        bool operator==(const Iterator other) const {
            return it == other.it;
        }

        bool operator!=(const Iterator other) const {
            return it != other.it;
        }
    };

    template <typename T, bool Const>
    class DereferencingIterator {
    public:
        explicit DereferencingIterator(Iterator<T *, Const> inner) : inner(inner) {}

        using difference_type = std::ptrdiff_t;

        using dereference_type = std::conditional_t<
            Const,
            const T&,
            T&
        >;

        // Dereference operator: dereference the pointer value
        dereference_type operator*() const {
            return **inner;
        }

        // Forward operations to the inner iterator
        DereferencingIterator& operator++() {
            ++inner;
            return *this;
        }

        DereferencingIterator& operator--() {
            --inner;
            return *this;
        }

        DereferencingIterator& operator+=(difference_type n) {
            inner += n;
            return *this;
        }

        DereferencingIterator& operator-=(difference_type n) {
            inner -= n;
            return *this;
        }

        DereferencingIterator operator+(difference_type n) const {
            return DereferencingIterator(inner + n);
        }

        DereferencingIterator operator-(difference_type n) const {
            return DereferencingIterator(inner - n);
        }

        bool operator==(const DereferencingIterator& other) const {
            return inner == other.inner;
        }

        bool operator!=(const DereferencingIterator& other) const {
            return inner != other.inner;
        }

    private:
        Iterator<T *, Const> inner;
    };

}

template <typename T>
class Span final {
    static_assert(std::is_trivially_copyable<T>::value, "Span should only be instantiated with a trivially copyable type.");

    T *elements;
    u32 _size;
public:
    using iterator = span_iterator::Iterator<T, false>;
    using const_iterator = span_iterator::Iterator<T, true>;

    Span()
        : elements{nullptr}, _size{0} {}

    Span(T *elements, u32 size)
        : elements{elements}, _size{size} {}

    u32 size() const {
        return _size;
    }

    T& operator[](u32 index) {
        assert(index < _size);
        return elements[index];
    }

    const T& operator[](u32 index) const {
        assert(index < _size);
        return elements[index];
    }
    
    bool isEmpty() const {
        return _size == 0;
    }

    void shrinkTo(u32 newSize) {
        assert(newSize <= _size);
        _size = newSize;
    }

    iterator begin() {
        return iterator(elements);
    }

    iterator end() {
        return iterator(elements + _size);
    }

    const_iterator begin() const {
        return const_iterator(elements);
    }

    const_iterator end() const {
        return const_iterator(elements + _size);
    }

    const_iterator cbegin() const {
        return begin();
    }

    const_iterator cend() const {
        return end();
    }
};

template <typename T>
class GrowingSpan final {
    using Allocator = ArrayArenaAllocator;

    static_assert(std::is_trivially_copyable<T>::value, "GrowingSpan should only be instantiated with a trivially copyable type.");

    u32 size = 0;
    u32 capacity = 0;
    T *elements = nullptr;

    // This could be replaced with allocation size in bytes and allocation index.
    Allocator::Allocation<T> allocation{nullptr, 0};
    Allocator& allocator;

    /// Copy elements into a buffer that is at least as large as elements.
    void copyElementsInto(T *buffer) {
        std::memcpy(buffer, elements, sizeof(T) * size);
    }

    void growToCapacity(u32 newCapacity) {
        assert(newCapacity > capacity);
        auto newAllocation = allocator.allocate<T>(newCapacity);

        if (elements) {
            copyElementsInto(newAllocation.space);
            allocator.deallocate(allocation);
        }

        allocation = newAllocation;

        assert((allocation.size / sizeof(T)) >= newCapacity);

        elements = allocation.space;
        capacity = allocation.size / sizeof(T);
    }

    void growElements() {
        auto newCapacity = capacity == 0 ? 8 : capacity * 2;
        growToCapacity(newCapacity);
    }

public:
    GrowingSpan(Allocator& allocator)
        : allocator{allocator} {}

    GrowingSpan(GrowingSpan&& other) 
        : size{other.size}
        , capacity{other.capacity}
        , elements{other.elements}
        , allocator{other.allocator}
        {
            other.size = 0;
            other.capacity = 0;
            other.elements = nullptr;
        }

    GrowingSpan& operator=(GrowingSpan&) = delete;
    GrowingSpan& operator=(GrowingSpan&& other) = delete;

    void reserve(u32 requestedCapacity) {
        if (capacity >= requestedCapacity) {
            return;
        }
        growToCapacity(requestedCapacity);
    }

    void append(T value) {
        if (size + 1 > capacity) {
            growElements();
        }
        elements[size] = value;
        size += 1;
    }

    Span<T> freeze() {
        if (size == 0) {
            if (elements) {
                allocator.deallocate(allocation);
            }
            return Span<T>{nullptr, 0};
        } else {
            auto *elementsEnd = (std::byte *)(elements + size);
            auto *allocationEnd = ((std::byte *) allocation.space) + allocation.size;
            allocator.reclaim(elementsEnd, allocationEnd - elementsEnd);
            auto span = Span<T>{elements, size};
            elements = nullptr;
            size = 0;
            capacity = 0;
            return span;
        }
    }

    ~GrowingSpan() {
        if (elements) {
            allocator.deallocate(allocation);
        }
    }
};

#endif // LANG_container_span_h
