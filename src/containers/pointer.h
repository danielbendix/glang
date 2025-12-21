#include <cstddef>
#include <concepts>

/// This file implements wrappers around pointers. This yields two benefits:
/// - Portable verification of pointer nullability.
/// - Disabling default initialization for pointers.
///
/// This should primarily be used as a storage type.

template<typename T>
class nullable {
    T *value;

    nullable(std::nullptr_t) : value{nullptr} {}
    nullable(T *pointer) : value{pointer} {}

    nullable& operator=(T *value) {
        this->value = value;
        return *this;
    }

    T& operator*() const {
        return *value;
    }

    T *operator->() const {
        return value;
    }
};

template<typename T>
requires(std::is_pointer_v<T>)
class nonnull {
    T *value;

    nonnull(T *value) : value{value} {
        assert(value && "Pointer must not be null.");
    }

    nonnull& operator=(T *value) {
        assert(value && "Pointer must not be null.");
        this->value = value;
        return *this;
    }

    T& operator*() const {
        assert(value && "Pointer must not be null.");
        return *value;
    }

    T *operator->() const {
        assert(value && "Pointer must not be null.");
        return value;
    }
};
