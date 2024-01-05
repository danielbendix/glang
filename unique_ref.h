#include <memory>

template <typename T>
class unique_ref final {
private:
    std::unique_ptr<T> pointer;
public:
    unique_ref(T& ref) : pointer{&ref} {}

    T* operator->() {
        return &get();
    }

    operator T&() const {
        return *this;
    }

    T& get() const {
        return *pointer.get();
    }

    const T& cget() const noexcept {
        return *pointer.get();
    }

    void reset(T& newRef) noexcept {
        pointer.reset(&newRef);
    }

    void swap(unique_ref<T>& other) {
        pointer.swap(other.pointer);
    }
};
