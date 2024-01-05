#include <memory>

template <typename T>
class unique_ref final {
private:
    std::unique_ptr<T> pointer;

public:
    T& get() const {
        return *pointer.get();
    }

    const T& cget() const {
        return *pointer.get();
    }




    

};

class Test {
    unique_ref<int> i;
};
