#include <vector>

template <typename T>
class Iterable {
    std::vector<T>& vector;

public:
    Iterable(std::vector<T>& vector) : vector{vector} {}

    std::vector<T>::iterator begin() {
        return vector.begin();
    }

    std::vector<T>::iterator end() {
        return vector.end();
    }

    std::vector<T>::const_iterator begin() const {
        return vector.begin();
    }

    std::vector<T>::const_iterator end() const {
        return vector.end();
    }

    std::vector<T>::const_iterator cbegin() const {
        return vector.cbegin();
    }

    std::vector<T>::const_iterator cend() const {
        return vector.cend();
    }
};

template <typename T>
class ConstIterable {
    const std::vector<T>& vector;

public:
    ConstIterable(const std::vector<T>& vector) : vector{vector} {}

    std::vector<T>::const_iterator begin() const {
        return vector.begin();
    }

    std::vector<T>::const_iterator end() const {
        return vector.end();
    }

    std::vector<T>::const_iterator cbegin() const {
        return vector.cbegin();
    }

    std::vector<T>::const_iterator cend() const {
        return vector.cend();
    }
};
