#ifndef LANG_common_h
#define LANG_common_h

#include "templates.h"
#include <vector>


enum class PassResultKind {
    OK = 0,
    ERROR = 1,
};

class PassResult {
    PassResultKind kind;

public:
    PassResult(PassResultKind kind) : kind{kind} {}

    bool ok() const {
        return kind == PassResultKind::OK;
    }

    bool failed() const {
        return kind != PassResultKind::OK;
    }

    // TODO: Consider implicit conversion to bool, where error is true,
    // Just like functions that return 0 on success.

    friend PassResult operator|(PassResult lhs, PassResult rhs);
    friend PassResult& operator|=(PassResult& lhs, PassResult rhs);
};

template <typename T, void (*deleter)(T*) = T::deleteValue>
struct Deleter {
    void operator()(T *value) const {
        deleter(value);
    }
};


template <typename T, typename Base = Templates::DeleteValueArgType_t<T>>
using unique_ptr_t = std::unique_ptr<T, Deleter<Base>>;

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

#endif // LANG_common_h
