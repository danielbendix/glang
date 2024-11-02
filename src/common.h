#ifndef LANG_common_h
#define LANG_common_h

#include "templates.h"
#include <vector>

#ifdef __clang__
    #define NULLABLE _Nullable
    #define NONNULL _Nonnull
#else
    #define NULLABLE
    #define NONNULL
#endif

enum class PassResultKind : uint8_t {
    OK = 0,
    ERROR = 1,
};

class PassResult {
    PassResultKind kind;

public:
    PassResult(PassResultKind kind) : kind{kind} {}

    void error_if_false(bool value) {
        if (!value) {
            kind = PassResultKind::ERROR;
        }
    }

    void error_if_true(bool value) {
        if (value) {
            kind = PassResultKind::ERROR;
        }
    }

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
