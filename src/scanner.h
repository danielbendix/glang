#ifndef LANG_scanner_h
#define LANG_scanner_h

#include "token.h"

#include <cassert>
#include <string>

//class ScannerException : public std::exception {
//public:
//    enum class Cause {
//        ExpectedDigit,
//        UnterminatedString,
//        UnrecognizedCharacter,
//    };
//
//private:
//    Cause cause;
//    int line;
//    int offset;
//
//public:
//    ScannerException(Cause cause, int line, int offset) : cause{cause}, line{line}, offset{offset} {}
//};

class Scanner {
public:
    enum class ErrorCause;
    static const ErrorCause NO_ERROR;

    // TODO: Maybe we don't need to move the string here.
    Scanner(std::string&& string) : _string{std::move(string)}, start{_string.cbegin()}, current{start}, end{_string.cend()}, line{1}, offset{0} {}

    Token next() noexcept;

    ErrorCause getErrorCause() const noexcept {
        assert(_error != NO_ERROR);
        return _error;
    }
private:
    using iterator = std::string::const_iterator;
    std::string _string;
    iterator start;
    iterator current;
    iterator end;
    int line;
    int offset;
    ErrorCause _error;

    bool isAtEnd() {
        return current == end;
    }

    char advance() {
        offset += 1;
        return *current++;
        int a = 0x3;
    }

    void newline() {
        line += 1;
        offset = -1;
    }

//    void markStart() {
//        start = current;
//        // TODO: Set starting location
//    }

    char peek() {
        return *current;
    }

    char peekNext() {
        return *(current + 1);
    }

    void multilineComment();
    void skipWhitespace();
    template <auto Predicate> void munchMany();
    template <auto predicate> bool munchMany1();
    [[nodiscard]]
    Token makeToken(TokenType type);
    TokenType identifierType();
    Token identifier();
    Token escapedIdentifier();
    Token string();
    Token number(char first);

    [[nodiscard]]
    Token errorToken();
    [[nodiscard]]
    Token errorToken(ErrorCause cause);
    void error(ErrorCause errorType);
    void error(ErrorCause errorType, const std::string& message);
};

enum class Scanner::ErrorCause {
    None = 0,
    InvalidIntegerLiteralPrefix,
    EmptyBinaryLiteral,
    EmptyOctalLiteral,
    EmptyHexadecimalLiteral,
    EmptyFloatingPointFraction,
    EmptyFloatingPointExponent,
    UnterminatedStringLiteral,
    UnterminatedBlockComment,
    UnrecognizedCharacter,
};

#endif
