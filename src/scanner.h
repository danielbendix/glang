#ifndef LANG_scanner_h
#define LANG_scanner_h

#include "token.h"

#include <cassert>
#include <string>

class Scanner {
public:
    enum class ErrorCause;
    static const ErrorCause NO_ERROR;

    // TODO: Maybe we don't need to move the string here.
    Scanner(std::string&& string) : _string{std::move(string)}, start{_string.cbegin()}, current{start}, end{_string.cend()}, line{1}, column{0} {}

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
    int column;
    ErrorCause _error = NO_ERROR;

    bool isAtEnd() {
        return current == end;
    }

    char advance() {
        column += 1;
        return *current++;
    }

    void newline() {
        line += 1;
        column = 0;
    }

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
    TokenType testTry(std::string::const_iterator it, std::string::const_iterator end);
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
