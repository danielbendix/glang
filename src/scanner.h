#ifndef LANG_scanner_h
#define LANG_scanner_h

#include "token.h"

#include <cassert>
#include <string>
#include <vector>

class Scanner {
public:
    enum class ErrorCause;
    static const ErrorCause NO_ERROR;

    // TODO: Maybe we don't need to move the string here.
    Scanner(std::string&& string) : _string{std::move(string)}, start{_string.cbegin()}, current{start}, end{_string.cend()} {}

    Token next() noexcept;

    ErrorCause getErrorCause() const noexcept {
        assert(_error != NO_ERROR);
        return _error;
    }
private:
    using iterator = std::string::const_iterator;
    std::vector<u32> lineBreaks;
    std::string _string;
    iterator start;
    iterator current;
    iterator end;
    u32 line = 1;
    u32 column = 0;
    u32 startLine = line;
    u32 startColumn = column;
    ErrorCause _error = NO_ERROR;

    bool isAtEnd() {
        return current == end;
    }

    void setStart() {
        start = current;
        startLine = line;
        startColumn = column;
    }

    char advance() {
        column += 1;
        return *current++;
    }

    void newline() {
        auto offset = current - start;
        assert(offset <= UINT32_MAX);
        lineBreaks.push_back(offset);
        line += 1;
        column = -1;
    }

    char peek() {
        return *current;
    }

    char peekNext() {
        return *(current + 1);
    }

    void multilineComment();
    void skipWhitespace();
    template <char c, TokenType ifRead, TokenType ifNotRead>Token checkNext();
    template <auto predicate> void munchMany();
    template <auto predicate> bool munchMany1();
    [[nodiscard]]
    Token makeToken(TokenType type);
    TokenType testTry(std::string::const_iterator it, std::string::const_iterator end);
    TokenType identifierType();
    Token identifier();
    Token hashIdentifier();
    Token escapedIdentifier();
    Token character();
    Token string();
    Token number(char first);

    [[nodiscard]]
    Token errorToken();
    [[nodiscard]]
    Token errorToken(ErrorCause cause);
    void error(ErrorCause errorType);
    void error(ErrorCause errorType, const std::string& message);

    friend class Parser;
};

enum class Scanner::ErrorCause {
    None = 0,
    InvalidIntegerLiteralPrefix,
    EmptyBinaryLiteral,
    EmptyOctalLiteral,
    EmptyHexadecimalLiteral,
    EmptyFloatingPointFraction,
    EmptyFloatingPointExponent,
    EmptyCharacterLiteral,
    UnterminatedCharacterLiteral,
    UnterminatedStringLiteral,
    UnterminatedBlockComment,
    UnrecognizedCharacter,
};

#endif
