#ifndef LANG_scanner_h
#define LANG_scanner_h

#include <string>
#include <string_view>

enum class TokenType : uint8_t {
    // Brackets
    /// []
    LeftBrace, RightBrace,
    /// {}
    LeftBracket, RightBracket,
    LeftParenthesis, RightParenthesis,

    // Symbols
    Comma, 
    Dot,
    DotDot,
    Colon,
    Semicolon,
    Arrow,
    Equal,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Power, // TODO: Rename to caret

    Ampersand,

    PlusEqual, 
    MinusEqual,
    StarEqual, 
    SlashEqual,


    // Logical operators
    Not,
    And,
    Or,

    // Comparison
    NotEqual,
    EqualEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    // Keywords
    Identifier,
    Self,
    Enum,
    Struct,
    Let,
    Var,
    If,
    Else,
    Fn,
    For,
    While,
    Repeat,
    Break,
    Return,

    // Literals
    True,
    False,
    Integer, 
    Floating, 
    Hexadecimal,
    Binary,
    String,

    EndOfFile,
};

std::ostream& operator<<(std::ostream& os, TokenType tokenType);

struct Token {
public:
    int line;
    int offset;
    std::string_view chars;
    TokenType type;

    Token(TokenType type, std::string_view chars, int line, int offset) 
        : type{type}, chars{chars}, line{line}, offset{offset} {}
};

class ScannerException : public std::exception {
public:
    enum class Cause {
        ExpectedDigit,
        UnterminatedString,
        UnrecognizedCharacter,
    };

private:
    Cause cause;
    int line;
    int offset;

public:
    ScannerException(Cause cause, int line, int offset) : cause{cause}, line{line}, offset{offset} {}
};

class Scanner {
private:
    using iterator = std::string::const_iterator;
    std::string _string;
    iterator start;
    iterator current;
    iterator end;
    int line;
    int offset;

    bool isAtEnd() {
        return current == end;
    }

    char advance() {
        offset += 1;
        return *current++;
    }

    void newline() {
        line += 1;
        offset = -1;
    }

    char peek() {
        return *current;
    }

    char peekNext() {
        return *(current + 1);
    }

    void skipWhitespace();
    template <auto Predicate> void munchMany();
    template <auto predicate, ScannerException::Cause cause> void munchMany1();
    [[nodiscard]]
    Token makeToken(TokenType type);
    TokenType identifierType();
    Token identifier();
    Token string();
    Token number(char first);

public:
    Scanner(std::string&& string) : _string{std::move(string)}, start{_string.cbegin()}, current{start}, end{_string.cend()}, line{1}, offset{0} {
//        _string = std::move(string);
//        start = _string.cbegin();
//        current = start;
//        end = _string.cend();
//        line = 1;
//        offset = 0;
    };

    Token next();
};

#endif
