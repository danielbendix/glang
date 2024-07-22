#ifndef LANG_token_h
#define LANG_token_h

#include <string_view>

enum class TokenType : uint8_t {
    // Brackets
    /// []
    LeftBrace, RightBrace,
    /// {}
    LeftBracket, RightBracket,
    /// ()
    LeftParenthesis, RightParenthesis,

    // Symbols
    Comma, 
    Dot,
    DotDot,
    DotDotDot,
    DotDotLess,
    Colon,
    Semicolon,
    Arrow,
    Bang,
    Question,
    Equal,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    LessLess,
    GreaterGreater,

    Tilde,
    Ampersand,
    Caret,
    Pipe,

    PlusEqual, 
    MinusEqual,
    StarEqual, 
    SlashEqual,
    PercentEqual,

    // Logical operators
    Not,
    And,
    Or,

    // Comparison
    BangEqual,
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
    Case,
    If,
    Else,
    Init,
    Fn,
    For,
    In,
    Guard,
    While,
    Where,
    Repeat,
    Break,
    Return,

    LastKeyWord = Return,

    Try,
    TryQuestion,
    TryBang,
    Throw,

    // Literals
    True,
    False,
    Nil,
    Integer, 
    Floating, 
    Binary,
    Octal,
    Hexadecimal,
    String,

    Error,

    EndOfFile,
};

std::ostream& operator<<(std::ostream& os, TokenType tokenType);

struct Token final {
    TokenType type;
    int line;
    int column;
    int length;
    // Since we're storing the length (in bytes), we could compute the
    // string_view from a char * and the length;
    std::string_view chars;

    Token(TokenType type, std::string_view chars, int line, int column, int length) 
        : type{type}, chars{chars}, line{line}, column{column}, length{length} {}
};

#endif // LANG_token_h
