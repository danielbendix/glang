#ifndef LANG_token_h
#define LANG_token_h

#include "common.h"

#include <string_view>
#include <cstdint>

enum class TokenType : u8 {
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
    At,
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

    AmpersandEqual,
    CaretEqual,
    PipeEqual,

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
    HashIdentifier,

    Self,
    Enum,
    Struct,
    Let,
    Var,

    Static,
    Public,
    Private,
    Unpadded,
    Compact,

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
    Continue,
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
    Character,
    String,

    Error,

    EndOfFile,
};

const char *tokenTypeToString(TokenType tokenType);
std::ostream& operator<<(std::ostream& os, TokenType tokenType);

struct Token final {
    TokenType type;
    u32 length;
    u32 offset;

    std::string_view string_view(const char *fileStart) const {
        return {fileStart + offset, length};
    }

    Token(TokenType type, u32 length, u32 offset) 
        : type{type}, length{length}, offset{offset} {}
};

#endif // LANG_token_h
