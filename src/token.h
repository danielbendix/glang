#ifndef LANG_token_h
#define LANG_token_h

#include "common.h"
#include "containers/optional.h"

#include <string_view>
#include <cstdint>
#include <cassert>


enum class TokenType : u8 {
    // Used for empty tokens.
    Empty = 0,
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

    Token() : type{TokenType::Empty}, length{0}, offset{0} {}

    Token(TokenType type, u32 length, u32 offset) 
        : type{type}, length{length}, offset{offset} {}
};

template<>
struct OptionalDiscriminant<Token> {
    using OptionalDiscriminantType = decltype(Token::type);
    static constexpr size_t OptionalDiscriminantOffset = offsetof(Token, type);
};

#endif // LANG_token_h
