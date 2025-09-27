#include "scanner.h"

#include <iostream>
#include <format>

using enum Scanner::ErrorCause;
const Scanner::ErrorCause Scanner::NO_ERROR = None;

bool isAlpha(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool isDigit(char c)
{
    return c >= '0' && c <= '9';
}

bool isBinaryDigit(char c)
{
    return c == '0' || c == '1';
}

bool isOctalDigit(char c)
{
    return c >= '0' && c <= '7';
}

bool isHexDigit(char c)
{
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

void Scanner::multilineComment() {
    // Keep this for a potential error token.
    setStart();
    advance();
    advance();
    int depth = 1;
    while (depth) {
        if (isAtEnd()) {
            error(UnterminatedBlockComment);
            return;
        }
        switch(peek()) {
            case '*':
                if (peekNext() == '/') {
                    depth--;
                    advance();
                }
                advance();
                break;
            case '/':
                if (peekNext() == '*') {
                    depth++;
                    advance();
                }
                advance();
                break;
            case '\n':
                newline();
                advance();
                break;
            default:
                advance();
                break;
        }
    }
}

void Scanner::skipWhitespace() {
    for (;;) {
        char c = peek();
        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                advance();
                break;
            case '\n':
                newline();
                advance();
                break;
            case '/':
                switch (peekNext()) {
                    case '/': 
                        while (peek() != '\n' && !isAtEnd()) advance();
                        break;
                    case '*':
                        multilineComment();
                        break;
                    default:
                        return;
                }
                break;
            default:
                return;
        }
    }
}

Token Scanner::makeToken(TokenType type)
{
    const char *chars = start.base();
    u32 length = current - start;
    u32 offset = start - _string.begin();

    return Token{type, length, offset};
}

[[nodiscard]]
TokenType testKeyword(std::string::const_iterator it, std::string::const_iterator end, const char *string, int length, TokenType t)
{
    std::string_view view(it, end);
    if (view.length() == length && view.compare(string) == 0) return t;
    return TokenType::Identifier;
}

[[nodiscard]]
TokenType Scanner::testTry(std::string::const_iterator it, std::string::const_iterator end) {
    std::string_view view(it, end);
    if (view == "") return TokenType::Try;
    if (view == "?") return TokenType::TryQuestion;
    if (view == "!") return TokenType::TryBang;
    return TokenType::Identifier;
}

TokenType Scanner::identifierType()
{
    std::string::const_iterator it = start;
    switch (*it++) {
        case 'a': return testKeyword(it, current, "nd", 2, TokenType::And);
        case 'b': return testKeyword(it, current, "reak", 4, TokenType::Break);
        case 'c': 
            switch (*it++) {
                case 'a': return testKeyword(it, current, "se", 2, TokenType::Case);
                case 'o': 
                    switch (*it++) {
                        case 'm': return testKeyword(it, current, "pact", 4, TokenType::Compact);
                        case 'n': return testKeyword(it, current, "tinue", 5, TokenType::Continue);
                    }
            }
            break;
        case 'e':
            switch (*it++) {
                case 'l': return testKeyword(it, current, "se", 2, TokenType::Else);
                case 'n': return testKeyword(it, current, "um", 2, TokenType::Enum);
            }
            break;
        case 'f':
            switch (*it++) {
                case 'a': return testKeyword(it, current, "lse", 3, TokenType::False);
                case 'o': return testKeyword(it, current, "r", 1, TokenType::For);
                case 'n': return testKeyword(it, current, "", 0, TokenType::Fn);
            }
            break;
        case 'g': return testKeyword(it, current, "uard", 4, TokenType::Guard);
        case 'i': 
            switch (*it++) {
                case 'f': return testKeyword(it, current, "", 0, TokenType::If);
                case 'n': return testKeyword(it, current, "", 0, TokenType::In);
            }
            break;
        case 'l': return testKeyword(it, current, "et", 2, TokenType::Let);
        case 'n':
            switch(*it++) {
                case 'i': return testKeyword(it, current, "l", 1, TokenType::Nil);
                case 'o': return testKeyword(it, current, "t", 1, TokenType::Not);
            }
            break;
        case 'o': return testKeyword(it, current, "r", 1, TokenType::Or);
        case 'p':
            switch (*it++) {
                case 'r': return testKeyword(it, current, "ivate", 5, TokenType::Private);
                case 'u': return testKeyword(it, current, "blic", 4, TokenType::Public);
            }
        case 'r':
            if (*it++ == 'e') {
                switch (*it++) {
                    case 'p': return testKeyword(it, current, "eat", 3, TokenType::Repeat);
                    case 't': return testKeyword(it, current, "urn", 3, TokenType::Return);
                }
            }
            break;
        case 's': 
            switch (*it++) {
                case 'e': return testKeyword(it, current, "lf", 2, TokenType::Self);
                case 't': 
                    switch (*it++) {
                        case 'a': return testKeyword(it, current, "tic", 3, TokenType::Static);
                        case 'r': return testKeyword(it, current, "uct", 3, TokenType::Struct);
                    }
            }
            break;
        case 't':
            switch (*it++) {
                case 'h': return testKeyword(it, current, "row", 3, TokenType::Throw);
                case 'r': {
                    switch(*it++) {
                        case 'u': return testKeyword(it, current, "e", 1, TokenType::True);
                        case 'y': return testTry(it, current);
                    }
                }
            }
            break;
        case 'v': return testKeyword(it, current, "ar", 2, TokenType::Var);
        case 'w': 
            if (*it++ == 'h') {
                switch (*it++) {
                    case 'e': return testKeyword(it, current, "re", 2, TokenType::Where);
                    case 'i': return testKeyword(it, current, "le", 2, TokenType::While);
                }
            }
            break;
    }

    return TokenType::Identifier;
}

Token Scanner::identifier()
{
    char c;
    while (isAlpha(c = peek()) || isDigit(c)) advance();

    return makeToken(identifierType());
}

Token Scanner::hashIdentifier()
{
    char c;
    c = peek();

    if (!isAlpha(c)) {
        // Error: Intrinsic identifier must start with a letter or underscore.
    }

    munchMany<[](char c) { 
        return isAlpha(c) || isDigit(c); 
    }>();

    return makeToken(TokenType::HashIdentifier);
}

Token Scanner::escapedIdentifier()
{
    char c;
    while (isAlpha(c = peek()) || isDigit(c)) advance();
    
    if (c != '`') {
        // TODO: Throw error
    }
    
    return makeToken(TokenType::Identifier);
}

Token Scanner::character()
{
    char c = peek();
    while (c != '\'' && !isAtEnd()) {
        switch (advance()) {
            case '\n': 
                newline();
                break;
            case '\\':
                advance();
                break;
        }
        c = peek();
    }
    
    if (isAtEnd()) {
        error(UnterminatedCharacterLiteral, std::format("{}:{}: error: unterminated character literal", this->line, this->column));
        return errorToken();
    }

    advance();

    if (current - start == 2) {
        error(EmptyCharacterLiteral, std::format("{}:{}: error: empty character literal", this->line, this->column));
        return errorToken();
    }

    return makeToken(TokenType::Character);
}

Token Scanner::string()
{
    char c = peek();
    while (c != '"' && !isAtEnd()) {
        switch (advance()) {
            case '\n': 
                newline();
                break;
            case '\\':
                advance();
                break;
        }
        c = peek();
    }
    
    if (isAtEnd()) {
        error(UnterminatedStringLiteral, std::format("{}:{}: error: unterminated string literal", this->line, this->column));
        return errorToken();
    }

    advance();

    return makeToken(TokenType::String);
}

template <auto predicate>
void Scanner::munchMany() 
{
    char c;
    while (predicate(c = peek())) advance();
}

template <auto predicate>
bool Scanner::munchMany1() 
{
    char c = peek();
    if (!predicate(c)) return false;
    while (predicate(c = peek())) advance();
    return true;
}

Token Scanner::number(char first)
{
    char c;
    if (first == '0') {
        c = peek();
        switch (c) {
            case 'b':
                advance();
                if (!munchMany1<isBinaryDigit>()) {
                    return errorToken(EmptyBinaryLiteral);
                }
                return makeToken(TokenType::Binary);
            case 'o':
                advance();
                if (!munchMany1<isOctalDigit>()) {
                    return errorToken(EmptyOctalLiteral);
                }
                return makeToken(TokenType::Binary);
            case 'x':
                advance();
                if (!munchMany1<isHexDigit>()) {
                    return errorToken(EmptyHexadecimalLiteral);
                }
                return makeToken(TokenType::Hexadecimal);
        }
    }

    munchMany<isDigit>();

    c = peek();

    TokenType tokenType = TokenType::Integer;
    if (c == '.') {
        tokenType = TokenType::Floating;
        advance();
        if (!munchMany1<isDigit>()) {
            // We may need to consume more to avoid spurious errors.
            return errorToken(EmptyFloatingPointFraction);
        }
    }

    c = peek();
    if (c == 'e' || c == 'E') {
        tokenType = TokenType::Floating;
        advance();
        if (!munchMany1<isDigit>()) {
            // We may need to consume more to avoid spurious errors.
            return errorToken(EmptyFloatingPointExponent);
        }
    }

    return makeToken(tokenType);
}

template <char c, TokenType noRead, TokenType readNext>
Token Scanner::checkNext() {
    if (peek() == c) {
        advance();
        return makeToken(readNext);
    }
    return makeToken(noRead);
}

Token Scanner::next() noexcept {
    using enum TokenType;

    while (current != end) {
        skipWhitespace();
        if (_error != None) {
            return errorToken();
        }

        setStart();

        char c = advance();

        if (isAlpha(c)) return identifier();
        if (isDigit(c)) return number(c);

        switch (c) {
            case '#': return hashIdentifier();
            case '`': return escapedIdentifier();
            case '\'': return character();
            case '"': return string();
            case '{': return makeToken(LeftBracket);
            case '}': return makeToken(RightBracket);
            case '[': return makeToken(LeftBrace);
            case ']': return makeToken(RightBrace);
            case '(': return makeToken(LeftParenthesis);
            case ')': return makeToken(RightParenthesis);
            case ':': return makeToken(Colon);
            case ';': return makeToken(Semicolon);
            case ',': return makeToken(Comma);
            case '?': return makeToken(Question);
            case '~': return makeToken(Tilde);
            case '&': return checkNext<'=', Ampersand, AmpersandEqual>();
            case '|': return checkNext<'=', Pipe, PipeEqual>();
            case '^': return checkNext<'=', Caret, CaretEqual>();
            case '@': return makeToken(At);
            case '!': {
                if (peek() == '=') {
                    advance();
                    return makeToken(BangEqual);
                }
                return makeToken(Bang);
            }
            case '.': {
                if (peek() == '.') {
                    advance();
                    switch(peek()) {
                        case '.':
                            advance();
                            return makeToken(DotDotDot);
                        case '<':
                            advance();
                            return makeToken(DotDotLess);
                    }
                    return makeToken(DotDot);
                }
                return makeToken(Dot);
            }
            case '=': {
                if (peek() == '=') {
                    advance();
                    return makeToken(EqualEqual);
                }
                return makeToken(Equal);
            }
            case '+': 
                if (peek() == '=') {
                    advance();
                    return makeToken(PlusEqual);

                }
                return makeToken(Plus);
            case '-': {
                switch (peek()) {
                    case '=':
                        advance();
                        return makeToken(MinusEqual);
                    case '>':
                        advance();
                        return makeToken(Arrow);
                }
                return makeToken(Minus);
            }
            case '*': return checkNext<'=', Star, StarEqual>();
            case '/': return checkNext<'=', Slash, SlashEqual>();
            case '%': return checkNext<'=', Percent, PercentEqual>();
            case '<': {
                switch (peek()) {
                    case '=':
                        advance();
                        return makeToken(LessEqual);
                    case '<':
                        advance();
                        return makeToken(LessLess);
                }
                return makeToken(Less);
            }
            case '>': {
                switch (peek()) {
                    case '=':
                        advance();
                        return makeToken(GreaterEqual);
                    case '>':
                        advance();
                        return makeToken(GreaterGreater);
                }
                return makeToken(Greater);
            }
            case '\0': {
                // Produce EndOfFile as long as someone is asking.
                auto token = makeToken(EndOfFile);
                current = end;
                return token;
            }
            default: 
                return errorToken(UnrecognizedCharacter);
        }
    }
    setStart();
    return makeToken(EndOfFile);
}

Token Scanner::errorToken() {
    assert(_error != None);
    return makeToken(TokenType::Error);
}

Token Scanner::errorToken(ErrorCause cause) {
    _error = cause;
    return makeToken(TokenType::Error);
}

void Scanner::error(ErrorCause cause) {
    _error = cause;
}

void Scanner::error(ErrorCause cause, const std::string& message) {
    _error = cause;
}
