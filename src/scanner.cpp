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
    start = current;
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
                    advance();
                    break;
                }
            case '/':
                if (peekNext() == '*') {
                    depth++;
                    advance();
                    advance();
                }
                break;
            case '\n':
                newline();
            default:
                advance();
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
    std::string_view chars = std::string_view(start, current);
    return Token(type, chars, line, offset);
}

TokenType testKeyword(std::string::const_iterator it, std::string::const_iterator end, const char *string, int length, TokenType t)
{
    std::string_view view(it, end);
    if (view.length() == length && view.compare(string) == 0) return t;
    return TokenType::Identifier;
}

TokenType Scanner::identifierType()
{
    std::string::const_iterator it = start;
    switch (*it++) {
        case 'a': return testKeyword(it, current, "nd", 2, TokenType::And);
        case 'b': return testKeyword(it, current, "reak", 4, TokenType::Break);
        case 'c': return testKeyword(it, current, "ase", 3, TokenType::Case);
        case 'e':
            switch (*it++) {
                case 'l': return testKeyword(it, current, "se", 2, TokenType::Else);
                case 'n': return testKeyword(it, current, "um", 2, TokenType::Enum);
            }
            break;
        case 'f': {
            switch (*it++) {
                case 'o': return testKeyword(it, current, "r", 1, TokenType::For);
                case 'n': return testKeyword(it, current, "", 0, TokenType::Fn);
            }
            break;
        }
        case 'g': {
            return testKeyword(it, current, "uard", 4, TokenType::Guard);
        }
        case 'i': return testKeyword(it, current, "f", 1, TokenType::If);
        case 'l': return testKeyword(it, current, "et", 2, TokenType::Let);
        case 'n': return testKeyword(it, current, "ot", 2, TokenType::Not);
        case 'o': return testKeyword(it, current, "r", 1, TokenType::Or);
        case 'r': {
            if (*it++ == 'e') {
                switch (*it++) {
                    case 'p': return testKeyword(it, current, "eat", 3, TokenType::Repeat);
                    case 't': return testKeyword(it, current, "urn", 3, TokenType::Return);
                }
            }
            break;
        }
        case 's': 
            switch (*it++) {
                case 'e': return testKeyword(it, current, "lf", 2, TokenType::Self);
                case 't': return testKeyword(it, current, "ruct", 4, TokenType::Struct);
            }
        case 'v': return testKeyword(it, current, "ar", 2, TokenType::Var);
        case 'w': return testKeyword(it, current, "hile", 4, TokenType::While);
    }

    return TokenType::Identifier;
}

Token Scanner::identifier()
{
    char c;
    while (isAlpha(c = peek()) || isDigit(c)) advance();

    return makeToken(identifierType());
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
        error(UnterminatedStringLiteral, std::format("{}:{}: error: unterminated string literal", this->line, this->offset));
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
    if (c == '.') {
        if (!munchMany1<isDigit>()) {
            // We may need to consume more to avoid spurious errors.
            return errorToken(EmptyFloatingPointFraction);
        }
    }

    c = peek();
    if (c == 'e' || c == 'E') {
        if (!munchMany1<isDigit>()) {
            // We may need to consume more to avoid spurious errors.
            return errorToken(EmptyFloatingPointExponent);
        }
    }

    return makeToken(TokenType::Integer);
}


Token Scanner::next() noexcept {
    using enum TokenType;

    while (current != end) {
        skipWhitespace();
        if (_error != None) {
            return errorToken();
        }

        start = current;

        char c = advance();

        if (isAlpha(c)) return identifier();
        if (isDigit(c)) return number(c);

        switch (c) {
            case '`': return escapedIdentifier();
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
            case '&': return makeToken(Ampersand);
            case '|': return makeToken(Pipe);
            case '^': return makeToken(Caret);
            case '.': {
                if (peek() == '.') {
                    advance();
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
            case '*': {
                if (peek() == '=') {
                    advance();
                    return makeToken(Caret);
                }
                return makeToken(Star);
            }
            case '/': 
                if (peek() == '=') {
                    advance();
                    return makeToken(SlashEqual);
                }
                return makeToken(Slash);
            case '<': {
                if (peek() == '=') {
                    advance();
                    return makeToken(LessEqual);
                }
                return makeToken(Less);
            }
            case '>': {
                if (peek() == '=') {
                    advance();
                    return makeToken(GreaterEqual);
                }
                return makeToken(Greater);
            }
            case '\0':
                return makeToken(EndOfFile);
            default: 
                std::cout << c << "\n";
                std::cout << int(c) << "\n";
                return errorToken(UnrecognizedCharacter);
        }
    }

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
    std::cerr << message << "\n";
}
