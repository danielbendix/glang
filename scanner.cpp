#include "scanner.h"

#include <iostream>

#define TOKEN_TYPE_CASE(t) case t: return #t;

const char *tokenTypeToString(TokenType tokenType)
{
    using enum TokenType;
    switch (tokenType) {
        TOKEN_TYPE_CASE(LeftBrace)
        TOKEN_TYPE_CASE(RightBrace)
        TOKEN_TYPE_CASE(LeftBracket)
        TOKEN_TYPE_CASE(RightBracket)
        TOKEN_TYPE_CASE(LeftParenthesis)
        TOKEN_TYPE_CASE(RightParenthesis)
        TOKEN_TYPE_CASE(Comma)
        TOKEN_TYPE_CASE(Dot)
        TOKEN_TYPE_CASE(DotDot)
        TOKEN_TYPE_CASE(Colon)
        TOKEN_TYPE_CASE(Semicolon)
        TOKEN_TYPE_CASE(Arrow)
        TOKEN_TYPE_CASE(Equal)
        TOKEN_TYPE_CASE(Plus)
        TOKEN_TYPE_CASE(Minus)
        TOKEN_TYPE_CASE(Star)
        TOKEN_TYPE_CASE(Slash)
        TOKEN_TYPE_CASE(Power)
        TOKEN_TYPE_CASE(Not)
        TOKEN_TYPE_CASE(And)
        TOKEN_TYPE_CASE(Or)
        TOKEN_TYPE_CASE(NotEqual)
        TOKEN_TYPE_CASE(EqualEqual)
        TOKEN_TYPE_CASE(Less)
        TOKEN_TYPE_CASE(Greater)
        TOKEN_TYPE_CASE(LessEqual)
        TOKEN_TYPE_CASE(GreaterEqual)
        TOKEN_TYPE_CASE(Identifier)
        TOKEN_TYPE_CASE(Enum)
        TOKEN_TYPE_CASE(Struct)
        TOKEN_TYPE_CASE(Let)
        TOKEN_TYPE_CASE(Var)
        TOKEN_TYPE_CASE(If)
        TOKEN_TYPE_CASE(Else)
        TOKEN_TYPE_CASE(Fun)
        TOKEN_TYPE_CASE(For)
        TOKEN_TYPE_CASE(While)
        TOKEN_TYPE_CASE(Repeat)
        TOKEN_TYPE_CASE(Break)
        TOKEN_TYPE_CASE(Return)
        TOKEN_TYPE_CASE(True)
        TOKEN_TYPE_CASE(False)
        TOKEN_TYPE_CASE(Integer)
        TOKEN_TYPE_CASE(Floating)
        TOKEN_TYPE_CASE(Hexadecimal)
        TOKEN_TYPE_CASE(Binary)
        TOKEN_TYPE_CASE(String)
        TOKEN_TYPE_CASE(EndOfFile)
    }
}

std::ostream& operator<<(std::ostream& os, TokenType tokenType)
{
    std::cout << tokenTypeToString(tokenType);
    return os;
}

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

bool isHexDigit(char c)
{
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
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
                if (peekNext() == '/') {
                    while (peek() != '\n' && !isAtEnd()) advance();
                } else {
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
        case 'e':
            switch (*it++) {
                case 'l': return testKeyword(it, current, "se", 2, TokenType::Else);
                case 'n': return testKeyword(it, current, "um", 2, TokenType::Enum);
            }
            break;
        case 'f': {
            switch (*it++) {
                case 'o': return testKeyword(it, current, "r", 1, TokenType::For);
                case 'u': return testKeyword(it, current, "n", 1, TokenType::Fun);
            }
            break;
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
        case 's': return testKeyword(it, current, "truct", 5, TokenType::Struct);
        case 'v': return testKeyword(it, current, "ar", 2, TokenType::Var);
        case 'w': return testKeyword(it, current, "hile", 4, TokenType::While);
    }

    return TokenType::Identifier;
}

Token Scanner::identifier()
{
    char c;
    while (isAlpha(c = peek()) || isdigit(c)) advance();

    return makeToken(identifierType());
}

Token Scanner::string()
{
    char c = peek();
    while (c != '"' && !isAtEnd()) {
        if (peek() == '\n') newline();
        advance();
        c = peek();
    }
    
    if (isAtEnd()) throw ScannerException(ScannerException::Cause::UnterminatedString, line, offset);

    advance();

    return makeToken(TokenType::String);
}

template <auto predicate>
void Scanner::munchMany() 
{
    char c;
    while (predicate(c = peek())) advance();
}

template <auto predicate, ScannerException::Cause cause>
void Scanner::munchMany1() 
{
    char c = peek();
    if (!predicate(c)) throw ScannerException(cause, line, offset);
    while (predicate(c = peek())) advance();
}

Token Scanner::number(char first)
{
    char c;
    if (first == '0') {
        c = peek();
        switch (c) {
            case 'b':
                advance();
                munchMany1<isBinaryDigit, ScannerException::Cause::ExpectedDigit>();
                return makeToken(TokenType::Binary);
            case 'x':
                advance();
                munchMany1<isHexDigit, ScannerException::Cause::ExpectedDigit>();
                return makeToken(TokenType::Hexadecimal);
        }
    }

    munchMany<isDigit>();

    c = peek();
    if (c == '.') {
        munchMany1<isDigit, ScannerException::Cause::ExpectedDigit>();
    }

    c = peek();
    if (c == 'e' || c == 'E') {
        munchMany1<isDigit, ScannerException::Cause::ExpectedDigit>();
    }

    return makeToken(TokenType::Integer);
}


Token Scanner::next() {

    while (current != end) {
        skipWhitespace();
        start = current;

        char c = advance();

        if (isAlpha(c)) return identifier();
        if (isDigit(c)) return number(c);

        switch (c) {
            case '"': return string();
            case '{': return makeToken(TokenType::LeftBracket);
            case '}': return makeToken(TokenType::RightBracket);
            case '[': return makeToken(TokenType::LeftBrace);
            case ']': return makeToken(TokenType::RightBrace);
            case '(': return makeToken(TokenType::LeftParenthesis);
            case ')': return makeToken(TokenType::RightParenthesis);
            case ':': return makeToken(TokenType::Colon);
            case ';': return makeToken(TokenType::Semicolon);
            case ',': return makeToken(TokenType::Comma);
            case '.': {
                if (peek() == '.') {
                    advance();
                    return makeToken(TokenType::DotDot);
                }
                return makeToken(TokenType::Dot);
            }
            case '=': {
                if (peek() == '=') {
                    advance();
                    return makeToken(TokenType::EqualEqual);
                }
                return makeToken(TokenType::Equal);
            }
            case '+': return makeToken(TokenType::Comma);
            case '-': {
                if (peek() == '>') {
                    advance();
                    return makeToken(TokenType::Arrow);
                }
                return makeToken(TokenType::Minus);
            }
            case '*': {
                if (peek() == '*') {
                    advance();
                    return makeToken(TokenType::Power);
                }
                return makeToken(TokenType::Star);
            }
            case '/': return makeToken(TokenType::Slash);
            case '<': {
                if (peek() == '=') {
                    advance();
                    return makeToken(TokenType::LessEqual);
                }
                return makeToken(TokenType::Less);
            }
            case '>': {
                if (peek() == '=') {
                    advance();
                    return makeToken(TokenType::GreaterEqual);
                }
                return makeToken(TokenType::Greater);
            }

            case '\0':
                return makeToken(TokenType::EndOfFile);

            default: 
                std::cout << c << "\n";
                std::cout << int(c) << "\n";
                throw ScannerException(ScannerException::Cause::UnrecognizedCharacter, line, offset);
        }
    }



    return makeToken(TokenType::EndOfFile);
    
}
