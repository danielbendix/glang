#include "token.h"

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
        TOKEN_TYPE_CASE(Question)
        TOKEN_TYPE_CASE(Arrow)
        TOKEN_TYPE_CASE(Equal)
        TOKEN_TYPE_CASE(Plus)
        TOKEN_TYPE_CASE(Minus)
        TOKEN_TYPE_CASE(Star)
        TOKEN_TYPE_CASE(Slash)
        TOKEN_TYPE_CASE(Ampersand)
        TOKEN_TYPE_CASE(Caret)
        TOKEN_TYPE_CASE(Pipe)
        TOKEN_TYPE_CASE(PlusEqual) 
        TOKEN_TYPE_CASE(MinusEqual)
        TOKEN_TYPE_CASE(StarEqual) 
        TOKEN_TYPE_CASE(SlashEqual)
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
        TOKEN_TYPE_CASE(Self)
        TOKEN_TYPE_CASE(Enum)
        TOKEN_TYPE_CASE(Struct)
        TOKEN_TYPE_CASE(Let)
        TOKEN_TYPE_CASE(Var)
        TOKEN_TYPE_CASE(Case)
        TOKEN_TYPE_CASE(If)
        TOKEN_TYPE_CASE(Else)
        TOKEN_TYPE_CASE(Fn)
        TOKEN_TYPE_CASE(For)
        TOKEN_TYPE_CASE(Guard)
        TOKEN_TYPE_CASE(While)
        TOKEN_TYPE_CASE(Repeat)
        TOKEN_TYPE_CASE(Break)
        TOKEN_TYPE_CASE(Return)
        TOKEN_TYPE_CASE(True)
        TOKEN_TYPE_CASE(False)
        TOKEN_TYPE_CASE(Nil)
        TOKEN_TYPE_CASE(Integer)
        TOKEN_TYPE_CASE(Floating)
        TOKEN_TYPE_CASE(Binary)
        TOKEN_TYPE_CASE(Octal)
        TOKEN_TYPE_CASE(Hexadecimal)
        TOKEN_TYPE_CASE(String)
        TOKEN_TYPE_CASE(Error)
        TOKEN_TYPE_CASE(EndOfFile)
    }
}

std::ostream& operator<<(std::ostream& os, TokenType tokenType)
{
    return os << tokenTypeToString(tokenType);
}