#include "parser.h"

struct ParsingError {
    ParsingError() = delete;
    static void unexpectedToken(Parser& parser, Token encountered, TokenType expected);
    static void disallowedModifiers(Parser& parser, Parser::Modifiers modifiers, AST::Modifiers disallowedModifiers);
    static void conflictingAccessModifiers(Parser& parser, Parser::Modifiers modifiers, AST::Modifiers conflictingAccessModifiers);
    static void invalidCharacterLiteral(Parser& parser, Token token);
    static void invalidEscapeSequence(Parser& parser, Token token);
    static void invalidFPLiteral(Parser& parser, Token token);
    static void expectedExpression(Parser& parser, Token token);
    static void unexpectedEndOfFile(Parser& parser, Token token);
};
