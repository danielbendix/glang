#include "parser/errors.h"

std::string tokenTypeToHumanReadableString(TokenType tokenType) {
    // TODO: Some of these types will never be expected, 
    // so we could use unreachable on them.
    using enum TokenType;
    switch (tokenType) {
    case Empty: return "<EMPTY>";
    case LeftBrace: return "'['";
    case RightBrace: return "']'";
    case LeftBracket: return "'{'";
    case RightBracket: return "'}'";
    case LeftParenthesis: return "'('";
    case RightParenthesis: return "')'";
    case Comma: return "','";
    case Dot: return "'.'";
    case DotDot: return "'..'";
    case DotDotDot: return "'...'";
    case DotDotLess: return "'..<'";
    case Colon: return "':'";
    case Semicolon: return "';'";
    case Arrow: return "'->'";
    case At: return "'@'";
    case Bang: return "'!'";
    case Question: return "'?'";
    case Equal: return "'='";
    case Plus: return "'+'";
    case Minus: return "'-'";
    case Star: return "'*'";
    case Slash: return "'/'";
    case Percent: return "'%'";
    case LessLess: return "'<<'";
    case GreaterGreater: return "'>>'";
    case Tilde: return "'~'";
    case Ampersand: return "'&'";
    case Caret: return "'^'";
    case Pipe: return "'|'";
    case PlusEqual: return "'+='";
    case MinusEqual: return "'-='";
    case StarEqual: return "'*='";
    case SlashEqual: return "'/='";
    case PercentEqual: return "'%='";
    case AmpersandEqual: return "'&='";
    case CaretEqual: return "'^='";
    case PipeEqual: return "'|='";
    case Not: return "'not'";
    case And: return "'and'";
    case Or: return "'or'";
    case BangEqual: return "'!='";
    case EqualEqual: return "'=='";
    case Less: return "'<'";
    case Greater: return "'>'";
    case LessEqual: return "'<='";
    case GreaterEqual: return "'>='";
    case Identifier: return "identifier";
    case HashIdentifier: return "<INTRINSIC>";
    case Self: return "'self'";
    case Enum: return "'enum'";
    case Struct: return "'struct'";
    case Var: return "'var'";
    case Const: return "'const'";
    case Bind: return "'bind'";
    case Unwrap: return "'unwrap'";
    case Static: return "'static'";
    case Public: return "'public'";
    case Private: return "'private'";
    case Unpadded: return "'unpadded'";
    case Compact: return "'compact'";
    case Case: return "'case'";
    case If: return "'if'";
    case Else: return "'else'";
    case Init: return "'init'";
    case Fn: return "'fn'";
    case For: return "'for'";
    case In: return "'in'";
    case Guard: return "'guard'";
    case While: return "'while'";
    case Where: return "'where'";
    case Repeat: return "'repeat'";
    case Break: return "'break'";
    case Continue: return "'continue'";
    case Return: return "'return'";
    case Try: return "'try'";
    case TryQuestion: return "'try?'";
    case TryBang: return "'try!'";
    case Throw: return "'throw'";
    case True: return "'true'";
    case False: return "'false'";
    case Nil: return "'nil'";
    case Integer: return "integer literal";
    case Floating: return "floating point literal";
    case Binary: return "binary literal";
    case Octal: return "octal literal";
    case Hexadecimal: return "hex literal";
    case Character: return "character literal";
    case String: return "string literal";
    case Error: return "<ERROR>";
    case EndOfFile: return "<EOF>";
      break;
    }
}

void ParsingError::unexpectedToken(Parser& parser, Token encountered, TokenType expected) {
    parser.error("Expected " + tokenTypeToHumanReadableString(expected), encountered);
}

void ParsingError::disallowedModifiers(Parser& parser, Parser::Modifiers modifiers, AST::Modifiers disallowedModifiers) {
    // TODO: Provide more context.
    parser.error("Disallowed modifier for declaration", DiagnosticLocation{modifiers.offset, modifiers.length});
}

void ParsingError::conflictingAccessModifiers(Parser& parser, Parser::Modifiers modifiers, AST::Modifiers accessModifiers) {
    // TODO: Provide more context.
    parser.error("Conflicting access modifiers.", DiagnosticLocation{modifiers.offset, modifiers.length});
}

void ParsingError::invalidCharacterLiteral(Parser& parser, Token token) {

}

void ParsingError::invalidEscapeSequence(Parser& parser, Token token) {

}

void ParsingError::invalidFPLiteral(Parser& parser, Token token) {
    parser.error("Error while parsing floating-point literal.", token);
}

void ParsingError::expectedExpression(Parser& parser, Token token) {
    parser.error("Expected expression.", token);
}

void ParsingError::unexpectedEndOfFile(Parser& parser, Token token) {
    parser.error("Encountered unexpected EOF during parsing.", token);
}
