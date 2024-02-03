#include "parser.h"

#include <cstdlib>
#include <charconv>

ParsedFile Parser::parse() 
{
    std::vector<unique_ptr<AST::Declaration>> declarations;

    while (!match(TokenType::EndOfFile)) {
        declarations.emplace_back(declaration());
    }

    return ParsedFile(std::move(declarations));
}

// Utilities

AST::Block Parser::block() 
{
    consume(TokenType::LeftBracket);

    std::vector<std::unique_ptr<AST::Declaration>> declarations;

    while (!match(TokenType::RightBracket)) {
        declarations.emplace_back(declaration());
    }

    return AST::Block(std::move(declarations));
}

// Types

std::unique_ptr<AST::TypeNode> Parser::type() 
{
    // TODO: Support more than type literals
    Token name = consume(TokenType::Identifier);
    return AST::TypeLiteral::create(name);
}

// Declarations

std::unique_ptr<AST::Declaration> Parser::declaration() 
{
    if (match(TokenType::Fun)) return functionDeclaration();
    if (match(TokenType::Struct)) return structDeclaration();
    if (match(TokenType::Class)) return classDeclaration();
    if (match(TokenType::Var)) return variableDeclaration();
    if (match(TokenType::Let)) return variableDeclaration();

    return statementDeclaration();
}

AST::FunctionDeclaration::Parameter Parser::parameter()
{
    auto name = consume(TokenType::Identifier);
    consume(TokenType::Colon);
    auto tp = type();

    return AST::FunctionDeclaration::Parameter(name.chars, std::move(tp));
}

std::unique_ptr<AST::FunctionDeclaration> Parser::functionDeclaration()
{
    auto name = consume(TokenType::Identifier);

    consume(TokenType::LeftParenthesis);

    std::vector<AST::FunctionDeclaration::Parameter> parameters;
    while (!check(TokenType::RightParenthesis)) {
        parameters.emplace_back(parameter());
    }

    consume(TokenType::RightParenthesis);

    std::unique_ptr<AST::TypeNode> returnType = nullptr;
    if (match(TokenType::Arrow)) {
        returnType = type();
    }

    consume(TokenType::LeftBracket);

    std::vector<std::unique_ptr<AST::Declaration>> declarations;

    while (!match(TokenType::RightBracket)) {
        declarations.emplace_back(declaration());
    }

    return AST::FunctionDeclaration::create(name, std::move(parameters), std::move(returnType), std::move(declarations));
}



std::unique_ptr<AST::StructDeclaration> Parser::structDeclaration()
{
    std::vector<AST::StructDeclaration::Field> fields;
    std::vector<unique_ptr<AST::FunctionDeclaration>> methods;

    consume(TokenType::LeftBrace);
    while (!match(TokenType::RightBrace)) {
        if (match(TokenType::Var)) {
            // TODO: READ FIELD
        }

        if (match(TokenType::Fun)) {
            methods.emplace_back(functionDeclaration());
        }
    }

    return nullptr;
}

std::unique_ptr<AST::ClassDeclaration> Parser::classDeclaration()
{
    return nullptr;
}

std::unique_ptr<AST::EnumDeclaration> Parser::enumDeclaration()
{
    return nullptr;
}

std::unique_ptr<AST::VariableDeclaration> Parser::variableDeclaration()
{
    auto token = previous;
    auto identifier = consume(TokenType::Identifier);
    std::unique_ptr<AST::TypeNode> tp = nullptr;
    if (match(TokenType::Colon)) {
        tp = type();
    }

    std::unique_ptr<AST::Expression> initial = nullptr;
    if (match(TokenType::Equal)) {
        initial = expression();
    }

    consume(TokenType::Semicolon);

    return AST::VariableDeclaration::create(token, std::string(identifier.chars), std::move(tp), std::move(initial));
}

std::unique_ptr<AST::StatementDeclaration> Parser::statementDeclaration()
{
    return AST::StatementDeclaration::create(statement());
}

// Statements

bool isAssignmentOperator(TokenType tokenType)
{
    using enum TokenType;
    switch (tokenType) {
        case Equal:
        case PlusEqual:
        case MinusEqual:
        case StarEqual:
        case SlashEqual:
            return true;
        default:
            return false;
    }
}

std::unique_ptr<AST::Statement> Parser::statement()
{
    if (match(TokenType::If)) return ifStatement();
    if (match(TokenType::Return)) return returnStatement();
    if (match(TokenType::While)) return whileStatement();
    return assignmentOrExpression();
}

std::unique_ptr<AST::IfStatement> Parser::ifStatement()
{
    auto token = previous;
    std::vector<AST::IfStatement::Branch> conditionals;
    std::optional<AST::Block> fallback;
    while (true) {
        auto condition = expression();
        auto code = block();
        conditionals.emplace_back(std::move(condition), std::move(code));

        if (match(TokenType::Else)) {
            if (!match(TokenType::If)) {
                fallback = std::move(block());
                break;
            }
        } else {
            break;
        }
    }

    return AST::IfStatement::create(token, std::move(conditionals), std::move(fallback));
}

std::unique_ptr<AST::ReturnStatement> Parser::returnStatement()
{
    auto token = previous;
    auto expr = expression();
    consume(TokenType::Semicolon);
    return AST::ReturnStatement::create(token, std::move(expr));
}

std::unique_ptr<AST::WhileStatement> Parser::whileStatement()
{
    auto token = previous;
    auto condition = expression();
    auto code = block();

    return AST::WhileStatement::create(token, std::move(condition), std::move(code));
}

unique_ptr<AST::Statement> Parser::assignmentOrExpression()
{
    auto expr = expression();

    if (isAssignmentOperator(current.type)) {
        // FIXME: Assignment type
        auto op = current;
        advance();
        auto value = expression();
        consume(TokenType::Semicolon);
        return AST::AssignmentStatement::create(op, std::move(expr), std::move(value));
    } else {
        consume(TokenType::Semicolon);
        return AST::ExpressionStatement::create(std::move(expr));
    }
}

// Expressions

enum class Precedence {
    None,
    Or,            // or
    And,           // and
    Equality,      // == !=
    Comparison,    // < > <= >=
    Term,          // + -
    Factor,        // * /
    Unary,         // ! -
    Call,          // . ()
    Primary
};

bool operator<=(Precedence l, Precedence r)
{
    return static_cast<int>(l) <= static_cast<int>(r);
}

Precedence operator+(Precedence l, int i)
{
    return static_cast<Precedence>(static_cast<int>(l) + i);
}

struct ParseRule {
    typedef unique_ptr<AST::Expression> (Parser::*ExpressionPrefixHandler)();
    typedef unique_ptr<AST::Expression> (Parser::*ExpressionInfixHandler)(std::unique_ptr<AST::Expression>&& left);

    ExpressionPrefixHandler prefixHandler;
    ExpressionInfixHandler infixHandler;
    Precedence precedence;

    using enum TokenType;
    static ParseRule expressionRules[];
};



std::unique_ptr<AST::Expression> Parser::expression()
{
    return parseExpression(Precedence::Or);
    /*
    std::unique_ptr<AST::Expression> left = expressionTerminal();;
    auto op = operatorFromToken(current);
    if (op.has_value()) {
        advance();
    } else {
        return left;
    }
    auto token = previous;
    std::unique_ptr<AST::Expression> right = expression();

    return AST::BinaryExpression::create(token, op.value(), std::move(left), std::move(right));
    */
}

std::unique_ptr<AST::Expression> Parser::parseExpression(Precedence precedence)
{
    advance();
    ParseRule rule = ParseRule::expressionRules[static_cast<int>(previous.type)];

    if (!rule.prefixHandler) {
        throw ParserException(previous, ParserException::Cause::ExpectedExpression);
        // FIXME: Throw exception
    }

    auto expr = (this->*rule.prefixHandler)();

    while (true) {
        rule = ParseRule::expressionRules[static_cast<int>(current.type)];
        if (precedence <= rule.precedence) {
            advance();
            expr = (this->*rule.infixHandler)(std::move(expr));
        } else {
            break;
        }
    }

    return expr;
}

unique_ptr<AST::Expression> Parser::call(unique_ptr<AST::Expression>&& left)
{
    auto token = previous;

    if (match(TokenType::RightParenthesis)) {
        return AST::CallExpression::create(token, std::move(left), {});
    }

    std::vector<unique_ptr<AST::Expression>> arguments;

    do {
        arguments.emplace_back(expression());
    } while (match(TokenType::Comma));

    consume(TokenType::RightParenthesis);

    return AST::CallExpression::create(token, std::move(left), std::move(arguments));
}

unique_ptr<AST::Expression> Parser::dot(unique_ptr<AST::Expression>&& left)
{
    return nullptr;
}

std::pair<AST::BinaryOperator, Precedence> operatorFromToken(Token& token)
{
    using AST::BinaryOperator;
    using enum TokenType;
    switch (token.type) {
        case Plus: return {BinaryOperator::Add, Precedence::Term};
        case Minus: return {BinaryOperator::Subtract, Precedence::Term};
        case Star: return {BinaryOperator::Multiply, Precedence::Factor};
        case Slash: return {BinaryOperator::Divide, Precedence::Factor};

        case EqualEqual: return {BinaryOperator::Equal, Precedence::Equality};
        case NotEqual: return {BinaryOperator::NotEqual, Precedence::Equality};
        case Less: return {BinaryOperator::Less, Precedence::Comparison};
        case LessEqual: return {BinaryOperator::Less, Precedence::Comparison};
        case Greater: return {BinaryOperator::Greater, Precedence::Comparison};
        case GreaterEqual: return {BinaryOperator::GreaterEqual, Precedence::Comparison};

        case And: return {BinaryOperator::LogicalAnd, Precedence::And};
        case Or: return {BinaryOperator::LogicalOr, Precedence::Or};

        default: return {};
    }

}

unique_ptr<AST::Expression> Parser::binary(unique_ptr<AST::Expression>&& left)
{
    // Previous is operator.
    auto token = previous;
    auto [op, newPrecedence] = operatorFromToken(previous);
    auto right = parseExpression(newPrecedence);

    return AST::BinaryExpression::create(token, op, std::move(left), std::move(right));
}

template <typename T>
T parseNumber(Token& token)
{
    T value;
    std::string_view string = token.chars;
    auto result = std::from_chars(string.data(), string.data() + string.size(), value);
    if (result.ec == std::errc::invalid_argument) {
        throw ParserException(token, ParserException::Cause::InvalidInteger);
    }
    return value;
}

template <typename T, int skip, int base>
T parseNumber(Token token)
{
    T value;
    std::string_view string = token.chars;
    auto result = std::from_chars(string.data() + skip, string.data() + string.size(), value, base);
    if (result.ec == std::errc::invalid_argument) {
        throw ParserException(token, ParserException::Cause::InvalidInteger);
    }
    return value;
}

template <>
double parseNumber(Token& token)
{
    char *end = nullptr;
    double value = strtod(token.chars.data(), &end);
    if (end == (token.chars.data() + token.chars.size())) {
        return value;
    }
    // FIXME: Raise exception if all of string was not consumed
    return value;
}

std::optional<char> escapeCharacter(char c)
{
    switch (c) {
        case '\\': return '\\';
        case 'n': return '\n';
        case 't': return '\t';
        case 'r': return '\r';
        case '"': return '"';
        default: return {};
    }
}

std::unique_ptr<AST::Literal> createStringLiteral(const Token& token)
{
    unique_ptr<std::string> string{new std::string()};
    string->reserve(token.chars.length() - 2); // Don't reserve for quotes
    
    // NOTE: This currently only supports double quotes as separators
    std::string_view characters = token.chars;
    characters.remove_prefix(1);
    characters.remove_suffix(1);

    for (auto it = characters.cbegin(); it != characters.cend(); ++it) {
        char c = *it;
        if (c == '\\') {
            ++it;
            auto escaped = escapeCharacter(*it);
            if (escaped.has_value()) {
                string->push_back(escaped.value());
            } else {
                throw ParserException(token, ParserException::Cause::InvalidEscapeSequence);
            }
        } else {
            string->push_back(c);
        }
    }

    return AST::Literal::create(token, std::move(string));
}

// This should be renamed to something like terminal or expressionTerminal
std::unique_ptr<AST::Expression> Parser::literal()
{
    // FIXME: Check overflow of numerical literals
    using enum TokenType;
    using AST::Literal;

    switch (previous.type) {
        case Integer: return Literal::create(previous, parseNumber<uint64_t>(previous));
        case Binary: return Literal::create(previous, parseNumber<uint64_t, 2, 2>(previous));
        case Hexadecimal: return Literal::create(previous, parseNumber<uint64_t, 2, 16>(previous));
        case Floating: return Literal::create(previous, parseNumber<double>(previous));

        case String: return createStringLiteral(previous);

        case True: return Literal::create(previous, true);
        case False: return Literal::create(previous, false);

        default: throw ParserException(previous, ParserException::Cause::ExpectedLiteral);
    }

    return nullptr;
}

unique_ptr<AST::Expression> Parser::unary()
{
    // TODO: Convert to function.
    auto token = previous;
    AST::UnaryOperator op;
    switch (previous.type) {
        case TokenType::Not: op = AST::UnaryOperator::Not; break;
        case TokenType::Minus: op = AST::UnaryOperator::Negate; break;
        default: break; // FIXME: throw exception
    }

    auto target = parseExpression(Precedence::Unary);

    return AST::UnaryExpression::create(token, op, std::move(target));
}

unique_ptr<AST::Expression> Parser::identifier()
{
    return AST::Identifier::create(previous);
}

unique_ptr<AST::Expression> Parser::grouping()
{
    auto expr = parseExpression(Precedence::Or);
    consume(TokenType::RightParenthesis);
    return expr;
}

ParseRule ParseRule::expressionRules[] = {
    [static_cast<int>(LeftParenthesis)]       = {&Parser::grouping,   &Parser::call,      Precedence::Call},
    [static_cast<int>(RightParenthesis)]      = {NULL,                NULL,               Precedence::None},

    [static_cast<int>(LeftBracket)]           = {NULL,                NULL,               Precedence::None},
    [static_cast<int>(RightBracket)]          = {NULL,                NULL,               Precedence::None},
    [static_cast<int>(Comma)]                 = {NULL,                NULL,               Precedence::None},
    [static_cast<int>(Dot)]                   = {NULL,                &Parser::dot,       Precedence::Call},

    [static_cast<int>(Plus)]                  = {NULL,                &Parser::binary,    Precedence::Term},
    [static_cast<int>(Minus)]                 = {&Parser::unary,      &Parser::binary,    Precedence::Term},
    [static_cast<int>(Star)]                  = {NULL,                &Parser::binary,    Precedence::Factor},
    [static_cast<int>(Slash)]                 = {NULL,                &Parser::binary,    Precedence::Factor},

    [static_cast<int>(And)]                   = {NULL,                &Parser::binary,    Precedence::And},
    [static_cast<int>(Or)]                    = {NULL,                &Parser::binary,    Precedence::Or},

    [static_cast<int>(NotEqual)]              = {NULL,                &Parser::binary,    Precedence::Equality},
    [static_cast<int>(EqualEqual)]            = {NULL,                &Parser::binary,    Precedence::Equality},

    [static_cast<int>(Greater)]               = {NULL,                &Parser::binary,    Precedence::Comparison},
    [static_cast<int>(GreaterEqual)]          = {NULL,                &Parser::binary,    Precedence::Comparison},
    [static_cast<int>(Less)]                  = {NULL,                &Parser::binary,    Precedence::Comparison},
    [static_cast<int>(LessEqual)]             = {NULL,                &Parser::binary,    Precedence::Comparison},

    [static_cast<int>(Not)]                   = {&Parser::unary,      NULL,               Precedence::Comparison},

    [static_cast<int>(String)]                = {&Parser::literal,    NULL,               Precedence::None},
    [static_cast<int>(Integer)]               = {&Parser::literal,    NULL,               Precedence::None},
    [static_cast<int>(Binary)]                = {&Parser::literal,    NULL,               Precedence::None},
    [static_cast<int>(Hexadecimal)]           = {&Parser::literal,    NULL,               Precedence::None},
    [static_cast<int>(Floating)]              = {&Parser::literal,    NULL,               Precedence::None},
    [static_cast<int>(True)]                  = {&Parser::literal,    NULL,               Precedence::None},
    [static_cast<int>(False)]                 = {&Parser::literal,    NULL,               Precedence::None},

    [static_cast<int>(Identifier)]            = {&Parser::identifier, NULL,               Precedence::None},

    [static_cast<int>(Equal)]                 = {NULL,                NULL,               Precedence::None},

    [static_cast<int>(Semicolon)]             = {NULL,                NULL,               Precedence::None},
    [static_cast<int>(EndOfFile)]             = {NULL,                NULL,               Precedence::None},
    [static_cast<int>(Var)]                   = {NULL,                NULL,               Precedence::None},
    [static_cast<int>(If)]                    = {NULL,                NULL,               Precedence::None},
    [static_cast<int>(Else)]                  = {NULL,                NULL,               Precedence::None},
    [static_cast<int>(Fun)]                   = {NULL,                NULL,               Precedence::None},
    [static_cast<int>(For)]                   = {NULL,                NULL,               Precedence::None},
    [static_cast<int>(While)]                 = {NULL,                NULL,               Precedence::None},
    [static_cast<int>(Return)]                = {NULL,                NULL,               Precedence::None},


/*
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
    [TOKEN_SELF]          = {this_,    NULL,   PREC_NONE},
*/
};
