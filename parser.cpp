#include "parser.h"

#include <charconv>

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

std::unique_ptr<AST::Type> Parser::type() 
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
    if (match(TokenType::Var)) return varDeclaration();

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
    std::cout << parameters.size() << "\n";

    consume(TokenType::RightParenthesis);

    std::unique_ptr<AST::Type> returnType = nullptr;
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
    return nullptr;
}

std::unique_ptr<AST::EnumDeclaration> Parser::enumDeclaration()
{
    return nullptr;
}

std::unique_ptr<AST::VarDeclaration> Parser::varDeclaration()
{
    auto token = previous;
    auto identifier = consume(TokenType::Identifier);
    std::unique_ptr<AST::Type> tp = nullptr;
    if (match(TokenType::Colon)) {
        tp = type();
    }

    std::unique_ptr<AST::Expression> initial = nullptr;
    if (match(TokenType::Equal)) {
        initial = expression();
    }

    return AST::VarDeclaration::create(token, std::string(identifier.chars), std::move(tp), std::move(initial));
}

std::unique_ptr<AST::StatementDeclaration> Parser::statementDeclaration()
{
    return AST::StatementDeclaration::create(statement());
}

// Statements

std::unique_ptr<AST::Statement> Parser::statement()
{
    if (match(TokenType::If)) return ifStatement();
    if (match(TokenType::Return)) return returnStatement();

    std::cout << "DEBUG\n";
    std::cout << current.line << "\n";
    std::cout << current.offset << "\n";
    std::cout << current.chars << "\n";

    //return expressionStatement();

    std::cout << "Returning nullptr\n";
    return nullptr;
}

std::unique_ptr<AST::IfStatement> Parser::ifStatement()
{
    auto token = previous;
    std::vector<AST::IfStatement::Branch> conditionals;
    bool hasFallback = false;
    while (true) {
        auto condition = expression();
        auto code = block();
        conditionals.emplace_back(std::move(condition), std::move(code));

        if (match(TokenType::Else)) {
            if (!match(TokenType::If)) {
                hasFallback = true;
                break;
            }
        } else {
            break;
        }
    }

    std::optional<AST::Block> fallback;
    if (hasFallback) {
        fallback = std::move(block());
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

std::unique_ptr<AST::ExpressionStatement> Parser::expressionStatement()
{
    //return AST::ExpressionStatement::create(expression());
    return nullptr;
}

// Expressions

static std::optional<AST::BinaryOperator> operatorFromToken(Token &token) {
    using AST::BinaryOperator;
    using enum TokenType;
    switch (token.type) {
        case Plus: return BinaryOperator::Add;
        case Minus: return BinaryOperator::Subtract;
        case Star: return BinaryOperator::Multiply;
        case Slash: return BinaryOperator::Divide;

        case EqualEqual: return BinaryOperator::Equal;
        case Less: return BinaryOperator::Less;
        case LessEqual: return BinaryOperator::Less;
        case Greater: return BinaryOperator::Greater;
        case GreaterEqual: return BinaryOperator::GreaterEqual;

        default: return {};
    }

}

std::unique_ptr<AST::Expression> Parser::expression()
{
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

    // TODO: Pratt parser
}

unique_ptr<AST::Expression> Parser::expressionTerminal()
{
    if (match(TokenType::Identifier)) {
        return AST::Identifier::create(previous);
    }
    return literal();
}

template <typename T>
T parseNumber(std::string_view string)
{
    T value;
    auto result = std::from_chars(string.data(), string.data() + string.size(), value);
    if (result.ec == std::errc::invalid_argument) {
        // TODO: Throw exception
    }
    return value;
}

template <>
double parseNumber(std::string_view string)
{
    // TODO: Improve performance
    // TODO: Catch exception
    return std::stod(std::string(string));
}

// This should be renamed to something like terminal or expressionTerminal
std::unique_ptr<AST::Literal> Parser::literal()
{
    // FIXME: Check overflow of numerical literals
    using enum TokenType;
    using AST::Literal;

    advance();

    switch (previous.type) {
        // FIXME: Have IdentifierLiteral
        case Integer: return Literal::create(previous, parseNumber<int64_t>(previous.chars));
        case Floating: return Literal::create(previous, parseNumber<double>(previous.chars));
        case String: return Literal::create(previous, std::string(previous.chars));

        default: throw ParserException(previous, ParserException::Cause::ExpectedLiteral);
    }

    return nullptr;
}
