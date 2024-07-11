#include "parser.h"

#include <cstdlib>
#include <charconv>

/* TODO:
 * - Implement error messages
 * - Implement error recovery to parse entire file.
 */

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

    std::vector<unique_ptr<AST::Declaration>> declarations;

    while (!match(TokenType::RightBracket)) {
        declarations.emplace_back(declaration());
    }

    return AST::Block(std::move(declarations));
}

// Types

unique_ptr<AST::TypeNode> Parser::type(bool hasIdentifier) 
{
    Token name = hasIdentifier ? previous : consume(TokenType::Identifier);

    std::vector<AST::TypeModifier::Modifier> modifiers;

    using enum AST::TypeModifier::Modifier;

    for (;;) {
        if (match(TokenType::Star)) {
            modifiers.push_back(Pointer);
            continue;
        }

        if (match(TokenType::Question)) {
            modifiers.push_back(Optional);
            continue;
        }
        break;
    }

    if (modifiers.empty()) {
        return AST::TypeLiteral::create(name);
    } else {
        return AST::TypeModifier::create(AST::TypeLiteral::create(name), modifiers);
    }
}

// Declarations

unique_ptr<AST::Declaration> Parser::declaration() 
{
    if (match(TokenType::Fn)) return functionDeclaration();
    if (match(TokenType::Struct)) return structDeclaration();
    if (match(TokenType::Var)) return variableDeclaration();
    if (match(TokenType::Let)) return variableDeclaration();
    if (match(TokenType::Enum)) return enumDeclaration();

    return statementDeclaration();
}

AST::FunctionParameter Parser::parameter()
{
    auto name = consume(TokenType::Identifier);
    consume(TokenType::Colon);
    auto tp = type();

    return AST::FunctionParameter(name.chars, std::move(tp));
}

unique_ptr<AST::FunctionDeclaration> Parser::functionDeclaration()
{
    auto name = consume(TokenType::Identifier);

    consume(TokenType::LeftParenthesis);

    std::vector<AST::FunctionParameter> parameters;
    if (!check(TokenType::RightParenthesis)) parameters.emplace_back(std::move(parameter()));
    while (!check(TokenType::RightParenthesis)) {
        consume(TokenType::Comma);
        parameters.emplace_back(std::move(parameter()));
    }

    consume(TokenType::RightParenthesis);

    unique_ptr<AST::TypeNode> returnType = nullptr;
    if (match(TokenType::Arrow)) {
        returnType = type();
    }

    auto code = block();

    return AST::FunctionDeclaration::create(name, std::move(parameters), std::move(returnType), std::move(code));
}

unique_ptr<AST::FunctionDeclaration> Parser::initializerDeclaration()
{
    if (match(TokenType::Question)) {
        assert(false && "TODO: Implement failable initializers");
    }

    consume(TokenType::LeftParenthesis);

    std::vector<AST::FunctionParameter> parameters;
    if (!check(TokenType::RightParenthesis)) parameters.emplace_back(std::move(parameter()));
    while (!check(TokenType::RightParenthesis)) {
        consume(TokenType::Comma);
        parameters.emplace_back(std::move(parameter()));
    }

    consume(TokenType::RightParenthesis);

    if (match(TokenType::Arrow)) {
        auto returnType = type();
        // TODO: Report error with type.
    }


    auto code = block();


}

unique_ptr<AST::StructDeclaration> Parser::structDeclaration()
{
    auto name = consume(TokenType::Identifier);

    consume(TokenType::LeftBracket);
    std::vector<unique_ptr<AST::Declaration>> declarations;
    while (!match(TokenType::RightBracket)) {
        declarations.push_back(declaration());
    }

    return AST::StructDeclaration::create(name, name.chars, std::move(declarations));
}

unique_ptr<AST::EnumDeclaration> Parser::enumDeclaration()
{
    auto token = previous;
    auto name = consume(TokenType::Identifier);

    consume(TokenType::LeftBracket);
  
    unique_ptr<AST::TypeNode> rawType = nullptr;
    if (match(TokenType::Colon)) {
        rawType = type();
    }
    std::vector<AST::EnumDeclaration::Case> cases;
    std::vector<AST::unique_ptr<AST::Declaration>> declarations;

    while (!match(TokenType::RightBracket)) {
        if (match(TokenType::Case)) {
            auto case_ = enumCase();
            cases.push_back(std::move(case_));
        } else {
            declarations.push_back(declaration());
        }
    }

    return AST::EnumDeclaration::create(token, name.chars, std::move(rawType), std::move(cases), std::move(declarations));
}

AST::EnumDeclaration::Case Parser::enumCase()
{
    auto token = previous;
    auto name = consume(TokenType::Identifier);

    if (match(TokenType::LeftParenthesis)) {
        std::vector<AST::EnumDeclaration::Case::Member> members;
        do {
            members.push_back(enumCaseMember());
        } while (match(TokenType::Comma));
        consume(TokenType::RightParenthesis);
        consume(TokenType::Semicolon);
        return AST::EnumDeclaration::Case(token, name, std::move(members));
    }

    consume(TokenType::Semicolon);

    return AST::EnumDeclaration::Case(token, name);
}

AST::EnumDeclaration::Case::Member Parser::enumCaseMember()
{
    if (match(TokenType::Identifier)) {
        Token identifier = previous;
        if (match(TokenType::Colon)) {
            auto memberType = type();
            return AST::EnumDeclaration::Case::Member(identifier.chars, std::move(memberType));
        } else {
            auto memberType = type(true);
            return AST::EnumDeclaration::Case::Member(std::move(memberType));
        }
    } else {
        auto memberType = type();
        return AST::EnumDeclaration::Case::Member(std::move(memberType));
    }
}

unique_ptr<AST::VariableDeclaration> Parser::variableDeclaration()
{
    auto token = previous;
    bool isMutable = token.type == TokenType::Var;
    auto identifier = consume(TokenType::Identifier);
    unique_ptr<AST::TypeNode> tp = nullptr;
    if (match(TokenType::Colon)) {
        tp = type();
    }

    unique_ptr<AST::Expression> initial = nullptr;
    if (match(TokenType::Equal)) {
        initial = expression({});
    }

    consume(TokenType::Semicolon);

    return AST::VariableDeclaration::create(token, isMutable, std::string(identifier.chars), std::move(tp), std::move(initial));
}

unique_ptr<AST::StatementDeclaration> Parser::statementDeclaration()
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

unique_ptr<AST::Statement> Parser::statement()
{
    if (match(TokenType::If)) return ifStatement();
    if (match(TokenType::Return)) return returnStatement();
    if (match(TokenType::While)) return whileStatement();
    if (match(TokenType::Guard)) return guardStatement();
    return assignmentOrExpression();
}

unique_ptr<AST::IfStatement> Parser::ifStatement()
{
    auto token = previous;
    std::vector<AST::IfStatement::Branch> conditionals;
    std::optional<AST::Block> fallback;
    while (true) {
        auto condition = expression({.allowInitializer = false});
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

unique_ptr<AST::GuardStatement> Parser::guardStatement() 
{
    auto token = previous;

    auto condition = expression({.allowInitializer = false});

    consume(TokenType::Else); // TODO: Expected 'else' after condition in guard

    auto code = block();

    return AST::GuardStatement::create(token, std::move(condition), std::move(code));
}

unique_ptr<AST::ReturnStatement> Parser::returnStatement()
{
    auto token = previous;
    auto expr = expression({});
    consume(TokenType::Semicolon);
    return AST::ReturnStatement::create(token, std::move(expr));
}

unique_ptr<AST::WhileStatement> Parser::whileStatement()
{
    auto token = previous;
    auto condition = expression({.allowInitializer = false});
    auto code = block();

    return AST::WhileStatement::create(token, std::move(condition), std::move(code));
}

std::optional<AST::BinaryOperator> binaryOperatorFromAssignment(Token token)
{
    using enum AST::BinaryOperator;
    switch (token.type) {
        case TokenType::Equal:
            return {};
        case TokenType::PlusEqual:
            return Add;
        case TokenType::MinusEqual:
            return Subtract;
        case TokenType::StarEqual:
            return Multiply;
        case TokenType::SlashEqual:
            return Divide;
        default:
            llvm::llvm_unreachable_internal();
    }
}

unique_ptr<AST::Statement> Parser::assignmentOrExpression()
{
    auto expr = expression({});

    if (isAssignmentOperator(current.type)) {
        // FIXME: Assignment type
        auto token = current;
        auto op = binaryOperatorFromAssignment(token);
        advance();
        auto value = expression({});
        consume(TokenType::Semicolon);

        if (op) {
            return AST::CompoundAssignmentStatement::create(token, *op, std::move(expr), std::move(value));
        } else {
            return AST::AssignmentStatement::create(token, std::move(expr), std::move(value));

        }
    } else {
        consume(TokenType::Semicolon);
        return AST::ExpressionStatement::create(std::move(expr));
    }
}

// Expressions

enum class Precedence {
    None,
    LogicalOr,      // or
    LogicalAnd,     // and
    Equality,       // == !=
    Comparison,     // < > <= >=
    Shift,          // << >>
    Term,           // + -
    Factor,         // * /
    // FIXME: Ensure bit operators have the desired precedence
    BitwiseAnd,     // &
    BitwiseXor,     // ^
    BitwiseOr,      // |
    Unary,          // ! -
    Call,           // . ()
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
    typedef unique_ptr<AST::Expression> (Parser::*ExpressionInfixHandler)(unique_ptr<AST::Expression>&& left);

    ExpressionPrefixHandler prefixHandler;
    ExpressionInfixHandler infixHandler;
    Precedence precedence;

    using enum TokenType;
    static ParseRule expressionRules[];
};



unique_ptr<AST::Expression> Parser::expression(ExpressionRules rules)
{
    ExpressionRules saved = expressionRules;
    expressionRules = rules;
    auto result = parseExpression(Precedence::LogicalOr);
    expressionRules = saved;
    return result;
}

unique_ptr<AST::Expression> Parser::parseExpression(Precedence precedence)
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
        arguments.emplace_back(expression({}));
    } while (match(TokenType::Comma));

    consume(TokenType::RightParenthesis);

    return AST::CallExpression::create(token, std::move(left), std::move(arguments));
}

unique_ptr<AST::Expression> Parser::member(unique_ptr<AST::Expression>&& left)
{
    auto token = previous;
    auto name = consume(TokenType::Identifier);

    return AST::MemberAccessExpression::create(token, std::move(left), name);
}

unique_ptr<AST::Expression> Parser::inferredMember()
{
    auto token = previous;
    auto name = consume(TokenType::Identifier);

    return AST::InferredMemberAccessExpression::create(token, name);
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
        case Percent: return {BinaryOperator::Modulo, Precedence::Factor};

        case LessLess: return {BinaryOperator::ShiftLeft, Precedence::Shift};
        case GreaterGreater: return {BinaryOperator::ShiftRight, Precedence::Shift};

        case Ampersand: return {BinaryOperator::BitwiseAnd, Precedence::BitwiseAnd};
        case Pipe: return {BinaryOperator::BitwiseOr, Precedence::BitwiseOr};
        case Caret: return {BinaryOperator::BitwiseXor, Precedence::BitwiseXor};

        case EqualEqual: return {BinaryOperator::Equal, Precedence::Equality};
        case NotEqual: return {BinaryOperator::NotEqual, Precedence::Equality};
        case Less: return {BinaryOperator::Less, Precedence::Comparison};
        case LessEqual: return {BinaryOperator::Less, Precedence::Comparison};
        case Greater: return {BinaryOperator::Greater, Precedence::Comparison};
        case GreaterEqual: return {BinaryOperator::GreaterEqual, Precedence::Comparison};

        case And: return {BinaryOperator::LogicalAnd, Precedence::LogicalAnd};
        case Or: return {BinaryOperator::LogicalOr, Precedence::LogicalOr};

        default: llvm_unreachable("[PROGRAMMER ERROR]: Unsupported binary operator.");
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

template <int skip, int base>
unsigned bitCount(unsigned length) {
    if constexpr (__builtin_popcount(base) == 1) {
        int digits = length - skip;
        unsigned bits = ceil(__builtin_ctz(base) * digits);
        return bits;
    } else {
        int digits = length - skip;
        unsigned bits = ceil(log2(base) * digits);
        return bits;
    }
}

template <int skip, int base>
llvm::APInt parseInteger(Token token) {
    unsigned bits = bitCount<skip, base>(token.chars.length());
    std::string_view chars = token.chars;
    chars.remove_prefix(skip);
    return llvm::APInt{bits, {chars}, base};
}

double parseDouble(Token& token)
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

unique_ptr<AST::Literal> createStringLiteral(const Token& token)
{
    std::string string;
    string.reserve(token.chars.length() - 2); // Don't reserve for quotes
    
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
                string.push_back(escaped.value());
            } else {
                throw ParserException(token, ParserException::Cause::InvalidEscapeSequence);
            }
        } else {
            string.push_back(c);
        }
    }

    return AST::Literal::create(token, std::move(string));
}

// This should be renamed to something like terminal or expressionTerminal
unique_ptr<AST::Expression> Parser::literal()
{
    // FIXME: Check overflow of numerical literals
    using enum TokenType;
    using AST::Literal;
    using IntegerType = AST::Literal::IntegerType;

    switch (previous.type) {
        case Integer: return Literal::create(previous, parseInteger<0, 10>(previous), IntegerType::Decimal);
        case Binary: return Literal::create(previous, parseInteger<2, 2>(previous), IntegerType::Binary);
        case Octal: return Literal::create(previous, parseInteger<2, 8>(previous), IntegerType::Octal);
        case Hexadecimal: return Literal::create(previous, parseInteger<2, 16>(previous), IntegerType::Hexadecimal);

        case Floating: return Literal::create(previous, parseDouble(previous));

        case String: return createStringLiteral(previous);

        case True: return Literal::create(previous, true);
        case False: return Literal::create(previous, false);

        case Nil: return Literal::createNil(previous);

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
        case TokenType::Ampersand: op = AST::UnaryOperator::AddressOf; break;
        case TokenType::Star: op = AST::UnaryOperator::Dereference; break;
        default: llvm_unreachable("Token type is not a unary operator");
    }

    auto target = parseExpression(Precedence::Unary);

    return AST::UnaryExpression::create(token, op, std::move(target));
}

unique_ptr<AST::Expression> Parser::identifier()
{
    auto identifier = AST::Identifier::create(previous);
    if (expressionRules.allowInitializer && match(TokenType::LeftBracket)) {
        return initializer(std::move(identifier));
    }
    return identifier;
}

unique_ptr<AST::Expression> Parser::self()
{
    return AST::Self::create(previous);
}

unique_ptr<AST::Expression> Parser::grouping()
{
    auto expr = expression({});
    consume(TokenType::RightParenthesis);
    return expr;
}

unique_ptr<AST::Expression> Parser::inferredInitializer()
{
    return initializer(nullptr);
    
}

unique_ptr<AST::Expression> Parser::initializer(unique_ptr<AST::Identifier>&& identifier)
{
    Token token = previous;
    std::vector<AST::InitializerExpression::Pair> pairs;
    while (!match(TokenType::RightBracket)) {
        auto name = consume(TokenType::Identifier);
        consume(TokenType::Equal);
        auto member = AST::InferredMemberAccessExpression::create(name, name);
        auto value = expression({});
        pairs.push_back({std::move(member), std::move(value)});
        if (!match(TokenType::Comma)) {
            consume(TokenType::RightBracket);
            break;
        }
    }

    return AST::InitializerExpression::create(token, std::move(identifier), std::move(pairs));
}

ParseRule ParseRule::expressionRules[] = {
    [static_cast<int>(LeftParenthesis)]       = {&Parser::grouping,            &Parser::call,      Precedence::Call},
    [static_cast<int>(RightParenthesis)]      = {NULL,                         NULL,               Precedence::None},

    [static_cast<int>(LeftBracket)]           = {&Parser::inferredInitializer, NULL,               Precedence::None},
    [static_cast<int>(RightBracket)]          = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(Comma)]                 = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(Dot)]                   = {&Parser::inferredMember,      &Parser::member,    Precedence::Call},

    [static_cast<int>(Plus)]                  = {NULL,                         &Parser::binary,    Precedence::Term},
    [static_cast<int>(Minus)]                 = {&Parser::unary,               &Parser::binary,    Precedence::Term},
    [static_cast<int>(Star)]                  = {&Parser::unary,               &Parser::binary,    Precedence::Factor},
    [static_cast<int>(Slash)]                 = {NULL,                         &Parser::binary,    Precedence::Factor},
    [static_cast<int>(Percent)]               = {NULL,                         &Parser::binary,    Precedence::Factor},

    [static_cast<int>(LessLess)]              = {NULL,                         &Parser::binary,    Precedence::Shift},
    [static_cast<int>(GreaterGreater)]        = {NULL,                         &Parser::binary,    Precedence::Shift},

    [static_cast<int>(Ampersand)]             = {&Parser::unary,               &Parser::binary,    Precedence::BitwiseAnd},
    [static_cast<int>(Caret)]                 = {NULL,                         &Parser::binary,    Precedence::BitwiseXor},
    [static_cast<int>(Pipe)]                  = {NULL,                         &Parser::binary,    Precedence::BitwiseOr},

    [static_cast<int>(And)]                   = {NULL,                         &Parser::binary,    Precedence::LogicalAnd},
    [static_cast<int>(Or)]                    = {NULL,                         &Parser::binary,    Precedence::LogicalOr},

    [static_cast<int>(NotEqual)]              = {NULL,                         &Parser::binary,    Precedence::Equality},
    [static_cast<int>(EqualEqual)]            = {NULL,                         &Parser::binary,    Precedence::Equality},

    [static_cast<int>(Greater)]               = {NULL,                         &Parser::binary,    Precedence::Comparison},
    [static_cast<int>(GreaterEqual)]          = {NULL,                         &Parser::binary,    Precedence::Comparison},
    [static_cast<int>(Less)]                  = {NULL,                         &Parser::binary,    Precedence::Comparison},
    [static_cast<int>(LessEqual)]             = {NULL,                         &Parser::binary,    Precedence::Comparison},

    [static_cast<int>(Not)]                   = {&Parser::unary,               NULL,               Precedence::Comparison},

    [static_cast<int>(String)]                = {&Parser::literal,             NULL,               Precedence::None},
    [static_cast<int>(Integer)]               = {&Parser::literal,             NULL,               Precedence::None},
    [static_cast<int>(Binary)]                = {&Parser::literal,             NULL,               Precedence::None},
    [static_cast<int>(Octal)]                 = {&Parser::literal,             NULL,               Precedence::None},
    [static_cast<int>(Hexadecimal)]           = {&Parser::literal,             NULL,               Precedence::None},
    [static_cast<int>(Floating)]              = {&Parser::literal,             NULL,               Precedence::None},
    [static_cast<int>(True)]                  = {&Parser::literal,             NULL,               Precedence::None},
    [static_cast<int>(False)]                 = {&Parser::literal,             NULL,               Precedence::None},
    [static_cast<int>(Nil)]                   = {&Parser::literal,             NULL,               Precedence::None},

    [static_cast<int>(Identifier)]            = {&Parser::identifier,          NULL,               Precedence::None},
    [static_cast<int>(Self)]                  = {&Parser::self,                NULL,               Precedence::None},

    [static_cast<int>(Equal)]                 = {NULL,                         NULL,               Precedence::None},

    [static_cast<int>(Semicolon)]             = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(EndOfFile)]             = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(Enum)]                  = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(Struct)]                = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(Var)]                   = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(If)]                    = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(Else)]                  = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(Fn)]                    = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(For)]                   = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(While)]                 = {NULL,                         NULL,               Precedence::None},
    [static_cast<int>(Return)]                = {NULL,                         NULL,               Precedence::None},

/*
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
    [TOKEN_SELF]          = {this_,    NULL,   PREC_NONE},
*/
};
