#include "parser.h"

#include <cstdlib>
#include <charconv>

ParsedFile parseString(std::string&& string) {
    Parser parser{*ThreadContext::get()->symbols, std::move(string)};

    return parser.parse();
}

/* TODO:
 * - Implement error messages
 * - Implement error recovery to parse entire file.
 */

ParsedFile Parser::parse() 
{
    std::vector<AST::Declaration *NONNULL> declarations;

    while (!match(TokenType::EndOfFile)) {
        declarations.emplace_back(declaration());
    }

    u32 size = previous.offset;

    auto astHandle = std::make_unique<ASTHandle>(std::move(heap), std::move(nodeAllocator));
    return ParsedFile(size, std::move(declarations), std::move(scanner.lineBreaks), std::move(astHandle));
}

// Utilities
Parser::Modifiers Parser::parseModifiers()
{
    Modifiers result = {
        .offset = current.offset,
        .length = 0,
        .modifiers = {},
    };

    using enum AST::Modifiers::Modifier;
    // TODO: Error on repeated modifiers
    for (;;) {
        switch (current.type) {
            case TokenType::Static:
                advance();
                result.modifiers.set(Static);
                break;
            case TokenType::Public:
                advance();
                result.modifiers.set(Public);
                break;
            case TokenType::Private:
                advance();
                result.modifiers.set(Private);
                break;
            case TokenType::Compact:
                advance();
                result.modifiers.set(Compact);
                break;
            case TokenType::Unpadded:
                advance();
                result.modifiers.set(Unpadded);
                break;
            default: return result;
        }
    }
}

void Parser::checkModifiers(AST::Modifiers modifiers, AST::Modifiers allowed)
{
    AST::Modifiers notAllowed = modifiers.disablingAllIn(allowed);

    if (notAllowed.isNonEmpty()) {
        throw ParserException(previous, ParserException::Cause::InvalidModifier);
    }

    if (auto accessModifiers = modifiers.maskedBy(AST::accessModifiers); accessModifiers.count() > 1) {
        throw ParserException(previous, ParserException::Cause::ConflictingAccessModifiers);
    }
}

AST::Block Parser::block() 
{
    consume(TokenType::LeftBracket);

    AST::vector<AST::Declaration *NONNULL> declarations{allocator<AST::Declaration *>()};

    while (!match(TokenType::RightBracket)) {
        declarations.emplace_back(declaration());
    }

    return AST::Block(std::move(declarations));
}

// Types

AST::TypeNode *Parser::type(bool hasIdentifier) 
{
    Token nameToken = hasIdentifier ? previous : consume(TokenType::Identifier);
    Symbol& name = symbols.getSymbol(toStringView(nameToken));

    AST::vector<AST::TypeModifier::Modifier> typeModifiers{allocator<AST::TypeModifier::Modifier>()};

    using enum AST::TypeModifier::Modifier;

    u32 modifierOffset = current.offset;

    for (;;) {
        if (match(TokenType::Star)) {
            typeModifiers.push_back(Pointer);
            continue;
        }

        if (match(TokenType::Question)) {
            typeModifiers.push_back(Optional);
            continue;
        }

        if (match(TokenType::Ampersand)) {
            typeModifiers.push_back(Location);
            continue;
        }
        if (match(TokenType::LeftBrace)) {
            if (match(TokenType::Bang)) {
                typeModifiers.push_back(UnboundedArray);
            } else if (match(TokenType::Identifier)) {
                assert(false && "TODO: Support binding to size");
            } else if (match(TokenType::Integer)) {
                assert(false && "TODO: Support binding to size");
            } else {
                typeModifiers.push_back(Array);
            }
            consume(TokenType::RightBrace);
            continue;
        }
        break;
    }

    if (typeModifiers.empty()) {
        return AST::TypeLiteral::create(nodeAllocator, nameToken, name);
    } else {
        auto type = AST::TypeLiteral::create(nodeAllocator, nameToken, name);
        return AST::TypeModifier::create(nodeAllocator, type, typeModifiers, modifierOffset);
    }
}

// Bindings

AST::Binding *Parser::binding()
{
    auto token = consume(TokenType::Identifier);

    auto& identifier = symbols.getSymbol(toStringView(token));

    return AST::IdentifierBinding::create(nodeAllocator, token, identifier);
}

// Declarations

AST::Declaration *Parser::declaration() 
{
    auto modifiers = parseModifiers();
    if (match(TokenType::Fn)) return functionDeclaration(modifiers);
    if (match(TokenType::Struct)) return structDeclaration(modifiers);
    if (match(TokenType::Var)) return variableDeclaration(modifiers);
    if (match(TokenType::Let)) return variableDeclaration(modifiers);
    if (match(TokenType::Enum)) return enumDeclaration(modifiers);

    if (modifiers.modifiers.isEmpty()) {
        return statementDeclaration();
    } else {
        throw ParserException(current, ParserException::Cause::StatementModifiers);
    }
}

AST::FunctionParameter Parser::parameter()
{
    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));
    consume(TokenType::Colon);
    auto tp = type();

    return AST::FunctionParameter(name, tp);
}

AST::FunctionDeclaration *Parser::functionDeclaration(Modifiers modifiers)
{
    checkModifiers(modifiers.modifiers, AST::FunctionDeclaration::allowedModifiers);

    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));

    consume(TokenType::LeftParenthesis);

    AST::vector<AST::FunctionParameter> parameters{allocator<AST::FunctionParameter>()};
    if (!check(TokenType::RightParenthesis)) parameters.emplace_back(std::move(parameter()));
    while (!check(TokenType::RightParenthesis)) {
        consume(TokenType::Comma);
        parameters.emplace_back(std::move(parameter()));
    }

    consume(TokenType::RightParenthesis);

    AST::TypeNode *returnType = nullptr;
    if (match(TokenType::Arrow)) {
        returnType = type();
    }

    auto code = block();

    return AST::FunctionDeclaration::create(
        nodeAllocator, nameToken, modifiers.modifiers, name, std::move(parameters), returnType, std::move(code)
    );
}

AST::FunctionDeclaration *Parser::initializerDeclaration(Modifiers modifiers)
{
    checkModifiers(modifiers.modifiers, AST::FunctionDeclaration::allowedModifiers);
    if (match(TokenType::Question)) {
        assert(false && "TODO: Implement failable initializers");
    }

    consume(TokenType::LeftParenthesis);

    AST::vector<AST::FunctionParameter> parameters{allocator<AST::FunctionParameter>()};
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

    assert(false && "TODO: Implement this, or reconsider.");

    llvm_unreachable("TODO: Implement this, or reconsider");
}

AST::StructDeclaration *Parser::structDeclaration(Modifiers modifiers)
{
    checkModifiers(modifiers.modifiers, AST::StructDeclaration::allowedModifiers);
    auto token = previous;
    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));

    consume(TokenType::LeftBracket);
    AST::vector<AST::Declaration *NONNULL> declarations{allocator<AST::Declaration *>()};
    while (!match(TokenType::RightBracket)) {
        declarations.push_back(declaration());
    }

    return AST::StructDeclaration::create(
        nodeAllocator, token, modifiers.modifiers, name, std::move(declarations)
    );
}

AST::EnumDeclaration *Parser::enumDeclaration(Modifiers modifiers)
{
    checkModifiers(modifiers.modifiers, AST::EnumDeclaration::allowedModifiers);
    auto token = previous;
    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));

    AST::TypeNode *rawType = nullptr;
    if (match(TokenType::Colon)) {
        rawType = type();
    }

    consume(TokenType::LeftBracket);
  
    AST::vector<AST::EnumDeclaration::Case> cases{allocator<AST::EnumDeclaration::Case>()};
    AST::vector<AST::Declaration *NONNULL> declarations{allocator<AST::Declaration *>()};

    while (!match(TokenType::RightBracket)) {
        if (match(TokenType::Case)) {
            auto case_ = enumCase();
            cases.push_back(std::move(case_));
        } else {
            declarations.push_back(declaration());
        }
    }

    return AST::EnumDeclaration::create(
        nodeAllocator, token, modifiers.modifiers, name, rawType, std::move(cases), std::move(declarations)
    );
}

AST::EnumDeclaration::Case Parser::enumCase()
{
    auto token = previous;
    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));

    if (match(TokenType::LeftParenthesis)) {
        AST::vector<AST::EnumDeclaration::Case::Member> members{allocator<AST::EnumDeclaration::Case::Member>()};
        do {
            members.push_back(enumCaseMember());
        } while (match(TokenType::Comma));
        consume(TokenType::RightParenthesis);
        consume(TokenType::Semicolon);
        return AST::EnumDeclaration::Case(token, name, std::move(members));
    }

    consume(TokenType::Semicolon);

    return AST::EnumDeclaration::Case(token, name, allocator<AST::EnumDeclaration::Case::Member>());
}

AST::EnumDeclaration::Case::Member Parser::enumCaseMember()
{
    if (match(TokenType::Identifier)) {
        Token identifierToken = previous;
        auto& identifier = symbols.getSymbol(toStringView(identifierToken));
        if (match(TokenType::Colon)) {
            auto memberType = type();
            return AST::EnumDeclaration::Case::Member(&identifier, memberType);
        } else {
            auto memberType = type(true);
            return AST::EnumDeclaration::Case::Member(std::move(memberType));
        }
    } else {
        auto memberType = type();
        return AST::EnumDeclaration::Case::Member(std::move(memberType));
    }
}

AST::VariableDeclaration *Parser::variableDeclaration(Modifiers modifiers, bool isPattern)
{
    checkModifiers(modifiers.modifiers, AST::VariableDeclaration::allowedModifiers);
    auto token = previous;
    bool isMutable = token.type == TokenType::Var;

    auto variableBinding = binding();
    AST::TypeNode *tp = nullptr;
    if (match(TokenType::Colon)) {
        tp = type();
    }

    AST::Expression *initial = nullptr;
    if (match(TokenType::Equal)) {
        initial = expression({.allowInitializer = !isPattern});
    }

    if (!isPattern) {
        consume(TokenType::Semicolon);
    }

    return AST::VariableDeclaration::create(nodeAllocator, token, modifiers.modifiers, isMutable, variableBinding, tp, initial);
}

AST::StatementDeclaration *Parser::statementDeclaration()
{
    return AST::StatementDeclaration::create(nodeAllocator, statement());
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
        case AmpersandEqual:
        case CaretEqual:
        case PipeEqual:
            return true;
        default:
            return false;
    }
}

AST::Statement *Parser::statement()
{
    if (match(TokenType::If)) return ifStatement();
    if (match(TokenType::For)) return forStatement();
    if (match(TokenType::Return)) return returnStatement();
    if (match(TokenType::While)) return whileStatement();
    if (match(TokenType::Guard)) return guardStatement();
    if (match(TokenType::Break)) return breakStatement();
    if (match(TokenType::Continue)) return continueStatement();
    return assignmentOrExpression();
}

AST::vector<AST::Condition> Parser::conditions() 
{
    AST::vector<AST::Condition> conditions{allocator<AST::Condition>()};
    while (true) {
        if (match(TokenType::Var) || match(TokenType::Let)) {
            conditions.emplace_back(variableDeclaration({}, true));
        } else {
            conditions.emplace_back(expression({.allowInitializer = false}));
        }

        if (!match(TokenType::Comma)) {
            break;
        }
    }
    return conditions;
}

AST::IfStatement *Parser::ifStatement()
{
    auto token = previous;
    AST::vector<AST::IfStatement::Branch> branches{allocator<AST::IfStatement::Branch>()};
    std::optional<AST::Block> fallback;
    while (true) {
        auto conds = conditions();
        auto code = block();
        branches.emplace_back(std::move(conds), std::move(code));

        if (match(TokenType::Else)) {
            if (!match(TokenType::If)) {
                fallback = std::move(block());
                break;
            }
        } else {
            break;
        }
    }

    return AST::IfStatement::create(nodeAllocator, token, std::move(branches), std::move(fallback));
}

AST::ForStatement *Parser::forStatement()
{
    auto token = previous;

    auto loopBinding = binding();

    consume(TokenType::In);

    auto iterable = expression({.allowInitializer = false});

    auto code = block();

    return AST::ForStatement::create(nodeAllocator, token, std::move(loopBinding), std::move(iterable), std::move(code));
}

AST::GuardStatement *Parser::guardStatement() 
{
    auto token = previous;

    auto conds = conditions();

    consume(TokenType::Else); // TODO: Expected 'else' after condition in guard

    auto code = block();

    return AST::GuardStatement::create(nodeAllocator, token, std::move(conds), std::move(code));
}

AST::ReturnStatement *Parser::returnStatement()
{
    auto token = previous;
    auto expr = expression({});
    consume(TokenType::Semicolon);
    return AST::ReturnStatement::create(nodeAllocator, token, expr);
}

AST::WhileStatement *Parser::whileStatement()
{
    auto token = previous;
    auto conds = conditions();
    auto code = block();

    return AST::WhileStatement::create(nodeAllocator, token, std::move(conds), std::move(code));
}

AST::BreakStatement *Parser::breakStatement()
{
    auto token = previous;
    consume(TokenType::Semicolon);
    return AST::BreakStatement::create(nodeAllocator, token);
}

AST::ContinueStatement *Parser::continueStatement()
{
    auto token = previous;
    consume(TokenType::Semicolon);
    return AST::ContinueStatement::create(nodeAllocator, token);
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
        case TokenType::AmpersandEqual:
            return BitwiseAnd;
        case TokenType::CaretEqual:
            return BitwiseXor;
        case TokenType::PipeEqual:
            return BitwiseOr;
        default:
            llvm::llvm_unreachable_internal();
    }
}

AST::Statement *Parser::assignmentOrExpression()
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
            return AST::CompoundAssignmentStatement::create(nodeAllocator, token, *op, expr, value);
        } else {
            return AST::AssignmentStatement::create(nodeAllocator, token, expr, value);

        }
    } else {
        consume(TokenType::Semicolon);
        return AST::ExpressionStatement::create(nodeAllocator, expr);
    }
}

// Expressions

enum class Precedence : int {
    None,
    LogicalOr,      // or
    LogicalAnd,     // and
    LogicalNot,     // not
    Equality,       // == !=
    Comparison,     // < > <= >=
    Shift,          // << >>
    Range,          // ... ..<
    Term,           // + -
    Factor,         // * /
    // FIXME: Ensure bit operators have the desired precedence
    BitwiseAnd,     // &
    BitwiseXor,     // ^
    BitwiseOr,      // |
    Unary,          // ! -
    Call,           // . () []
    Stop,
};

Precedence operator++(Precedence& precedence) {
    precedence = Precedence{static_cast<int>(precedence) + 1};
    assert(precedence <= Precedence::Stop);
    return precedence;
}

bool operator<=(Precedence l, Precedence r)
{
    return static_cast<int>(l) <= static_cast<int>(r);
}

Precedence operator+(Precedence l, int i)
{
    return static_cast<Precedence>(static_cast<int>(l) + i);
}

struct ParseRule {
    typedef AST::Expression *(Parser::*ExpressionPrefixHandler)();
    typedef AST::Expression *(Parser::*ExpressionInfixHandler)(AST::Expression *left);

    ExpressionPrefixHandler prefixHandler;
    ExpressionInfixHandler infixHandler;
    Precedence precedence;

    using enum TokenType;
    static ParseRule expressionRules[];
};



AST::Expression *Parser::expression(ExpressionRules rules)
{
    ExpressionRules saved = expressionRules;
    expressionRules = rules;
    auto result = parseExpression(Precedence::LogicalOr);
    expressionRules = saved;
    return result;
}

AST::Expression *Parser::parseExpression(Precedence precedence)
{
    advance();
    ParseRule rule = ParseRule::expressionRules[static_cast<int>(previous.type)];

    if (!rule.prefixHandler) {
        if (previous.type == TokenType::Error) {
            // TODO: Use scanner error.
            throw ParserException(previous, ParserException::Cause::ExpectedExpression);
        } else {
            throw ParserException(previous, ParserException::Cause::ExpectedExpression);
        }
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

AST::Expression *Parser::call(AST::Expression *left)
{
    auto token = previous;

    AST::vector<AST::Expression *NONNULL> arguments{allocator<AST::Expression *>()};

    if (match(TokenType::RightParenthesis)) {
        return AST::CallExpression::create(nodeAllocator, token, std::move(left), std::move(arguments));
    }

    do {
        arguments.emplace_back(expression({}));
    } while (match(TokenType::Comma));

    consume(TokenType::RightParenthesis);

    return AST::CallExpression::create(nodeAllocator, token, std::move(left), std::move(arguments));
}

AST::Expression *Parser::subscript(AST::Expression *NONNULL left) {
    auto token = previous;

    auto index = expression({});

    consume(TokenType::RightBrace);

    return AST::SubscriptExpression::create(nodeAllocator, token, std::move(left), std::move(index));
}

AST::Expression *Parser::member(AST::Expression *left)
{
    auto token = previous;
    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));

    return AST::MemberAccessExpression::create(nodeAllocator, token, std::move(left), name);
}

AST::Expression *Parser::inferredMember()
{
    auto token = previous;
    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));

    return AST::InferredMemberAccessExpression::create(nodeAllocator, token, name);
}

std::pair<AST::BinaryOperator, Precedence> operatorFromToken(Token& token)
{
    using AST::BinaryOperator;
    using enum TokenType;
    switch (token.type) {
        case DotDotDot: return {BinaryOperator::ClosedRange, Precedence::Range};
        case DotDotLess: return {BinaryOperator::OpenRange, Precedence::Range};

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
        case BangEqual: return {BinaryOperator::NotEqual, Precedence::Equality};
        case Less: return {BinaryOperator::Less, Precedence::Comparison};
        case LessEqual: return {BinaryOperator::Less, Precedence::Comparison};
        case Greater: return {BinaryOperator::Greater, Precedence::Comparison};
        case GreaterEqual: return {BinaryOperator::GreaterEqual, Precedence::Comparison};

        case And: return {BinaryOperator::LogicalAnd, Precedence::LogicalAnd};
        case Or: return {BinaryOperator::LogicalOr, Precedence::LogicalOr};

        default: llvm_unreachable("[PROGRAMMER ERROR]: Unsupported binary operator.");
    }
}

AST::Expression *Parser::binary(AST::Expression *left)
{
    // Previous is operator.
    auto token = previous;
    auto [op, newPrecedence] = operatorFromToken(previous);
    ++newPrecedence;
    auto right = parseExpression(newPrecedence);

    return AST::BinaryExpression::create(nodeAllocator, token, op, std::move(left), std::move(right));
}

template <int skip, int base>
unsigned bitCount(unsigned length) {
    if constexpr (__builtin_popcount(base) == 1) {
        int digits = length - skip;
        unsigned bits = ceil(__builtin_ctz(base) * digits) + 1;
        return bits;
    } else {
        int digits = length - skip;
        unsigned bits = ceil(log2(base) * digits) + 1;
        return bits;
    }
}

template <int skip, int base>
llvm::APInt parseInteger(std::string_view chars) {
    unsigned bits = bitCount<skip, base>(chars.length());
    chars.remove_prefix(skip);
    return llvm::APInt{bits, {chars}, base};
}

double parseDouble(std::string_view chars)
{
    char *end = nullptr;
    double value = strtod(chars.data(), &end);
    if (end == (chars.end())) {
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

template <int length>
static inline void checkLength(const Token& token, std::string_view characters) {
    if (characters.length() != length) {
        throw ParserException(token, ParserException::Cause::InvalidCharacterLiteral);
    }
}

AST::Literal *Parser::createCharacterLiteral(const Token& token) 
{
    // NOTE: Assumption of single quote delimiters.
    std::string_view characters = toStringView(token);
    characters.remove_prefix(1);
    characters.remove_suffix(1);

    AST::CharacterLiteral::Character value = 0;

    // TODO: I shouldn't be rolling my own unicode handling code.
    u8 c = characters[0];

    if (c == '\\') {

    } else if (c <= 0x7F) {
        checkLength<1>(token, characters);
        value = c;
    } else {
        int length = std::countl_one(c);
        switch (length) {
            case 2:
                checkLength<2>(token, characters);
                value = (characters[0] & 0x1F) << 6 | (characters[1] & 0x3F);
                break;
            case 3:
                checkLength<3>(token, characters);
                value = (characters[0] & 0xF) << 12 | (characters[1] & 0x3F) << 6 | (characters[2] & 0x3F);
                break;
            case 4:
                checkLength<4>(token, characters);
                value = (characters[0] & 0x7) << 18 | (characters[1] & 0x3F) << 12 | (characters[2] & 0x3F) << 6 | (characters[3] & 0x3F);
                break;
            default:
                throw ParserException(token, ParserException::Cause::InvalidCharacterLiteral);
        }
    }

    return AST::CharacterLiteral::create(nodeAllocator, token, value);
}

AST::Literal *Parser::createStringLiteral(const Token& token)
{
    AST::string string{allocator<char>()};
    string.reserve(token.length - 2); // Don't reserve for quotes
    
    // NOTE: This currently only supports double quotes as delimiters.
    std::string_view characters = toStringView(token);
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

    return AST::StringLiteral::create(nodeAllocator, token, std::move(string));
}

AST::Expression *Parser::literal()
{
    // FIXME: Check overflow of numerical literals
    using enum TokenType;
    using AST::NilLiteral;
    using AST::BooleanLiteral;
    using AST::IntegerLiteral;
    using AST::FloatingPointLiteral;
    using IntegerType = AST::IntegerLiteral::Type;

    switch (previous.type) {
        case Binary: 
            return IntegerLiteral::create(
                nodeAllocator, 
                previous, 
                parseInteger<2, 2>(toStringView(previous)), 
                IntegerType::Binary
            );
        case Octal: 
            return IntegerLiteral::create(
                nodeAllocator, 
                previous, 
                parseInteger<2, 8>(toStringView(previous)), 
                IntegerType::Octal
            );
        case Integer: 
            return IntegerLiteral::create(
                nodeAllocator, 
                previous, 
                parseInteger<0, 10>(toStringView(previous)), 
                IntegerType::Decimal
            );
        case Hexadecimal: 
            return IntegerLiteral::create(
                nodeAllocator, 
                previous, 
                parseInteger<2, 16>(toStringView(previous)), 
                IntegerType::Hexadecimal
            );

        case Floating: return FloatingPointLiteral::create(nodeAllocator, previous, parseDouble(toStringView(previous)));

        case Character: return createCharacterLiteral(previous);

        case String: return createStringLiteral(previous);

        case True: return BooleanLiteral::create(nodeAllocator, previous, true);
        case False: return BooleanLiteral::create(nodeAllocator, previous, false);

        case Nil: return NilLiteral::create(nodeAllocator, previous);

        default: llvm_unreachable("[PROGRAMMER ERROR]: Unsupported literal type in parser.");
    }

    return nullptr;
}

std::pair<AST::UnaryOperator, Precedence> unaryOperator(TokenType type)
{
    switch (type) {
        case TokenType::Not: return {AST::UnaryOperator::Not, Precedence::LogicalNot};
        case TokenType::Minus: return {AST::UnaryOperator::Negate, Precedence::Unary};
        case TokenType::Tilde: return {AST::UnaryOperator::BitwiseNegate, Precedence::Unary};
        case TokenType::Ampersand: return {AST::UnaryOperator::AddressOf, Precedence::Unary};
        case TokenType::Star: return {AST::UnaryOperator::PrefixDereference, Precedence::Unary};
        default: llvm_unreachable("Token type is not a unary prefix operator");
    }
}

AST::Expression *Parser::prefixUnary()
{
    auto token = previous;
    auto [op, precedence] = unaryOperator(previous.type);

    auto target = parseExpression(precedence);

    return AST::UnaryExpression::create(nodeAllocator, token, op, std::move(target));
}

AST::Expression *Parser::postfixUnary(AST::Expression *expression)
{
    auto token = previous;

    switch (token.type) {
        case TokenType::Bang: 
            return AST::UnaryExpression::create(
                nodeAllocator, 
                token, 
                AST::UnaryOperator::ForceUnwrap, 
                expression
            );
        case TokenType::At:
            return AST::UnaryExpression::create(
                nodeAllocator,
                token,
                AST::UnaryOperator::PostfixDereference,
                expression
            );
        default:
            llvm_unreachable("Token type is not a unary postfix operator.");
    }
}

AST::Expression *Parser::identifier()
{
    auto token = previous;
    auto& name = symbols.getSymbol(toStringView(token));
    auto identifier = AST::Identifier::create(nodeAllocator, token, name);
    if (expressionRules.allowInitializer && match(TokenType::LeftBracket)) {
        return initializer(std::move(identifier));
    }
    return identifier;
}

AST::Expression *Parser::self()
{
    return AST::Self::create(nodeAllocator, previous);
}

AST::Expression *Parser::grouping()
{
    auto expr = expression({});
    consume(TokenType::RightParenthesis);
    return expr;
}

AST::Expression *Parser::intrinsic()
{
    auto token = previous;
    auto& name = symbols.getSymbol(toStringView(token).substr(1));

    bool hasTypeArguments = false;
    AST::vector<AST::TypeNode *NONNULL> typeArguments{allocator<AST::TypeNode *>()};
    if (match(TokenType::Less)) {
        hasTypeArguments = true;

        if (match(TokenType::Greater)) {
            // TODO: Error: empty type paramters are not allowed.
        }
        do {
            typeArguments.emplace_back(type());
        } while (match(TokenType::Comma));
        consume(TokenType::Greater);
    }

    bool hasCall = false;
    AST::vector<AST::Expression *NONNULL> arguments{allocator<AST::Expression *>()};
    if (match(TokenType::LeftParenthesis)) {
        hasCall = true;

        if (!match(TokenType::RightParenthesis)) {
            do {
                arguments.emplace_back(expression({}));
            } while (match(TokenType::Comma));

            consume(TokenType::RightParenthesis);
        }
    }

    return AST::IntrinsicExpression::create(
        nodeAllocator, 
        token, 
        name, 
        hasTypeArguments,
        std::move(typeArguments), 
        hasCall,
        std::move(arguments)
    );
}

AST::Expression *Parser::inferredInitializer()
{
    return initializer(nullptr);
}

AST::Expression *Parser::initializer(AST::Identifier *identifier)
{
    Token token = previous;
    AST::vector<AST::InitializerExpression::Pair> pairs{allocator<AST::InitializerExpression::Pair>()};
    while (!match(TokenType::RightBracket)) {
        auto nameToken = consume(TokenType::Identifier);
        auto& name = symbols.getSymbol(toStringView(nameToken));
        consume(TokenType::Equal);
        auto member = AST::InferredMemberAccessExpression::create(nodeAllocator, nameToken, name);
        auto value = expression({});
        pairs.push_back({std::move(member), std::move(value)});
        if (!match(TokenType::Comma)) {
            consume(TokenType::RightBracket);
            break;
        }
    }

    return AST::InitializerExpression::create(nodeAllocator, token, std::move(identifier), std::move(pairs));
}

// For improve cache-friendliness, this should probably be a "struct of arrays".
ParseRule ParseRule::expressionRules[] = {
    // FIXME: Figure out the precedence.
    [static_cast<int>(LeftBrace)]             = {NULL,                         &Parser::subscript,    Precedence::Call},
    [static_cast<int>(RightBrace)]            = {NULL,                         NULL,                  Precedence::None},

    [static_cast<int>(LeftParenthesis)]       = {&Parser::grouping,            &Parser::call,         Precedence::Call},
    [static_cast<int>(RightParenthesis)]      = {NULL,                         NULL,                  Precedence::None},

    [static_cast<int>(LeftBracket)]           = {&Parser::inferredInitializer, NULL,                  Precedence::None},
    [static_cast<int>(RightBracket)]          = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(Comma)]                 = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(Dot)]                   = {&Parser::inferredMember,      &Parser::member,       Precedence::Call},

    [static_cast<int>(DotDotDot)]             = {NULL,                         &Parser::binary,       Precedence::Range},
    [static_cast<int>(DotDotLess)]            = {NULL,                         &Parser::binary,       Precedence::Range},

    [static_cast<int>(Bang)]                  = {NULL,                         &Parser::postfixUnary, Precedence::Unary},
    [static_cast<int>(At)]                    = {NULL,                         &Parser::postfixUnary, Precedence::Unary},

    [static_cast<int>(Plus)]                  = {NULL,                         &Parser::binary,       Precedence::Term},
    [static_cast<int>(Minus)]                 = {&Parser::prefixUnary,         &Parser::binary,       Precedence::Term},
    [static_cast<int>(Star)]                  = {&Parser::prefixUnary,         &Parser::binary,       Precedence::Factor},
    [static_cast<int>(Slash)]                 = {NULL,                         &Parser::binary,       Precedence::Factor},
    [static_cast<int>(Percent)]               = {NULL,                         &Parser::binary,       Precedence::Factor},

    [static_cast<int>(LessLess)]              = {NULL,                         &Parser::binary,       Precedence::Shift},
    [static_cast<int>(GreaterGreater)]        = {NULL,                         &Parser::binary,       Precedence::Shift},

    [static_cast<int>(Tilde)]                 = {&Parser::prefixUnary,         NULL,                  Precedence::None},
    [static_cast<int>(Ampersand)]             = {&Parser::prefixUnary,         &Parser::binary,       Precedence::BitwiseAnd},
    [static_cast<int>(Caret)]                 = {NULL,                         &Parser::binary,       Precedence::BitwiseXor},
    [static_cast<int>(Pipe)]                  = {NULL,                         &Parser::binary,       Precedence::BitwiseOr},

    [static_cast<int>(And)]                   = {NULL,                         &Parser::binary,       Precedence::LogicalAnd},
    [static_cast<int>(Or)]                    = {NULL,                         &Parser::binary,       Precedence::LogicalOr},

    [static_cast<int>(BangEqual)]             = {NULL,                         &Parser::binary,       Precedence::Equality},
    [static_cast<int>(EqualEqual)]            = {NULL,                         &Parser::binary,       Precedence::Equality},

    [static_cast<int>(Greater)]               = {NULL,                         &Parser::binary,       Precedence::Comparison},
    [static_cast<int>(GreaterEqual)]          = {NULL,                         &Parser::binary,       Precedence::Comparison},
    [static_cast<int>(Less)]                  = {NULL,                         &Parser::binary,       Precedence::Comparison},
    [static_cast<int>(LessEqual)]             = {NULL,                         &Parser::binary,       Precedence::Comparison},

    [static_cast<int>(Not)]                   = {&Parser::prefixUnary,         NULL,                  Precedence::None},

    [static_cast<int>(Nil)]                   = {&Parser::literal,             NULL,                  Precedence::None},
    [static_cast<int>(True)]                  = {&Parser::literal,             NULL,                  Precedence::None},
    [static_cast<int>(False)]                 = {&Parser::literal,             NULL,                  Precedence::None},
    [static_cast<int>(Integer)]               = {&Parser::literal,             NULL,                  Precedence::None},
    [static_cast<int>(Binary)]                = {&Parser::literal,             NULL,                  Precedence::None},
    [static_cast<int>(Octal)]                 = {&Parser::literal,             NULL,                  Precedence::None},
    [static_cast<int>(Hexadecimal)]           = {&Parser::literal,             NULL,                  Precedence::None},
    [static_cast<int>(Floating)]              = {&Parser::literal,             NULL,                  Precedence::None},
    [static_cast<int>(Character)]             = {&Parser::literal,             NULL,                  Precedence::None},
    [static_cast<int>(String)]                = {&Parser::literal,             NULL,                  Precedence::None},

    [static_cast<int>(Identifier)]            = {&Parser::identifier,          NULL,                  Precedence::None},
    [static_cast<int>(HashIdentifier)]        = {&Parser::intrinsic,           NULL,                  Precedence::None},
    [static_cast<int>(Self)]                  = {&Parser::self,                NULL,                  Precedence::None},

    [static_cast<int>(Equal)]                 = {NULL,                         NULL,                  Precedence::None},

    [static_cast<int>(Semicolon)]             = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(EndOfFile)]             = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(Enum)]                  = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(Struct)]                = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(Var)]                   = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(If)]                    = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(Else)]                  = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(Fn)]                    = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(For)]                   = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(While)]                 = {NULL,                         NULL,                  Precedence::None},
    [static_cast<int>(Return)]                = {NULL,                         NULL,                  Precedence::None},

/*
    [TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
    [TOKEN_SELF]          = {this_,    NULL,   PREC_NONE},
*/
};

