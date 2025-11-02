#include "parser.h"
#include "parser/errors.h"

#include <cstdlib>
#include <cmath>
#include <fstream>
#include <iostream>
#include <charconv>
#include <cmath>

struct NoneValue {
    template <typename T>
    constexpr operator T*() const noexcept { return nullptr; }
};

struct ErrorValue {
    template <typename T>
    constexpr operator T*() const noexcept { return nullptr; }
};

constexpr NoneValue NONE{};
constexpr ErrorValue ERROR{};

// NOTE: Should only be used within functions that return values where the
// error case is created by {}, e.g. pointers.
#define TRY(expression) ({        \
    auto _result = (expression);  \
    if (!_result) [[unlikely]] {  \
        return {};                \
    }                             \
    _result;                      \
})

#define TRY_OPTIONAL(expression) ({        \
    auto _result = (expression);  \
    if (!_result) [[unlikely]] {  \
        return {};                \
    }                             \
    *_result;                     \
})

static std::optional<std::string> readFile(const char *path) {
    std::ifstream file(path, std::ios::in | std::ios::binary);
    if (file.fail()) {
        return {};
    }

    file.seekg(0, std::ifstream::end);
    size_t file_size = file.tellg();
    file.seekg(0, std::ifstream::beg);

    std::string contents(file_size, '\0');

    file.read(contents.data(), file_size);
    file.close();

    return contents;
}

ParsedFile parseFile(FileID fileID, File& file, DiagnosticWriter& writer) {
    auto contents = readFile(file.path);

    if (!contents) {
        // TODO: Create top-level diagnostic type.
        std::cout << "error: file '" << file.path << "' does not exist.\n";
        return ParsedFile{0, ParseResult::FATAL, {}, {}, {}, {}};
    }

    file.size = contents->size();

    Parser parser{fileID, *ThreadContext::get()->symbols, std::move(*contents)};

    return parser.parse(writer);
}

/* TODO:
 * - Implement error messages emitted during parsing:
 *   - Every return of nullptr (when indicating error), should.
 * - Implement error recovery to parse entire file.
 * - Consider making stringly typed errors into error cases/enums.
 */

ParsedFile Parser::parse(DiagnosticWriter& writer) 
{
    std::vector<AST::Declaration *NONNULL> declarations;

    int returnValue = sigsetjmp(startPoint, 0);

    if (returnValue == 0) {
        while (!match(TokenType::EndOfFile)) {
            declarations.emplace_back(declaration());
        }
        u32 size = previous.offset;

        auto astHandle = std::make_unique<ASTHandle>(std::move(nodeAllocator), std::move(arrayAllocator));
        return ParsedFile(size, ParseResult::OK, std::move(declarations), std::move(scanner.lineBreaks), std::move(astHandle), std::move(diagnostics));
    } else [[unlikely]] { // We're bailing out due to a fatal error
        auto astHandle = std::make_unique<ASTHandle>(std::move(nodeAllocator), std::move(arrayAllocator));
        return ParsedFile{0, ParseResult::FATAL, std::move(declarations), std::move(scanner.lineBreaks), std::move(astHandle), std::move(diagnostics)};
    }
}

// Utilities

void Parser::error(std::string&& message, DiagnosticLocation location) {
    diagnostics.error(message, fileID, location.offset, location);
    // TODO: Implement synchronization and recoverable (nonfatal) parsing.
    earlyExit();
}

Optional<Token> Parser::consume(TokenType type) {
    if (!match(type)) [[unlikely]] {
        ParsingError::unexpectedToken(*this, current, type);
        return {};
    }
    return previous;
}

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

/// Returns true if modifiers are allowed
bool Parser::checkModifiers(Modifiers modifiers, AST::Modifiers allowed)
{
    AST::Modifiers notAllowed = modifiers.modifiers.disablingAllIn(allowed);

    if (notAllowed.isNonEmpty()) {
        ParsingError::disallowedModifiers(*this, modifiers, notAllowed);
        return false;
    }

    if (auto accessModifiers = modifiers.modifiers.maskedBy(AST::accessModifiers); accessModifiers.count() > 1) {
        ParsingError::conflictingAccessModifiers(*this, modifiers, accessModifiers);
        return false;
    }
    
    return true;
}

AST::Block Parser::block() 
{
    consume(TokenType::LeftBracket);

    GrowingSpan<AST::Declaration *NONNULL> declarations{arrayAllocator};

    while (!match(TokenType::RightBracket)) {
        declarations.append(declaration());
    }

    return AST::Block(declarations.freeze());
}

// Types

AST::TypeNode *Parser::type(bool hasIdentifier) 
{
    Token nameToken = hasIdentifier ? previous : TRY_OPTIONAL(consume(TokenType::Identifier));
    Symbol& name = symbols.getSymbol(toStringView(nameToken));

    std::vector<AST::TypeModifier::Modifier> typeModifiers{};

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
    Token modifierToken = current;
    auto modifiers = parseModifiers();

    if (match(TokenType::Fn)) return functionDeclaration(modifiers);
    if (match(TokenType::Struct)) return structDeclaration(modifiers);
    if (match(TokenType::Var)) return variableDeclaration(modifiers);
    if (match(TokenType::Const)) return variableDeclaration(modifiers);
    if (match(TokenType::Enum)) return enumDeclaration(modifiers);

    if (!modifiers.modifiers.isEmpty()) {
        error("A statement cannot have modifiers.", modifierToken);
    }
    return statementDeclaration();
}

Optional<AST::FunctionParameter> Parser::parameter()
{
    auto nameToken = TRY(consume(TokenType::Identifier));
    auto& name = symbols.getSymbol(toStringView(nameToken));
    consume(TokenType::Colon);
    auto *type_ = TRY(type());

    return AST::FunctionParameter(name, type_);
}

AST::FunctionDeclaration *Parser::functionDeclaration(Modifiers modifiers)
{
    checkModifiers(modifiers, AST::FunctionDeclaration::allowedModifiers);

    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));

    consume(TokenType::LeftParenthesis);

    GrowingSpan<AST::FunctionParameter> parameters{arrayAllocator};
    if (!check(TokenType::RightParenthesis)) parameters.append(TRY(parameter()));
    while (!check(TokenType::RightParenthesis)) {
        consume(TokenType::Comma);
        parameters.append(TRY(parameter()));
    }

    consume(TokenType::RightParenthesis);

    AST::TypeNode *returnType = NONE;
    if (match(TokenType::Arrow)) {
        returnType = type();
    }

    auto code = block();

    assert(previous.type == TokenType::RightBracket);
    u32 closingBracket = previous.offset;

    return AST::FunctionDeclaration::create(
        nodeAllocator, nameToken, closingBracket, modifiers.modifiers, name, parameters.freeze(), returnType, code
    );
}

AST::FunctionDeclaration *Parser::initializerDeclaration(Modifiers modifiers)
{
    checkModifiers(modifiers, AST::FunctionDeclaration::allowedModifiers);
    if (match(TokenType::Question)) {
        assert(false && "TODO: Implement failable initializers");
    }

    consume(TokenType::LeftParenthesis);

    GrowingSpan<AST::FunctionParameter> parameters{arrayAllocator};
    if (!check(TokenType::RightParenthesis)) parameters.append(TRY(parameter()));
    while (!check(TokenType::RightParenthesis)) {
        consume(TokenType::Comma);
        parameters.append(TRY(parameter()));
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
    checkModifiers(modifiers, AST::StructDeclaration::allowedModifiers);
    auto token = previous;
    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));

    consume(TokenType::LeftBracket);
    GrowingSpan<AST::Declaration *NONNULL> declarations{arrayAllocator};
    while (!match(TokenType::RightBracket)) {
        declarations.append(declaration());
    }

    return AST::StructDeclaration::create(
        nodeAllocator, token, modifiers.modifiers, name, declarations.freeze()
    );
}

AST::EnumDeclaration *Parser::enumDeclaration(Modifiers modifiers)
{
    checkModifiers(modifiers, AST::EnumDeclaration::allowedModifiers);
    auto token = previous;
    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));

    AST::TypeNode *rawType = NONE;
    if (match(TokenType::Colon)) {
        rawType = type();
    }

    consume(TokenType::LeftBracket);
  
    GrowingSpan<AST::EnumDeclaration::Case> cases{arrayAllocator};
    GrowingSpan<AST::Declaration *NONNULL> declarations{arrayAllocator};

    while (!match(TokenType::RightBracket)) {
        if (match(TokenType::Case)) {
            auto case_ = enumCase();
            cases.append(case_);
        } else {
            declarations.append(declaration());
        }
    }

    return AST::EnumDeclaration::create(
        nodeAllocator, token, modifiers.modifiers, name, rawType, cases.freeze(), declarations.freeze()
    );
}

AST::EnumDeclaration::Case Parser::enumCase()
{
    auto token = previous;
    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));

    if (match(TokenType::LeftParenthesis)) {
        GrowingSpan<AST::EnumDeclaration::Case::Member> members{arrayAllocator};
        do {
            members.append(enumCaseMember());
        } while (match(TokenType::Comma));
        consume(TokenType::RightParenthesis);
        consume(TokenType::Semicolon);
        return AST::EnumDeclaration::Case(token, name, members.freeze());
    }

    consume(TokenType::Semicolon);

    return AST::EnumDeclaration::Case(token, name, {});
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
            return AST::EnumDeclaration::Case::Member(memberType);
        }
    } else {
        auto memberType = type();
        return AST::EnumDeclaration::Case::Member(memberType);
    }
}

AST::VariableDeclaration *Parser::variableDeclaration(Modifiers modifiers)
{
    checkModifiers(modifiers, AST::VariableDeclaration::allowedModifiers);
    auto token = previous;
    bool isMutable = token.type == TokenType::Var;

    auto variableBinding = binding();
    AST::TypeNode *tp = NONE;
    if (match(TokenType::Colon)) {
        tp = type();
    }

    AST::Expression *initial = NONE;
    if (match(TokenType::Equal)) {
        initial = expression({});
    }

    consume(TokenType::Semicolon);

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

AST::ConditionalUnwrap *Parser::unwrap()
{
    Token token = previous;
    auto *binding_ = binding();
    consume(TokenType::Equal);
    auto *expression_ = expression({.allowInitializer = false});

    return AST::ConditionalUnwrap::create(nodeAllocator, token, binding_, expression_);
}

Span<AST::Condition> Parser::conditions() 
{
    GrowingSpan<AST::Condition> conditions{arrayAllocator};
    while (true) {
        if (match(TokenType::Unwrap)) {
            conditions.append(unwrap());
        } else {
            conditions.append(expression({.allowInitializer = false}));
        }

        if (!match(TokenType::Comma)) {
            break;
        }
    }
    return conditions.freeze();
}

AST::IfStatement *Parser::ifStatement()
{
    auto token = previous;
    GrowingSpan<AST::IfStatement::Branch> branches{arrayAllocator};
    std::optional<AST::Block> fallback;
    while (true) {
        auto conds = conditions();
        auto code = block();
        branches.append({conds, code});

        if (match(TokenType::Else)) {
            if (!match(TokenType::If)) {
                fallback = block();
                break;
            }
        } else {
            break;
        }
    }

    return AST::IfStatement::create(nodeAllocator, token, branches.freeze(), fallback);
}

AST::ForStatement *Parser::forStatement()
{
    auto token = previous;

    auto *loopBinding = binding();

    consume(TokenType::In);

    auto *iterable = expression({.allowInitializer = false});

    auto code = block();

    return AST::ForStatement::create(nodeAllocator, token, loopBinding, iterable, code);
}

AST::GuardStatement *Parser::guardStatement() 
{
    auto token = previous;

    auto conds = conditions();

    consume(TokenType::Else); // TODO: Expected 'else' after condition in guard

    auto code = block();

    return AST::GuardStatement::create(nodeAllocator, token, conds, code);
}

AST::ReturnStatement *Parser::returnStatement()
{
    auto token = previous;
    AST::Expression *NULLABLE returnValue;
    if (match(TokenType::Semicolon)) {
        returnValue = nullptr;
    } else {
        returnValue = expression({});
        consume(TokenType::Semicolon);
    }
    return AST::ReturnStatement::create(nodeAllocator, token, returnValue);
}

AST::WhileStatement *Parser::whileStatement()
{
    auto token = previous;
    auto conds = conditions();
    auto code = block();

    return AST::WhileStatement::create(nodeAllocator, token, conds, code);
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
    auto *result = parseExpression(Precedence::LogicalOr);
    expressionRules = saved;
    return result;
}

AST::Expression *Parser::parseExpression(Precedence precedence)
{
    advance();
    ParseRule rule = ParseRule::expressionRules[static_cast<int>(previous.type)];

    if (rule.prefixHandler == nullptr) [[unlikely]] {
        if (previous.type == TokenType::EndOfFile) {
            ParsingError::unexpectedEndOfFile(*this, previous);
        }
        if (previous.type != TokenType::Error) {
            ParsingError::expectedExpression(*this, previous);
        }

        ParsingError::expectedExpression(*this, previous);
        return ERROR;
    }

    auto *expr = (this->*rule.prefixHandler)();

    while (true) {
        rule = ParseRule::expressionRules[static_cast<int>(current.type)];
        if (precedence <= rule.precedence) {
            advance();
            expr = (this->*rule.infixHandler)(expr);
        } else {
            break;
        }
    }

    return expr;
}

AST::Expression *Parser::call(AST::Expression *callee)
{
    auto token = previous;

    GrowingSpan<AST::Expression *NONNULL> arguments{arrayAllocator};

    if (match(TokenType::RightParenthesis)) {
        return AST::CallExpression::create(nodeAllocator, token, callee, arguments.freeze());
    }

    do {
        arguments.append(expression({}));
    } while (match(TokenType::Comma));

    consume(TokenType::RightParenthesis);

    return AST::CallExpression::create(nodeAllocator, token, callee, arguments.freeze());
}

AST::Expression *Parser::subscript(AST::Expression *NONNULL target) {
    auto token = previous;

    auto index = expression({});

    consume(TokenType::RightBrace);

    return AST::SubscriptExpression::create(nodeAllocator, token, target, index);
}

AST::Expression *Parser::member(AST::Expression *target)
{
    auto token = previous;
    auto nameToken = consume(TokenType::Identifier);
    auto& name = symbols.getSymbol(toStringView(nameToken));

    return AST::MemberAccessExpression::create(nodeAllocator, token, target, name);
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
    auto *right = parseExpression(newPrecedence);

    return AST::BinaryExpression::create(nodeAllocator, token, op, left, right);
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
    if (chars.size() > AST::IntegerLiteral::MAX_LENGTH) {
        // TODO: Emit non-fatal error.
        return llvm::APInt{0U, 0UL, true};
    }
    unsigned bits = bitCount<skip, base>(chars.length());
    chars.remove_prefix(skip);
    return llvm::APInt{bits, {chars}, base};
}

std::optional<double> parseDouble(std::string_view chars)
{
    char *end = nullptr;
    // NOTE: Depending on what the string view is backed by, this could read OOB in the future.
    double value = strtod(chars.data(), &end);
    if (end != (chars.end())) [[unlikely]] {
        return {};
    }
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

template <u32 length>
static inline bool checkCharacterLiteralLength(Token token, std::string_view characters, Parser& parser) {
    if (characters.length() != length) {
        ParsingError::invalidCharacterLiteral(parser, token);
        return false;
    }
    return true;
}

AST::Literal *Parser::createCharacterLiteral(const Token token) 
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
        TRY(checkCharacterLiteralLength<1>(token, characters, *this));
        value = c;
    } else {
        int length = std::countl_one(c);
        switch (length) {
            case 2:
                TRY(checkCharacterLiteralLength<2>(token, characters, *this));
                value = (characters[0] & 0x1F) << 6 | (characters[1] & 0x3F);
                break;
            case 3:
                TRY(checkCharacterLiteralLength<3>(token, characters, *this));
                value = (characters[0] & 0xF) << 12 | (characters[1] & 0x3F) << 6 | (characters[2] & 0x3F);
                break;
            case 4:
                TRY(checkCharacterLiteralLength<4>(token, characters, *this));
                value = (characters[0] & 0x7) << 18 | (characters[1] & 0x3F) << 12 | (characters[2] & 0x3F) << 6 | (characters[3] & 0x3F);
                break;
            default:
                ParsingError::invalidCharacterLiteral(*this, token);
                return ERROR;
        }
    }

    return AST::CharacterLiteral::create(nodeAllocator, token, value);
}

AST::Literal *Parser::createStringLiteral(Token token)
{
    GrowingString string{arrayAllocator};
    string.reserve(token.length - 2); // Don't reserve for quotes
    
    // NOTE: This currently only supports double quotes as delimiters.
    std::string_view characters = toStringView(token);
    characters.remove_prefix(1);
    characters.remove_suffix(1);

    for (const auto *it = characters.cbegin(); it != characters.cend(); ++it) {
        char c = *it;
        if (c == '\\') {
            ++it;
            auto escaped = escapeCharacter(*it);
            if (escaped.has_value()) {
                string.append(escaped.value());
            } else {
                ParsingError::invalidEscapeSequence(*this, token);
                return ERROR;
            }
        } else {
            string.append(c);
        }
    }

    return AST::StringLiteral::create(nodeAllocator, token, string.freeze());
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
                IntegerType::Binary,
                previous.length
            );
        case Octal: 
            return IntegerLiteral::create(
                nodeAllocator, 
                previous, 
                parseInteger<2, 8>(toStringView(previous)), 
                IntegerType::Octal,
                previous.length
            );
        case Integer: 
            return IntegerLiteral::create(
                nodeAllocator, 
                previous, 
                parseInteger<0, 10>(toStringView(previous)), 
                IntegerType::Decimal,
                previous.length
            );
        case Hexadecimal: 
            return IntegerLiteral::create(
                nodeAllocator, 
                previous, 
                parseInteger<2, 16>(toStringView(previous)), 
                IntegerType::Hexadecimal,
                previous.length
            );

        case Floating: {
            double value = 0.0;
            if (auto parsed = parseDouble(toStringView(previous))) {
                value = *parsed;
            } else {
                ParsingError::invalidFPLiteral(*this, previous);
            }
            return FloatingPointLiteral::create(nodeAllocator, previous, value);
        }

        case Character: return createCharacterLiteral(previous);

        case String: return createStringLiteral(previous);

        case True: return BooleanLiteral::create(nodeAllocator, previous, true);
        case False: return BooleanLiteral::create(nodeAllocator, previous, false);

        case Nil: return NilLiteral::create(nodeAllocator, previous);

        default: llvm_unreachable("[PROGRAMMER ERROR]: Unsupported literal type in parser.");
    }
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

    auto *target = parseExpression(precedence);

    return AST::UnaryExpression::create(nodeAllocator, token, op, target);
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
    auto *identifier = AST::Identifier::create(nodeAllocator, token, name);
    if (expressionRules.allowInitializer && match(TokenType::LeftBracket)) {
        return initializer(identifier);
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
    GrowingSpan<AST::TypeNode *NONNULL> typeArguments{arrayAllocator};
    if (match(TokenType::Less)) {
        hasTypeArguments = true;

        if (match(TokenType::Greater)) {
            // TODO: Error: empty type parameters are not allowed.
        }
        do {
            typeArguments.append(type());
        } while (match(TokenType::Comma));
        consume(TokenType::Greater);
    }

    bool hasCall = false;
    GrowingSpan<AST::Expression *NONNULL> arguments{arrayAllocator};
    if (match(TokenType::LeftParenthesis)) {
        hasCall = true;

        if (!match(TokenType::RightParenthesis)) {
            do {
                arguments.append(expression({}));
            } while (match(TokenType::Comma));

            consume(TokenType::RightParenthesis);
        }
    }

    return AST::IntrinsicExpression::create(
        nodeAllocator, 
        token, 
        name, 
        hasTypeArguments,
        typeArguments.freeze(), 
        hasCall,
        arguments.freeze()
    );
}

AST::Expression *Parser::inferredInitializer()
{
    return initializer(NONE);
}

AST::Expression *Parser::initializer(AST::Identifier *identifier)
{
    Token token = previous;
    GrowingSpan<AST::InitializerExpression::Pair> pairs{arrayAllocator};
    while (!match(TokenType::RightBracket)) {
        auto nameToken = consume(TokenType::Identifier);
        auto& name = symbols.getSymbol(toStringView(nameToken));
        consume(TokenType::Equal);
        auto *member = AST::InferredMemberAccessExpression::create(nodeAllocator, nameToken, name);
        auto *value = expression({});
        pairs.append({member, value});
        if (!match(TokenType::Comma)) {
            consume(TokenType::RightBracket);
            break;
        }
    }

    return AST::InitializerExpression::create(nodeAllocator, token, identifier, pairs.freeze());
}

// For improve cache-friendliness, this should probably be a "struct of arrays".
ParseRule ParseRule::expressionRules[] = {
    // FIXME: Figure out the precedence.
    [static_cast<int>(LeftBrace)]             = {nullptr,                      &Parser::subscript,    Precedence::Call},
    [static_cast<int>(RightBrace)]            = {nullptr,                      nullptr,               Precedence::None},

    [static_cast<int>(LeftParenthesis)]       = {&Parser::grouping,            &Parser::call,         Precedence::Call},
    [static_cast<int>(RightParenthesis)]      = {nullptr,                      nullptr,               Precedence::None},

    [static_cast<int>(LeftBracket)]           = {&Parser::inferredInitializer, nullptr,               Precedence::None},
    [static_cast<int>(RightBracket)]          = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(Comma)]                 = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(Dot)]                   = {&Parser::inferredMember,      &Parser::member,       Precedence::Call},

    [static_cast<int>(DotDotDot)]             = {nullptr,                      &Parser::binary,       Precedence::Range},
    [static_cast<int>(DotDotLess)]            = {nullptr,                      &Parser::binary,       Precedence::Range},

    [static_cast<int>(Bang)]                  = {nullptr,                      &Parser::postfixUnary, Precedence::Unary},
    [static_cast<int>(At)]                    = {nullptr,                      &Parser::postfixUnary, Precedence::Unary},

    [static_cast<int>(Plus)]                  = {nullptr,                      &Parser::binary,       Precedence::Term},
    [static_cast<int>(Minus)]                 = {&Parser::prefixUnary,         &Parser::binary,       Precedence::Term},
    [static_cast<int>(Star)]                  = {&Parser::prefixUnary,         &Parser::binary,       Precedence::Factor},
    [static_cast<int>(Slash)]                 = {nullptr,                      &Parser::binary,       Precedence::Factor},
    [static_cast<int>(Percent)]               = {nullptr,                      &Parser::binary,       Precedence::Factor},

    [static_cast<int>(LessLess)]              = {nullptr,                      &Parser::binary,       Precedence::Shift},
    [static_cast<int>(GreaterGreater)]        = {nullptr,                      &Parser::binary,       Precedence::Shift},

    [static_cast<int>(Tilde)]                 = {&Parser::prefixUnary,         nullptr,               Precedence::None},
    [static_cast<int>(Ampersand)]             = {&Parser::prefixUnary,         &Parser::binary,       Precedence::BitwiseAnd},
    [static_cast<int>(Caret)]                 = {nullptr,                      &Parser::binary,       Precedence::BitwiseXor},
    [static_cast<int>(Pipe)]                  = {nullptr,                      &Parser::binary,       Precedence::BitwiseOr},

    [static_cast<int>(And)]                   = {nullptr,                      &Parser::binary,       Precedence::LogicalAnd},
    [static_cast<int>(Or)]                    = {nullptr,                      &Parser::binary,       Precedence::LogicalOr},

    [static_cast<int>(BangEqual)]             = {nullptr,                      &Parser::binary,       Precedence::Equality},
    [static_cast<int>(EqualEqual)]            = {nullptr,                      &Parser::binary,       Precedence::Equality},

    [static_cast<int>(Greater)]               = {nullptr,                      &Parser::binary,       Precedence::Comparison},
    [static_cast<int>(GreaterEqual)]          = {nullptr,                      &Parser::binary,       Precedence::Comparison},
    [static_cast<int>(Less)]                  = {nullptr,                      &Parser::binary,       Precedence::Comparison},
    [static_cast<int>(LessEqual)]             = {nullptr,                      &Parser::binary,       Precedence::Comparison},

    [static_cast<int>(Not)]                   = {&Parser::prefixUnary,         nullptr,               Precedence::None},

    [static_cast<int>(Nil)]                   = {&Parser::literal,             nullptr,               Precedence::None},
    [static_cast<int>(True)]                  = {&Parser::literal,             nullptr,               Precedence::None},
    [static_cast<int>(False)]                 = {&Parser::literal,             nullptr,               Precedence::None},
    [static_cast<int>(Integer)]               = {&Parser::literal,             nullptr,               Precedence::None},
    [static_cast<int>(Binary)]                = {&Parser::literal,             nullptr,               Precedence::None},
    [static_cast<int>(Octal)]                 = {&Parser::literal,             nullptr,               Precedence::None},
    [static_cast<int>(Hexadecimal)]           = {&Parser::literal,             nullptr,               Precedence::None},
    [static_cast<int>(Floating)]              = {&Parser::literal,             nullptr,               Precedence::None},
    [static_cast<int>(Character)]             = {&Parser::literal,             nullptr,               Precedence::None},
    [static_cast<int>(String)]                = {&Parser::literal,             nullptr,               Precedence::None},

    [static_cast<int>(Identifier)]            = {&Parser::identifier,          nullptr,               Precedence::None},
    [static_cast<int>(HashIdentifier)]        = {&Parser::intrinsic,           nullptr,               Precedence::None},
    [static_cast<int>(Self)]                  = {&Parser::self,                nullptr,               Precedence::None},

    [static_cast<int>(Equal)]                 = {nullptr,                      nullptr,               Precedence::None},

    [static_cast<int>(Semicolon)]             = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(EndOfFile)]             = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(Enum)]                  = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(Struct)]                = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(Var)]                   = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(If)]                    = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(Else)]                  = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(Fn)]                    = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(For)]                   = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(While)]                 = {nullptr,                      nullptr,               Precedence::None},
    [static_cast<int>(Return)]                = {nullptr,                      nullptr,               Precedence::None},
};

