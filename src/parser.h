#ifndef LANG_parser_h
#define LANG_parser_h

#include "scanner.h"
#include "AST.h"
#include "context.h"

#include "containers/symbol_table.h"

struct ParsedFile {
    //std::string path;
public:
    std::vector<AST::Declaration *> declarations;
    std::vector<uint32_t> lineBreaks;
    std::unique_ptr<ASTHandle> astHandle;

    ParsedFile(std::vector<AST::Declaration *>&& declarations, std::vector<uint32_t>&& lineBreaks, std::unique_ptr<ASTHandle>&& astHandle)
        : declarations{std::move(declarations)}, lineBreaks{std::move(lineBreaks)}, astHandle{std::move(astHandle)} {}
};

ParsedFile parseString(std::string&& string);

struct ParserState {
    enum class Kind {
        Global,
        Function,
        Initializer,
        Struct,
        Class,
        Protocol,
        Enum,
    };

    Kind kind;
};

enum class Precedence : int;

class ParserException {
public:
    enum class During {
        Declaration,
        Statement,
        Expression,
    };
    enum class Cause {
        FailedExpectation,
        ExpectedExpression,
        ExpectedFunctionName,
        InvalidInteger,
        InvalidFloating,
        InvalidCharacterLiteral,
        InvalidEscapeSequence,
        InvalidModifier,
        StatementModifiers,
        ConflictingAccessModifiers,
    };
//private:
    Cause cause;
    Token token;
    union Data {
        struct {} nothing;
        TokenType expected;
    } data;

    std::string description() const {
        switch (cause) {
            case Cause::FailedExpectation:
                return std::string{"Expected '"} + tokenTypeToString(data.expected) + "', found '" + std::string{token.chars} + "'.";
            case Cause::ExpectedExpression:
                return "Expected expression.";
            case Cause::ExpectedFunctionName:
                return "Expected function name.";
            case Cause::InvalidInteger:
                return "Invalid integer.";
            case Cause::InvalidFloating:
                return "Invalid floating-point number.";
            case Cause::InvalidCharacterLiteral:
                return "Invalid character literal.";
            case Cause::InvalidEscapeSequence:
                return "Invalid escape sequence in string.";
            case Cause::InvalidModifier:
                return "Invalid modifier for declaration.";
            case Cause::StatementModifiers:
                return "Statements cannot be preceded by modifiers.";
            case Cause::ConflictingAccessModifiers:
                return "Conflicting access modifiers.";
        }
    }

    ParserException(Token token, Cause cause, Data data) : cause{cause}, token{token}, data{data}  {}
public:
    ParserException(Token token, Cause cause) : cause{cause}, token{token}, data{.nothing = {}}  {}

    static ParserException failedExpectation(Token token, TokenType expected) {
        return ParserException(token, Cause::FailedExpectation, {.expected = expected});
    }
};

class Parser {
    struct Modifiers {
        uint32_t line = 0;
        uint32_t column = 0;
        uint32_t length = 0;
        AST::Modifiers modifiers; 
    };

    SymbolTable& symbols;

    BumpAllocator nodeAllocator;
    Heap heap;
    Scanner scanner;
    Token previous;
    Token current;

    struct ExpressionRules {
        bool allowInitializer = true;
    } expressionRules;

    // Utilities
    [[nodiscard]]
    Modifiers parseModifiers();
    void checkModifiers(AST::Modifiers modifiers, AST::Modifiers allowed);
    [[nodiscard]]
    AST::Block block();
    [[nodiscard]]
    AST::FunctionParameter parameter();

    // Types
    [[nodiscard]]
    AST::TypeNode *type(bool hasIdentifier = false);

    // Bindings
    [[nodiscard]]
    AST::Binding *binding();

    // Declarations
    [[nodiscard]]
    AST::Declaration *declaration();
    [[nodiscard]]
    AST::FunctionDeclaration *functionDeclaration(Modifiers modifiers);
    [[nodiscard]]
    AST::StructDeclaration *structDeclaration(Modifiers modifiers);
    [[nodiscard]]
    AST::FunctionDeclaration *initializerDeclaration(Modifiers modifiers);
    [[nodiscard]]
    AST::EnumDeclaration *enumDeclaration(Modifiers modifiers);
    [[nodiscard]]
    AST::EnumDeclaration::Case enumCase();
    [[nodiscard]]
    AST::EnumDeclaration::Case::Member enumCaseMember();
    [[nodiscard]]
    AST::VariableDeclaration *variableDeclaration(Modifiers modifiers, bool isPattern = false);
    [[nodiscard]]
    AST::StatementDeclaration *statementDeclaration();

    [[nodiscard]]
    AST::vector<AST::Condition> conditions();

    // Statement
    [[nodiscard]]
    AST::Statement *statement();
    [[nodiscard]]
    AST::IfStatement *ifStatement();
    [[nodiscard]]
    AST::ForStatement *forStatement();
    [[nodiscard]]
    AST::GuardStatement *guardStatement();
    [[nodiscard]]
    AST::ReturnStatement *returnStatement();
    [[nodiscard]]
    AST::WhileStatement *whileStatement();
    [[nodiscard]]
    AST::BreakStatement *breakStatement();
    [[nodiscard]]
    AST::ContinueStatement *continueStatement();
    [[nodiscard]]
    AST::Statement *assignmentOrExpression();

    // Expression
    [[nodiscard]]
    AST::Expression *expression(ExpressionRules rules);

    // Expression helpers
    AST::Expression *parseExpression(Precedence precedence);

    [[nodiscard]]
    AST::Expression *call(AST::Expression *left);
    [[nodiscard]]
    AST::Expression *subscript(AST::Expression *left);
    [[nodiscard]]
    AST::Expression *member(AST::Expression *left);
    [[nodiscard]]
    AST::Expression *binary(AST::Expression *left);

    [[nodiscard]]
    AST::Expression *literal();
    [[nodiscard]]
    AST::Literal *createCharacterLiteral(const Token& token);
    [[nodiscard]]
    AST::Literal *createStringLiteral(const Token& token);
    [[nodiscard]]
    AST::Expression *identifier();
    [[nodiscard]]
    AST::Expression *self();
    [[nodiscard]]
    AST::Expression *grouping();
    [[nodiscard]]
    AST::Expression *intrinsic();
    [[nodiscard]]
    AST::Expression *initializer(AST::Identifier *identifier);
    [[nodiscard]]
    AST::Expression *inferredInitializer();
    [[nodiscard]]
    AST::Expression *inferredMember();

    [[nodiscard]]
    AST::Expression *prefixUnary();
    [[nodiscard]]
    AST::Expression *postfixUnary(AST::Expression *expression);

    void advance() {
        previous = current;
        current = scanner.next();
    }

    [[nodiscard]]
    bool check(TokenType type) {
        return current.type == type;
    }

    [[nodiscard]]
    bool match(TokenType type) {
        if (!check(type)) return false;
        advance();
        return true;
    }
   
    Token consume(TokenType type) {
        if (!match(type)) {
            throw ParserException::failedExpectation(current, type);
        }
        return previous;
    }

    template <typename T>
    ArrayAllocator<T> allocator() {
        return heap.allocator<T>();
    }

    friend class ParseRule;
public:
    Parser(SymbolTable& symbols, std::string&& string) 
        : symbols{symbols}
        , scanner{std::move(string)}
        , previous{scanner.next()}
        , current{previous} {}

    ParsedFile parse();
};

#endif
