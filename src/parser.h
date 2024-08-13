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

    ParsedFile(std::vector<AST::Declaration *> declarations) : declarations{std::move(declarations)} {}
};

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
        ExpectedExpression,
        ExpectedFunctionName,
        ExpectedClassName,
        ExpectedLiteral,
        InvalidBinaryOperator,
        InvalidInteger,
        InvalidFloating,
        InvalidEscapeSequence,
    };
//private:
    Cause cause;
    Token token;

    std::string description() const {
        switch (cause) {
        case ParserException::Cause::ExpectedExpression:
            return "Expected expression.";
        case ParserException::Cause::ExpectedFunctionName:
            return "Expected function name.";
        case ParserException::Cause::ExpectedClassName:
            return "Expected class name.";
        case ParserException::Cause::ExpectedLiteral:
            return "Expected literal.";
        case ParserException::Cause::InvalidBinaryOperator:
            return "Invalid binary operator.";
        case ParserException::Cause::InvalidInteger:
            return "Invalid integer.";
        case ParserException::Cause::InvalidFloating:
            return "Invalid floating-point number.";
        case ParserException::Cause::InvalidEscapeSequence:
            return "Invalid escape sequence in string.";
        }
    }

public:
    ParserException(Token token, Cause cause) : cause{cause}, token{token}  {}
};

class Parser {
    SymbolTable& symbols;
    ThreadContext& context = *ThreadContext::get();

    Scanner scanner;
    Token previous;
    Token current;

    struct ExpressionRules {
        bool allowInitializer = true;
    } expressionRules;

public:
    // Utilities
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
    AST::FunctionDeclaration *functionDeclaration();
    [[nodiscard]]
    AST::StructDeclaration *structDeclaration();
    [[nodiscard]]
    AST::FunctionDeclaration *initializerDeclaration();
    [[nodiscard]]
    AST::EnumDeclaration *enumDeclaration();
    [[nodiscard]]
    AST::EnumDeclaration::Case enumCase();
    [[nodiscard]]
    AST::EnumDeclaration::Case::Member enumCaseMember();
    [[nodiscard]]
    AST::VariableDeclaration *variableDeclaration();
    [[nodiscard]]
    AST::StatementDeclaration *statementDeclaration();

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
    AST::Literal *createStringLiteral(const Token& token);
    [[nodiscard]]
    AST::Expression *identifier();
    [[nodiscard]]
    AST::Expression *self();
    [[nodiscard]]
    AST::Expression *grouping();
    [[nodiscard]]
    AST::Expression *inferredInitializer();
    [[nodiscard]]
    AST::Expression *inferredMember();

    [[nodiscard]]
    AST::Expression *initializer(AST::Identifier *identifier);

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
        if (!match(type)) throw ParserException(current, ParserException::Cause::ExpectedClassName);
        return previous;
    }

public:
    Parser(SymbolTable& symbols, std::string&& string) 
        : symbols{symbols}
        , scanner{std::move(string)}
        , previous{scanner.next()}
        , current{previous} {}

    ParsedFile parse();
};

#endif
