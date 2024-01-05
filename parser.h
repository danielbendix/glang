#ifndef LANG_parser_h
#define LANG_parser_h

#include "scanner.h"
#include "AST.h"

struct ParsedFile {
    std::string path;
    std::vector<AST::Declaration *> declarations;
};

class ParserException {
public:
    enum class Cause {
        ExpectedFunctionName,
        ExpectedClassName,
        ExpectedLiteral,
        InvalidBinaryOperator,

    };
//private:
    Cause cause;
    Token token;

public:
    ParserException(Token token, Cause cause) : cause{cause}, token{token}  {}
};

using std::unique_ptr;

class Parser {
//private:
public:
    Scanner scanner;
    Token previous;
    Token current;

    // Utilities
    [[nodiscard]]
    AST::Block block();
    [[nodiscard]]
    AST::FunctionDeclaration::Parameter parameter();

    // Types
    [[nodiscard]]
    unique_ptr<AST::Type> type();

    // Declarations
    [[nodiscard]]
    unique_ptr<AST::Declaration> declaration();
    [[nodiscard]]
    unique_ptr<AST::FunctionDeclaration> functionDeclaration();
    [[nodiscard]]
    unique_ptr<AST::StructDeclaration> structDeclaration();
    [[nodiscard]]
    unique_ptr<AST::EnumDeclaration> enumDeclaration();
    [[nodiscard]]
    unique_ptr<AST::VarDeclaration> varDeclaration();
    [[nodiscard]]
    unique_ptr<AST::StatementDeclaration> statementDeclaration();

    // Statement
    [[nodiscard]]
    unique_ptr<AST::Statement> statement();
    [[nodiscard]]
    unique_ptr<AST::IfStatement> ifStatement();
    [[nodiscard]]
    unique_ptr<AST::ReturnStatement> returnStatement();
    [[nodiscard]]
    unique_ptr<AST::ExpressionStatement> expressionStatement();

    // Expression
    [[nodiscard]]
    unique_ptr<AST::Expression> expression();
    [[nodiscard]]
    unique_ptr<AST::BinaryExpression> binaryExpression();
    [[nodiscard]]
    unique_ptr<AST::Expression> expressionTerminal();
    [[nodiscard]]
    unique_ptr<AST::Literal> literal();

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
    Parser(std::string&& string) : scanner{std::move(string)}, previous{scanner.next()}, current{previous} {}

    ParsedFile parse();
};

#endif
