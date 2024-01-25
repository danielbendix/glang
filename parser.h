#ifndef LANG_parser_h
#define LANG_parser_h

#include "scanner.h"
#include "AST.h"

struct ParsedFile {
    //std::string path;
public:
    std::vector<unique_ptr<AST::Declaration>> declarations;

    ParsedFile(std::vector<unique_ptr<AST::Declaration>>&& declarations) : declarations{std::move(declarations)} {}
};

enum class Precedence;

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
    unique_ptr<AST::TypeNode> type();

    // Declarations
    [[nodiscard]]
    unique_ptr<AST::Declaration> declaration();
    [[nodiscard]]
    unique_ptr<AST::FunctionDeclaration> functionDeclaration();
    [[nodiscard]]
    unique_ptr<AST::StructDeclaration> structDeclaration();
    [[nodiscard]]
    unique_ptr<AST::ClassDeclaration> classDeclaration();
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
    unique_ptr<AST::WhileStatement> whileStatement();
    [[nodiscard]]
    unique_ptr<AST::Statement> assignmentOrExpression();

    // Expression
    [[nodiscard]]
    unique_ptr<AST::Expression> expression();

    [[nodiscard]]
    unique_ptr<AST::Expression> binaryExpression();

    // Expression helpers
    [[nodiscard]]
    unique_ptr<AST::Expression> expressionTerminal();

    std::unique_ptr<AST::Expression> parseExpression(Precedence precedence);

    [[nodiscard]]
    unique_ptr<AST::Expression> call(unique_ptr<AST::Expression>&& left);
    [[nodiscard]]
    unique_ptr<AST::Expression> dot(unique_ptr<AST::Expression>&& left);
    [[nodiscard]]
    unique_ptr<AST::Expression> binary(unique_ptr<AST::Expression>&& left);

    [[nodiscard]]
    unique_ptr<AST::Expression> literal();
    [[nodiscard]]
    unique_ptr<AST::Expression> identifier();
    [[nodiscard]]
    unique_ptr<AST::Expression> grouping();

    [[nodiscard]]
    unique_ptr<AST::Expression> unary();

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
