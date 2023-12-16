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

    };
private:
    Cause cause;
    Token token;

public:
    ParserException(Token token, Cause cause) : cause{cause}, token{token}  {}
};

class Parser {
private:
    Scanner scanner;
    Token previous;
    Token current;

    AST::Declaration *declaration();
    AST::FunctionDeclaration *functionDeclaration();
    AST::StructDeclaration *structDeclaration();

    AST::Statement *statement();
    AST::Expression *expression();

    void advance() {
        previous = current;
        current = scanner.next();
    }

    bool check(TokenType type) {
        return current.type == type;
    }

    bool match(TokenType type) {
        if (!check(type)) return false;
        advance();
        return true;
    }
    
    Token consume(TokenType type) {
        if (!check(type)) throw ParserException(current, ParserException::Cause::ExpectedClassName);

    }

public:
    Parser(std::string&& string) : scanner{std::move(string)}, previous{scanner.next()}, current{previous} {}

    ParsedFile parse();
};

#endif
