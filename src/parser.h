#ifndef LANG_parser_h
#define LANG_parser_h

#include "scanner.h"
#include "AST.h"

struct ParsedFile {
    //std::string path;
public:
    std::vector<AST::unique_ptr<AST::Declaration>> declarations;

    ParsedFile(std::vector<AST::unique_ptr<AST::Declaration>>&& declarations) : declarations{std::move(declarations)} {}
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

template <typename T>
using unique_ptr = unique_ptr_t<T>;

class Parser {
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
    unique_ptr<AST::TypeNode> type(bool hasIdentifier = false);

    // Bindings
    [[nodiscard]]
    unique_ptr<AST::Binding> binding();

    // Declarations
    [[nodiscard]]
    unique_ptr<AST::Declaration> declaration();
    [[nodiscard]]
    unique_ptr<AST::FunctionDeclaration> functionDeclaration();
    [[nodiscard]]
    unique_ptr<AST::StructDeclaration> structDeclaration();
    [[nodiscard]]
    unique_ptr<AST::FunctionDeclaration> initializerDeclaration();
    [[nodiscard]]
    unique_ptr<AST::EnumDeclaration> enumDeclaration();
    [[nodiscard]]
    AST::EnumDeclaration::Case enumCase();
    [[nodiscard]]
    AST::EnumDeclaration::Case::Member enumCaseMember();
    [[nodiscard]]
    unique_ptr<AST::VariableDeclaration> variableDeclaration();
    [[nodiscard]]
    unique_ptr<AST::StatementDeclaration> statementDeclaration();

    // Statement
    [[nodiscard]]
    unique_ptr<AST::Statement> statement();
    [[nodiscard]]
    unique_ptr<AST::IfStatement> ifStatement();
    [[nodiscard]]
    unique_ptr<AST::ForStatement> forStatement();
    [[nodiscard]]
    unique_ptr<AST::GuardStatement> guardStatement();
    [[nodiscard]]
    unique_ptr<AST::ReturnStatement> returnStatement();
    [[nodiscard]]
    unique_ptr<AST::WhileStatement> whileStatement();
    [[nodiscard]]
    unique_ptr<AST::BreakStatement> breakStatement();
    [[nodiscard]]
    unique_ptr<AST::ContinueStatement> continueStatement();
    [[nodiscard]]
    unique_ptr<AST::Statement> assignmentOrExpression();

    // Expression
    [[nodiscard]]
    unique_ptr<AST::Expression> expression(ExpressionRules rules);

    // Expression helpers
    unique_ptr<AST::Expression> parseExpression(Precedence precedence);

    [[nodiscard]]
    unique_ptr<AST::Expression> call(unique_ptr<AST::Expression>&& left);
    [[nodiscard]]
    unique_ptr<AST::Expression> subscript(unique_ptr<AST::Expression>&& left);
    [[nodiscard]]
    unique_ptr<AST::Expression> member(unique_ptr<AST::Expression>&& left);
    [[nodiscard]]
    unique_ptr<AST::Expression> binary(unique_ptr<AST::Expression>&& left);

    [[nodiscard]]
    unique_ptr<AST::Expression> literal();
    [[nodiscard]]
    unique_ptr<AST::Expression> identifier();
    [[nodiscard]]
    unique_ptr<AST::Expression> self();
    [[nodiscard]]
    unique_ptr<AST::Expression> grouping();
    [[nodiscard]]
    unique_ptr<AST::Expression> inferredInitializer();
    [[nodiscard]]
    unique_ptr<AST::Expression> inferredMember();

    [[nodiscard]]
    unique_ptr<AST::Expression> initializer(unique_ptr<AST::Identifier>&& identifier);

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
