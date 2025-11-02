#ifndef LANG_parser_h
#define LANG_parser_h

#include "scanner.h"
#include "AST.h"
#include "context.h"
#include "location.h"
#include "diagnostic.h"
#include "ids.h"

#include "containers/symbol_table.h"
#include "containers/span.h"
#include "containers/optional.h"

#include <csetjmp>

enum class ParseResult {
    OK = 0,
    INVALID = 1,
    FATAL = 2,
};

struct ParsedFile {
public:
    const u32 size;
    const ParseResult result;
    std::vector<AST::Declaration *> declarations;
    std::vector<u32> lineBreaks;
    std::unique_ptr<ASTHandle> astHandle;
    DiagnosticBuffer diagnostics;

    ParsedFile(
        u32 size, 
        ParseResult result, 
        std::vector<AST::Declaration *>&& declarations, 
        std::vector<u32>&& lineBreaks, 
        std::unique_ptr<ASTHandle>&& astHandle,
        DiagnosticBuffer&& diagnostics
    )   : size{size}
        , result{result}
        , declarations{std::move(declarations)}
        , lineBreaks{std::move(lineBreaks)}
        , astHandle{std::move(astHandle)} 
        , diagnostics{std::move(diagnostics)}
        {}

    ParsedFile(const ParsedFile&) = delete;
    ParsedFile(ParsedFile&&) = default;
};

ParsedFile parseFile(FileID fileID, File& file, DiagnosticWriter& writer);

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

class Parser {
    struct Modifiers {
        u32 offset = 0;
        u32 length = 0;
        AST::Modifiers modifiers; 
    };

    SymbolTable& symbols;

    BumpAllocator nodeAllocator;
    ArrayArenaAllocator arrayAllocator;
    Scanner scanner;
    Token previous;
    Token current;

    FileID fileID;

    jmp_buf startPoint;
    DiagnosticBuffer diagnostics;

    struct ExpressionRules {
        bool allowInitializer = true;
    } expressionRules;

    // Utilities
    [[nodiscard]]
    Modifiers parseModifiers();
    [[nodiscard]]
    bool checkModifiers(Modifiers modifiers, AST::Modifiers allowed);
    [[nodiscard]]
    AST::Block block();
    [[nodiscard]]
    Optional<AST::FunctionParameter> parameter();

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
    AST::VariableDeclaration *variableDeclaration(Modifiers modifiers);
    [[nodiscard]]
    AST::StatementDeclaration *statementDeclaration();

    [[nodiscard]]
    AST::ConditionalUnwrap *unwrap();
    [[nodiscard]]
    Span<AST::Condition> conditions();

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
    AST::Expression *call(AST::Expression *callee);
    [[nodiscard]]
    AST::Expression *subscript(AST::Expression *target);
    [[nodiscard]]
    AST::Expression *member(AST::Expression *target);
    [[nodiscard]]
    AST::Expression *binary(AST::Expression *left);

    [[nodiscard]]
    AST::Expression *literal();
    [[nodiscard]]
    AST::Literal *createCharacterLiteral(Token token);
    [[nodiscard]]
    AST::Literal *createStringLiteral(Token token);
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

    void error(std::string&& message, DiagnosticLocation location);

    void advance() {
        previous = current;
        current = scanner.next();
    }

    [[nodiscard]]
    bool check(TokenType type) const {
        return current.type == type;
    }

    [[nodiscard]]
    bool match(TokenType type) {
        if (!check(type)) {
            return false;
        }
        advance();
        return true;
    }
   
    Optional<Token> consume(TokenType type);

    [[nodiscard]]
    std::string_view toStringView(Token token) const {
        return token.string_view(scanner._string.data());
    };

    /// End parsing early due to an unrecoverable error.
    [[noreturn]]
    void earlyExit() {
        siglongjmp(startPoint, 1);
    }

    friend class ParseRule;
public:
    Parser(FileID fileID, SymbolTable& symbols, std::string&& string) 
        : fileID{fileID}
        , symbols{symbols}
        , scanner{std::move(string)}
        , previous{scanner.next()}
        , current{previous} {}

    ParsedFile parse(DiagnosticWriter& writer);

    friend struct ParsingError;
};

#endif
