#include "parser.h"

AST::Declaration * Parser::declaration() 
{
    if (match(TokenType::Fun)) return functionDeclaration();
    if (match(TokenType::Struct)) return structDeclaration();
}

AST::FunctionDeclaration * Parser::functionDeclaration()
{
    auto name = consume(TokenType::Identifier);

    consume(TokenType::LeftParenthesis);

    while (!check(TokenType::RightParenthesis)) {

    }

    consume(TokenType::RightParenthesis);




}

AST::StructDeclaration * Parser::structDeclaration()
{

}

AST::EnumDeclaration * Parser::enumDeclaration()
{

}

AST::StatementDeclaration * Parser::statementDeclaration()
{

}

AST::Statement * Parser::statement()
{

}

AST::Expression *expression();
