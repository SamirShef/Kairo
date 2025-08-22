#pragma once
#include "ast.hpp"
#include "../lexer/token.hpp"
#include <string>
#include <vector>

class Parser
{
public:
    Parser(std::vector<Token> t) : tokens(t)
    {
        pos = 0;
    }

    std::vector<AST::StmtPtr> parse();

private:
    std::vector<Token> tokens;
    int pos;

    AST::StmtPtr parseStmt();
    AST::StmtPtr parseVarDeclStmt();
    AST::StmtPtr parseFuncDeclStmt();
    AST::StmtPtr parseFuncCallStmt();
    AST::StmtPtr parseVarAsgnStmt(bool);
    AST::StmtPtr parseReturnStmt();
    AST::StmtPtr parseIfElseStmt();
    AST::StmtPtr parseWhileLoopStmt();
    AST::StmtPtr parseDoWhileLoopStmt();
    AST::StmtPtr parseForLoopStmt();
    AST::StmtPtr parseBreakStmt();
    AST::StmtPtr parseContinueStmt();
    AST::StmtPtr parseEchoStmt();

    AST::ExprPtr createCompoundAssignmentOperator(Token&);
    AST::ExprPtr parseExpr();
    AST::ExprPtr parseLogicalAnd();
    AST::ExprPtr parseLogicalOr();
    AST::ExprPtr parseEquality();
    AST::ExprPtr parseComparation();
    AST::ExprPtr parseMultiply();
    AST::ExprPtr parseAdditive();
    AST::ExprPtr parseUnary();
    AST::ExprPtr parsePrimary();

    TypeValue consumeType();

    Token peek();
    Token peek(int);
    Token consume(TokenType, std::string);

    bool match(TokenType);
};