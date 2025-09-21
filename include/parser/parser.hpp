#pragma once
#include "ast.hpp"
#include "../lexer/token.hpp"

class Parser
{
public:
    Parser(std::vector<Token> t, std::string cfp, std::string lp = "") : tokens(t), currentFilePath(cfp), libsPath(lp) { pos = 0; }

    std::vector<AST::StmtPtr> parse();

private:
    std::vector<Token> tokens;
    int pos;
    std::string currentFilePath;
    std::string libsPath;
    
    void parseIncludeStmt(std::vector<AST::StmtPtr>* stmts);
    
    AST::StmtPtr parseStmt();
    AST::StmtPtr parseVarDeclStmt();
    AST::StmtPtr parseConstDeclStmt();
    AST::StmtPtr parseArrayDeclStmt(Type elementType, std::string name);
    AST::StmtPtr parseFuncDeclStmt();
    AST::StmtPtr parseFuncCallStmt();
    AST::StmtPtr parseThisStmt();
    AST::StmtPtr parseVarAsgnStmt(bool);
    AST::StmtPtr parseObjectChainStmt();
    AST::StmtPtr parseReturnStmt();
    AST::StmtPtr parseIfElseStmt();
    AST::StmtPtr parseWhileLoopStmt();
    AST::StmtPtr parseDoWhileLoopStmt();
    AST::StmtPtr parseForLoopStmt();
    AST::StmtPtr parseBreakStmt();
    AST::StmtPtr parseContinueStmt();
    AST::StmtPtr parseEchoStmt();
    AST::StmtPtr parseClassDeclStmt();
    std::unique_ptr<AST::Member> parseFieldDecl(AST::AccessModifier&);
    std::unique_ptr<AST::Member> parseConstFieldDecl(AST::AccessModifier&);
    std::unique_ptr<AST::Member> parseMethodDecl(AST::AccessModifier&);
    std::unique_ptr<AST::Member> parseConstructorDecl(AST::AccessModifier&);
    AST::StmtPtr parseTraitDeclStmt();
    AST::StmtPtr parseTraitImplStmt();
    std::unique_ptr<AST::Member> parseTraitMethodDecl(AST::AccessModifier&);

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
    AST::ExprPtr parseNewExpr();
    AST::ExprPtr parseSizeofExpr();
    AST::ExprPtr parseArrayLiteral();
    AST::ExprPtr parseCallsChain(AST::ExprPtr);
    AST::ExprPtr parseFieldAccessExpr(AST::ExprPtr, std::string&);
    AST::ExprPtr parseMethodCallExpr(AST::ExprPtr, std::string&);
    AST::ExprPtr parseThisExpr();

    AST::Arguments parseArguments();
    
    Type consumeType();
    std::pair<Type, AST::ExprPtr> parseArrayType();

    Token peek();
    Token peek(int);
    Token consume(TokenType, const std::string&);

    bool match(TokenType);
};