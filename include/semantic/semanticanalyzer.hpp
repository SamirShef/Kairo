#pragma once
#include "../parser/ast.hpp"
#include <stack>
#include <string>
#include <vector>
#include <map>

class SemanticAnalyzer
{
public:
    void analyze(const std::vector<AST::StmtPtr>&);

private:
    std::stack<std::map<std::string, TypeValue>> variables;
    std::stack<TypeValue> functionReturnTypes;
    int loopDepth = 0;

    struct FunctionInfo
    {
        TypeValue returnType;
        AST::Arguments args;
    };
    std::map<std::string, FunctionInfo> functions;
    std::map<TypeValue, std::vector<TypeValue>> castsTable =
    {
        { TypeValue::INT, { TypeValue::FLOAT, TypeValue::DOUBLE } },
        { TypeValue::FLOAT, { TypeValue::DOUBLE } },
        { TypeValue::CHAR, { TypeValue::INT, TypeValue::FLOAT, TypeValue::DOUBLE } }
    };

    void analyzeStmt(AST::Stmt&);
    void analyzeVarDeclStmt(AST::VarDeclStmt&);
    void analyzeVarAsgnStmt(AST::VarAsgnStmt&);
    void analyzeFuncDeclStmt(AST::FuncDeclStmt&);
    void analyzeReturnStmt(AST::ReturnStmt&);
    void analyzeIfElseStmt(AST::IfElseStmt&);
    void analyzeWhileLoopStmt(AST::WhileLoopStmt&);
    void analyzeDoWhileLoopStmt(AST::DoWhileLoopStmt&);
    void analyzeForLoopStmt(AST::ForLoopStmt&);
    void analyzeBreakStmt();
    void analyzeContinueStmt();
    void analyzeEchoStmt(AST::EchoStmt&);

    TypeValue analyzeExpr(const AST::Expr&);
    TypeValue analyzeBinaryExpr(const AST::BinaryExpr&);
    TypeValue analyzeUnaryExpr(const AST::UnaryExpr&);
    TypeValue analyzeLiteral(const AST::Literal&);
    TypeValue analizeVarExpr(const AST::VarExpr&);
    TypeValue analyzeFuncCallExpr(const AST::FuncCallExpr&);

    bool canImplicitlyCast(TypeValue, TypeValue);

    bool isConstExpr(const AST::Expr&) const;
};