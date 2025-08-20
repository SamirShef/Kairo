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
    };

    void analyzeStmt(AST::Stmt&);
    void analyzeVarDeclStmt(AST::VarDeclStmt&);
    void analyzeVarAsgnStmt(AST::VarAsgnStmt&);
    void analyzeFuncDeclStmt(AST::FuncDeclStmt&);
    void analyzeEchoStmt(AST::EchoStmt&);

    TypeValue analyzeExpr(const AST::Expr&);
    TypeValue analyzeBinaryExpr(const AST::BinaryExpr&);
    TypeValue analyzeUnaryExpr(const AST::UnaryExpr&);
    TypeValue analyzeLiteral(const AST::Literal&);
    TypeValue analizeVarExpr(const AST::VarExpr&);
    TypeValue analyzeFuncCallExpr(const AST::FuncCallExpr&);

    bool canImplicitlyCast(TypeValue, TypeValue);

    // Проверка на константное выражение для глобальных инициализаторов
    bool isConstExpr(const AST::Expr&) const;
};