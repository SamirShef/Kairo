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
    std::stack<std::map<std::string, Type>> variables;
    std::stack<Type> variablesTypes;
    
    int loopDepth = 0;
    
    struct FunctionInfo
    {
        Type returnType;
        AST::Arguments args;

        FunctionInfo() = default;
    };
    std::map<std::string, FunctionInfo> functions;
    std::stack<Type> functionReturnTypes;

    struct ClassInfo
    {
        std::vector<std::unique_ptr<AST::FieldMember>> fields;
        std::vector<std::unique_ptr<AST::MethodMember>> methods;
    };
    std::map<std::string, ClassInfo> classes;
    std::stack<std::string> classesStack;
    
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
    void analyzeClassDeclStmt(AST::ClassDeclStmt&);
    void analyzeFieldMember(std::vector<std::unique_ptr<AST::FieldMember>>& members, AST::FieldMember&);
    void analyzeMethodMember(std::string, std::vector<std::unique_ptr<AST::MethodMember>>& members, AST::MethodMember&);

    Type analyzeExpr(const AST::Expr&);
    Type analyzeBinaryExpr(const AST::BinaryExpr&);
    Type analyzeUnaryExpr(const AST::UnaryExpr&);
    Type analyzeLiteral(const AST::Literal&);
    Type analizeVarExpr(const AST::VarExpr&);
    Type analyzeFuncCallExpr(const AST::FuncCallExpr&);
    Type analyzeNewExpr(const AST::NewExpr&);

    bool canImplicitlyCast(Type, Type);

    bool isConstExpr(const AST::Expr&) const;
};