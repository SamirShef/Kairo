#pragma once
#include "../parser/ast.hpp"
#include <stack>
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
        std::string mangledName;

        FunctionInfo() = default;
    };
    
    std::map<std::string, std::vector<FunctionInfo>> functions;
    std::stack<Type> functionReturnTypes;

    struct ClassInfo
    {
        std::vector<std::unique_ptr<AST::FieldMember>> fields;
        std::vector<std::unique_ptr<AST::MethodMember>> methods;
        std::vector<std::unique_ptr<AST::ConstructorMember>> constructors;

        ClassInfo() = default;
        ~ClassInfo() = default;

        ClassInfo(const ClassInfo&) = delete;
        ClassInfo& operator=(const ClassInfo&) = delete;

        ClassInfo(ClassInfo&& other) noexcept : 
            fields(std::move(other.fields)), 
            methods(std::move(other.methods)),
            constructors(std::move(other.constructors)) {}
            
        ClassInfo& operator=(ClassInfo&& other) noexcept
        {
            fields = std::move(other.fields);
            methods = std::move(other.methods);
            constructors = std::move(other.constructors);
            
            return *this;
        }
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
    void analyzeArrayAsgnStmt(AST::ArrayAsgnStmt&);
    void analyzeVarAsgnStmt(AST::VarAsgnStmt&);
    void analyzeFieldAsgnStmt(AST::FieldAsgnStmt&);
    void analyzeFuncDeclStmt(AST::FuncDeclStmt&);
    void analyzeFuncCallStmt(AST::FuncCallStmt&);
    void analyzeMethodCallStmt(AST::MethodCallStmt&);
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
    void analyzeConstructorMember(std::string, std::vector<std::unique_ptr<AST::ConstructorMember>>& members, AST::ConstructorMember&);

    Type analyzeExpr(const AST::Expr&);
    Type analyzeBinaryExpr(const AST::BinaryExpr&);
    Type analyzeUnaryExpr(const AST::UnaryExpr&);
    Type analyzeLiteral(const AST::Literal&);
    Type analyzeArrayLiteral(const AST::ArrayLiteral&);
    Type analizeVarExpr(const AST::VarExpr&);
    Type analyzeArrayExpr(const AST::ArrayExpr&);
    Type analyzeFuncCallExpr(const AST::FuncCallExpr&);
    Type analyzeNewExpr(const AST::NewExpr&);
    Type analyzeFieldAccessExpr(const AST::FieldAccessExpr&);
    Type analyzeMethodCallExpr(const AST::MethodCallExpr&);
    Type analyzeThisExpr(const AST::ThisExpr&);

    bool canImplicitlyCast(Type, Type);

    bool isConstExpr(const AST::Expr&) const;

    std::string mangleFunction(const std::string&, const AST::Arguments&) const;
    std::string mangleFunction(const std::string&, const std::vector<Type>&) const;
};