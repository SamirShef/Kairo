#pragma once
#include "../parser/ast.hpp"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include <llvm/IR/Value.h>
#include <stack>
#include <map>
#include <vector>

class CodeGenerator
{
private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;
    std::stack<std::map<std::string, llvm::Value*>> scopeStack;
    std::stack<std::map<std::string, Type>> typesScopeStack;
    std::stack<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> loopBlocks;

    struct FunctionInfo
    {
        Type returnType;
        AST::Arguments args;
        std::string mangledName;
        llvm::Function* function;
    };
    
    std::map<std::string, std::vector<FunctionInfo>> functions;

    struct ClassInfo
    {
        llvm::StructType* type;
        std::map<std::string, int> fieldIndices;
        std::map<std::string, std::vector<llvm::Function*>> methods;
        std::vector<llvm::Function*> constructors;
    };
    std::map<std::string, ClassInfo> classes;
    std::stack<std::string> classesStack;

    struct TraitInfo
    {
        std::map<std::string, std::vector<llvm::Function*>> methods;
    };
    std::map<std::string, TraitInfo> traits;

    struct TraitImplInfo
    {
        std::string traitName;
        std::string className;
        std::map<std::string, llvm::Function*> implementations;
    };
    std::map<std::string, std::vector<TraitImplInfo>> traitImplementations;
    
    std::map<std::string, llvm::ArrayType*> arrayTypes;
    std::map<std::string, Type> globalVariableTypes;
    
    std::map<std::string, std::string> arrayVariableNames;
    llvm::Function* charToStringFunc;
    Type currentFunctionReturnType;

    llvm::Type* getLLVMType(Type);
    llvm::Value* castToExpectedIfNeeded(llvm::Value* value, llvm::Type* expectedType);
    std::string resolveClassName(const AST::Expr&);
    std::string mangleFunction(const std::string&, const AST::Arguments&) const;
    std::string mangleFunction(const std::string&, const std::vector<Type>&) const;
    llvm::Function* declareFunctionPrototype(const AST::FuncDeclStmt&);
    
    void pushScope();
    void popScope();
    void setNamedValue(const std::string& name, llvm::Value* value);
    llvm::Value* getNamedValue(const std::string& name) const;
    Type getNamedType(const std::string& name) const;
    Type getVariableType(const std::string& name) const;
    
    void generateStmt(const AST::Stmt&);
    void generateVarDeclStmt(const AST::VarDeclStmt&);
    void generateArrayAsgnStmt(const AST::ArrayAsgnStmt&);
    void generateVarAsgnStmt(const AST::VarAsgnStmt&);
    void generateFieldAsgnStmt(const AST::FieldAsgnStmt&);
    void generateFuncDeclStmt(const AST::FuncDeclStmt&);
    void generateFuncCallStmt(const AST::FuncCallStmt&);
    void generateMethodCallStmt(const AST::MethodCallStmt&);
    void generateReturnStmt(const AST::ReturnStmt&);
    void generateIfElseStmt(const AST::IfElseStmt&);
    void generateWhileLoopStmt(const AST::WhileLoopStmt&);
    void generateDoWhileLoopStmt(const AST::DoWhileLoopStmt&);
    void generateForLoopStmt(const AST::ForLoopStmt&);
    void generateBreakStmt();
    void generateContinueStmt();
    void generateEchoStmt(const AST::EchoStmt&);
    void generateClassDeclStmt(const AST::ClassDeclStmt&);
    void generateFieldDeclStmt(const AST::FieldMember&);
    void generateMethodDeclStmt(const AST::MethodMember&);
    void generateConstructorDecl(const AST::ConstructorMember&);
    void generateTraitDeclStmt(const AST::TraitDeclStmt&);
    void generateTraitImplStmt(const AST::TraitImplStmt&);

    llvm::Value* generateExpr(const AST::Expr&);
    llvm::Value* generateLiteral(const AST::Literal&);
    llvm::Value* generateArrayLiteral(const AST::ArrayLiteral&);
    llvm::Value* generateBinaryExpr(const AST::BinaryExpr&);
    llvm::Value* generateUnaryExpr(const AST::UnaryExpr&);
    llvm::Value* generateVarExpr(const AST::VarExpr&);
    llvm::Value* generateArrayExpr(const AST::ArrayExpr&);
    llvm::Value* generateFuncCallExpr(const AST::FuncCallExpr&);
    llvm::Value* generateNewExpr(const AST::NewExpr&);
    llvm::Value* generateFieldAccessExpr(const AST::FieldAccessExpr&);
    llvm::Value* generateMethodCallExpr(const AST::MethodCallExpr&);
    llvm::Value* generateThisExpr(const AST::ThisExpr&);
    llvm::Value* generateSizeofExpr(const AST::SizeofExpr&);

public:
    CodeGenerator(const std::string&);
    
    void generate(const std::vector<AST::StmtPtr>&);
    
    void printIR() const;
    std::unique_ptr<llvm::Module> getModule() { return std::move(module); }
};