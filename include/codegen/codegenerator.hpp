#pragma once
#include "../parser/ast.hpp"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include <llvm/IR/Value.h>
#include <memory>
#include <stack>
#include <map>

class CodeGenerator
{
private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;
    std::stack<std::map<std::string, llvm::Value*>> scopeStack;
    std::stack<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> loopBlocks;

    struct ClassInfo
    {
        llvm::StructType* type;
        std::map<std::string, int> fieldIndices;
        std::map<std::string, llvm::Function*> methods;
    };
    std::map<std::string, ClassInfo> classes;
    std::stack<std::string> classesStack;

    llvm::Type* getLLVMType(Type);
    llvm::Value* castToExpectedIfNeeded(llvm::Value* value, llvm::Type* expectedType);
    
    void pushScope();
    void popScope();
    void setNamedValue(const std::string& name, llvm::Value* value);
    llvm::Value* getNamedValue(const std::string& name) const;
    
    void generateStmt(const AST::Stmt&);
    void generateVarDeclStmt(const AST::VarDeclStmt&);
    void generateVarAsgnStmt(const AST::VarAsgnStmt&);
    void generateFuncDeclStmt(const AST::FuncDeclStmt&);
    void generateFuncCallStmt(const AST::FuncCallStmt&);
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

    llvm::Value* generateExpr(const AST::Expr&);
    llvm::Value* generateLiteral(const AST::Literal&);
    llvm::Value* generateBinaryExpr(const AST::BinaryExpr&);
    llvm::Value* generateUnaryExpr(const AST::UnaryExpr&);
    llvm::Value* generateVarExpr(const AST::VarExpr&);
    llvm::Value* generateFuncCallExpr(const AST::FuncCallExpr&);
    llvm::Value* generateNewExpr(const AST::NewExpr&);
    llvm::Value* generateFieldAccessExpr(const AST::FieldAccessExpr&);
    llvm::Value* generateMethodCallExpr(const AST::MethodCallExpr&);
    llvm::Value* generateThisExpr(const AST::ThisExpr&);

public:
    CodeGenerator(const std::string&);
    
    void generate(const std::vector<AST::StmtPtr>&);
    
    void printIR() const;
    std::unique_ptr<llvm::Module> getModule() { return std::move(module); }
};