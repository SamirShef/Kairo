#include "../../include/semantic/semanticanalyzer.hpp"
#include <algorithm>
#include <llvm/IR/Type.h>
#include <stdexcept>
#include <string>
#include <vector>

std::string typeToString(TypeValue);

void SemanticAnalyzer::analyze(const std::vector<AST::StmtPtr>& stmts)
{
    variables.emplace(std::map<std::string, TypeValue>{});
    for (const AST::StmtPtr& stmt : stmts) analyzeStmt(*stmt);
}

void SemanticAnalyzer::analyzeStmt(AST::Stmt& stmt)
{
    if (auto vds = dynamic_cast<AST::VarDeclStmt*>(&stmt)) analyzeVarDeclStmt(*vds);
    else if (auto vas = dynamic_cast<AST::VarAsgnStmt*>(&stmt)) analyzeVarAsgnStmt(*vas);
    else if (auto fds = dynamic_cast<AST::FuncDeclStmt*>(&stmt)) analyzeFuncDeclStmt(*fds);
    else if (auto rs = dynamic_cast<AST::ReturnStmt*>(&stmt)) analyzeReturnStmt(*rs);
    else if (auto ies = dynamic_cast<AST::IfElseStmt*>(&stmt)) analyzeIfElseStmt(*ies);
    else if (auto es = dynamic_cast<AST::EchoStmt*>(&stmt)) analyzeEchoStmt(*es);
}

void SemanticAnalyzer::analyzeVarDeclStmt(AST::VarDeclStmt& vds)
{
    if (variables.size() == 1 && !isConstExpr(*vds.expr)) throw std::runtime_error("Global variable initializer must be a constant expression");

    if (variables.top().find(vds.name) != variables.top().end()) throw std::runtime_error("Variable '" + typeToString(vds.type) + " " + vds.name + "' already declared");

    TypeValue exprType = analyzeExpr(*vds.expr);
    if (!canImplicitlyCast(exprType, vds.type)) throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(exprType) + " to " + typeToString(vds.type));

    variables.top()[vds.name] = vds.type;
}

void SemanticAnalyzer::analyzeVarAsgnStmt(AST::VarAsgnStmt& vas)
{
    auto copy = variables;
    while (!copy.empty())
    {
        const auto& scope = copy.top();
        auto it = scope.find(vas.name);
        if (it != scope.end())
        {
            TypeValue exprType = analyzeExpr(*vas.expr);
            if (!canImplicitlyCast(exprType, it->second)) throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(exprType) + " to " + typeToString(it->second));

            return;
        }
        copy.pop();
    }
    
    throw std::runtime_error("Variable '" + vas.name + "' does not exists");
}

void SemanticAnalyzer::analyzeFuncDeclStmt(AST::FuncDeclStmt& fds)
{
    if (functions.find(fds.name) != functions.end())
    {
        if (functions[fds.name].args == fds.args)
        {
            std::string args;
            for (int i = 0; i < fds.args.size(); i++)
            {
                args.append(typeToString(fds.args[i].type));
                if (i < fds.args.size() - 1) args.append(", ");
            }

            throw std::runtime_error("Function '" + typeToString(fds.retType) + " " + fds.name + "(" + args + ")' already declared");
        }
    }

    AST::Arguments args;
    AST::Block block;

    for (const AST::Argument& arg : fds.args) args.push_back(arg);

    functions.emplace(fds.name, FunctionInfo{ fds.retType, args });

    functionReturnTypes.push(fds.retType);
    variables.emplace(std::map<std::string, TypeValue>{});
    for (const auto& arg : args) variables.top()[arg.name] = arg.type;
    for (const auto& stmt : fds.block) analyzeStmt(*stmt);
    variables.pop();
    functionReturnTypes.pop();
}

void SemanticAnalyzer::analyzeReturnStmt(AST::ReturnStmt& rs)
{
    if (functionReturnTypes.empty()) throw std::runtime_error("'return' outside of function");
    TypeValue expected = functionReturnTypes.top();

    if (!rs.expr)
    {
        if (expected != TypeValue::VOID) throw std::runtime_error("Missing return value for function returning " + typeToString(expected) + "");
        return;
    }

    TypeValue actual = analyzeExpr(*rs.expr);
    if (!canImplicitlyCast(actual, expected)) throw std::runtime_error("Type error: Cannot implicitly cast return value from " + typeToString(actual) + " to " + typeToString(expected));
}

void SemanticAnalyzer::analyzeIfElseStmt(AST::IfElseStmt& ies)
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("If statement cannot be outside function");

    TypeValue condType = analyzeExpr(*ies.condExpr);

    if (condType != TypeValue::BOOL) throw std::runtime_error("Conditional expression must be return bool value");

    variables.emplace(std::map<std::string, TypeValue>{});
    for (const auto& stmt : ies.thenBranch) analyzeStmt(*stmt);
    variables.pop();

    if (ies.elseBranch.size() != 0)
    {
        variables.emplace(std::map<std::string, TypeValue>{});
        for (const auto& stmt : ies.elseBranch) analyzeStmt(*stmt);
        variables.pop();
    }
}

void SemanticAnalyzer::analyzeEchoStmt(AST::EchoStmt& es)
{
    analyzeExpr(*es.expr);
}

TypeValue SemanticAnalyzer::analyzeExpr(const AST::Expr& expr)
{
    if (auto lit = dynamic_cast<const AST::Literal*>(&expr)) return analyzeLiteral(*lit);
    else if (auto binary = dynamic_cast<const AST::BinaryExpr*>(&expr)) return analyzeBinaryExpr(*binary);
    else if (auto unary = dynamic_cast<const AST::UnaryExpr*>(&expr)) return analyzeUnaryExpr(*unary);
    else if (auto var = dynamic_cast<const AST::VarExpr*>(&expr)) return analizeVarExpr(*var);
    else if (auto func = dynamic_cast<const AST::FuncCallExpr*>(&expr)) return analyzeFuncCallExpr(*func);
    
    throw std::runtime_error("Unknown expression type in semantic analysis");
}

TypeValue SemanticAnalyzer::analyzeBinaryExpr(const AST::BinaryExpr& expr)
{
    TypeValue leftType = analyzeExpr(*expr.left);
    TypeValue rightType = analyzeExpr(*expr.right);

    switch (expr.op)
    {
        case TokenType::PLUS:
        case TokenType::MINUS:
        case TokenType::MULTIPLY:
        case TokenType::DIVIDE:
        case TokenType::MODULO:
            if (!canImplicitlyCast(leftType, rightType) && !canImplicitlyCast(rightType, leftType)) throw std::runtime_error("Type error: " + typeToString(leftType) + " and " + typeToString(rightType));

            return leftType;
        
        case TokenType::EQUALS:
        case TokenType::NOT_EQUALS:
        case TokenType::GREATER:
        case TokenType::GREATER_EQUALS:
        case TokenType::LESS:
        case TokenType::LESS_EQUALS:
            if (!canImplicitlyCast(leftType, rightType) && !canImplicitlyCast(rightType, leftType)) throw std::runtime_error("Type error: " + typeToString(leftType) + " and " + typeToString(rightType));
            
            return TypeValue::BOOL;

        case TokenType::AND:
        case TokenType::OR:
            if (leftType != TypeValue::BOOL || rightType != TypeValue::BOOL) throw std::runtime_error("Logical operations require boolean operands");
            
            return TypeValue::BOOL;
        default: throw std::runtime_error("Unsupported binary operator in semantic analysis");
    }
}

TypeValue SemanticAnalyzer::analyzeUnaryExpr(const AST::UnaryExpr& expr)
{
    TypeValue operandType = analyzeExpr(*expr.expr);
    
    switch (expr.op)
    {
        case TokenType::MINUS:
            if (operandType != TypeValue::INT && operandType != TypeValue::FLOAT && operandType != TypeValue::DOUBLE) throw std::runtime_error("Unary minus requires numeric operand");

            return operandType;
            
        case TokenType::NOT:
            if (operandType != TypeValue::BOOL) throw std::runtime_error("Logical NOT requires boolean operand");

            return TypeValue::BOOL;
            
        default: throw std::runtime_error("Unsupported unary operator in semantic analysis");
    }
}

TypeValue SemanticAnalyzer::analyzeLiteral(const AST::Literal& literal)
{
    return literal.type;
}

TypeValue SemanticAnalyzer::analizeVarExpr(const AST::VarExpr& expr)
{
    auto copy = variables;
    while (!copy.empty())
    {
        const auto& scope = copy.top();
        auto it = scope.find(expr.name);
        if (it != scope.end()) return it->second;
        copy.pop();
    }
    
    throw std::runtime_error("Variable '" + expr.name + "' does not exists");
}

TypeValue SemanticAnalyzer::analyzeFuncCallExpr(const AST::FuncCallExpr& expr)
{
    auto it = functions.find(expr.name);
    if (it == functions.end()) throw std::runtime_error("Function '" + expr.name + "' does not exists");

    const FunctionInfo& info = it->second;

    if (info.args.size() != expr.args.size()) throw std::runtime_error("Function '" + expr.name + "' expects " + std::to_string(info.args.size()) + " argument(s), got " + std::to_string(expr.args.size()));

    for (size_t i = 0; i < expr.args.size(); ++i)
    {
        TypeValue passed = analyzeExpr(*expr.args[i]);
        TypeValue expected = info.args[i].type;
        if (!canImplicitlyCast(passed, expected)) throw std::runtime_error("Type error in call to '" + expr.name + "' at argument " + std::to_string(i) + ": expected " + typeToString(expected) + ", got " + typeToString(passed));
    }

    return info.returnType;
}

bool SemanticAnalyzer::canImplicitlyCast(TypeValue l, TypeValue r)
{
    if (l == r) return true;

    if (castsTable.find(l) != castsTable.end()) if (std::find(castsTable[l].begin(), castsTable[l].end(), r) != castsTable[l].end()) return true;

    return false;
}

bool SemanticAnalyzer::isConstExpr(const AST::Expr& expr) const
{
    if (dynamic_cast<const AST::Literal*>(&expr)) return true;
    if (auto bin = dynamic_cast<const AST::BinaryExpr*>(&expr)) return isConstExpr(*bin->left) && isConstExpr(*bin->right);
    if (auto un = dynamic_cast<const AST::UnaryExpr*>(&expr)) return isConstExpr(*un->expr);
    
    return false;
}

std::string typeToString(TypeValue type) {
    switch (type)
    {
        case TypeValue::INT: return "int";
        case TypeValue::FLOAT: return "float";
        case TypeValue::DOUBLE: return "double";
        case TypeValue::CHAR: return "char";
        case TypeValue::BOOL: return "bool";
        case TypeValue::STRING: return "string";
        case TypeValue::VOID: return "void";
        default: return "unknown";
    }
}