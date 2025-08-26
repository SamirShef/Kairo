#include "../../include/semantic/semanticanalyzer.hpp"
#include <algorithm>
#include <llvm/IR/Type.h>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

std::string typeToString(Type);

void SemanticAnalyzer::analyze(const std::vector<AST::StmtPtr>& stmts)
{
    variables.emplace(std::map<std::string, Type>{});
    for (const AST::StmtPtr& stmt : stmts) analyzeStmt(*stmt);
}

void SemanticAnalyzer::analyzeStmt(AST::Stmt& stmt)
{
    if (auto vds = dynamic_cast<AST::VarDeclStmt*>(&stmt)) analyzeVarDeclStmt(*vds);
    else if (auto vas = dynamic_cast<AST::VarAsgnStmt*>(&stmt)) analyzeVarAsgnStmt(*vas);
    else if (auto fds = dynamic_cast<AST::FuncDeclStmt*>(&stmt)) analyzeFuncDeclStmt(*fds);
    else if (auto rs = dynamic_cast<AST::ReturnStmt*>(&stmt)) analyzeReturnStmt(*rs);
    else if (auto ies = dynamic_cast<AST::IfElseStmt*>(&stmt)) analyzeIfElseStmt(*ies);
    else if (auto wls = dynamic_cast<AST::WhileLoopStmt*>(&stmt)) analyzeWhileLoopStmt(*wls);
    else if (auto dwls = dynamic_cast<AST::DoWhileLoopStmt*>(&stmt)) analyzeDoWhileLoopStmt(*dwls);
    else if (auto fls = dynamic_cast<AST::ForLoopStmt*>(&stmt)) analyzeForLoopStmt(*fls);
    else if (dynamic_cast<AST::BreakStmt*>(&stmt)) analyzeBreakStmt();
    else if (dynamic_cast<AST::ContinueStmt*>(&stmt)) analyzeContinueStmt();
    else if (auto es = dynamic_cast<AST::EchoStmt*>(&stmt)) analyzeEchoStmt(*es);
    else if (auto cds = dynamic_cast<AST::ClassDeclStmt*>(&stmt)) analyzeClassDeclStmt(*cds);
}

void SemanticAnalyzer::analyzeVarDeclStmt(AST::VarDeclStmt& vds)
{
    if (variables.size() == 1 && !isConstExpr(*vds.expr)) throw std::runtime_error("Global variable initializer must be a constant expression");

    if (variables.top().find(vds.name) != variables.top().end()) throw std::runtime_error("Variable '" + typeToString(vds.type) + " " + vds.name + "' already declared");

    variablesTypes.emplace(vds.type);
    Type exprType = analyzeExpr(*vds.expr);
    if (!canImplicitlyCast(exprType, vds.type)) throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(exprType) + " to " + typeToString(vds.type));
    variables.top().insert({vds.name, vds.type});
    variablesTypes.pop();
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
            Type exprType = analyzeExpr(*vas.expr);
            if (!canImplicitlyCast(exprType, it->second)) throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(exprType) + " to " + typeToString(it->second));

            return;
        }
        copy.pop();
    }

    if (!classesStack.empty())
    {
        std::string className = classesStack.top();
        auto classIt = classes.find(className);
        if (classIt != classes.end())
        {
            for (const auto& field : classIt->second.fields)
            {
                if (field->name == vas.name)
                {
                    Type exprType = analyzeExpr(*vas.expr);
                    if (!canImplicitlyCast(exprType, field->type)) throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(exprType) + " to " + typeToString(field->type));
                    
                    return;
                }
            }
        }
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

    functions.emplace(fds.name, FunctionInfo{ fds.retType, fds.args });

    functionReturnTypes.push(fds.retType);
    variables.emplace(std::map<std::string, Type>{});
    for (const auto& arg : fds.args) variables.top()[arg.name] = arg.type;
    for (const auto& stmt : fds.block) analyzeStmt(*stmt);
    variables.pop();
    functionReturnTypes.pop();
}

void SemanticAnalyzer::analyzeReturnStmt(AST::ReturnStmt& rs)
{
    if (functionReturnTypes.empty()) throw std::runtime_error("'return' outside of function");
    Type expected = functionReturnTypes.top();

    if (!rs.expr)
    {
        if (expected.type != TypeValue::VOID) throw std::runtime_error("Missing return value for function returning " + typeToString(expected) + "");
        return;
    }

    Type actual = analyzeExpr(*rs.expr);
    if (!canImplicitlyCast(actual, expected)) throw std::runtime_error("Type error: Cannot implicitly cast return value from " + typeToString(actual) + " to " + typeToString(expected));
}

void SemanticAnalyzer::analyzeIfElseStmt(AST::IfElseStmt& ies)
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("If statement cannot be outside function");

    Type condType = analyzeExpr(*ies.condExpr);

    if (condType.type != TypeValue::BOOL) throw std::runtime_error("Conditional expression must be return bool value");

    variables.emplace(std::map<std::string, Type>{});
    for (const auto& stmt : ies.thenBranch) analyzeStmt(*stmt);
    variables.pop();

    if (ies.elseBranch.size() != 0)
    {
        variables.emplace(std::map<std::string, Type>{});
        for (const auto& stmt : ies.elseBranch) analyzeStmt(*stmt);
        variables.pop();
    }
}

void SemanticAnalyzer::analyzeWhileLoopStmt(AST::WhileLoopStmt& wls)
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("While statement cannot be outside function");

    Type condType = analyzeExpr(*wls.condExpr);

    if (condType.type != TypeValue::BOOL) throw std::runtime_error("Conditional expression must be return bool value");

    variables.emplace(std::map<std::string, Type>{});
    loopDepth++;
    for (const auto& stmt : wls.block) analyzeStmt(*stmt);
    loopDepth--;
    variables.pop();
}

void SemanticAnalyzer::analyzeDoWhileLoopStmt(AST::DoWhileLoopStmt& dwls)
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("While statement cannot be outside function");

    Type condType = analyzeExpr(*dwls.condExpr);

    if (condType.type != TypeValue::BOOL) throw std::runtime_error("Conditional expression must be return bool value");

    variables.emplace(std::map<std::string, Type>{});
    loopDepth++;
    for (const auto& stmt : dwls.block) analyzeStmt(*stmt);
    loopDepth--;
    variables.pop();
}

void SemanticAnalyzer::analyzeForLoopStmt(AST::ForLoopStmt& fls)
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("While statement cannot be outside function");
    
    variables.emplace(std::map<std::string, Type>{});

    analyzeStmt(*fls.iterator);

    Type condType = analyzeExpr(*fls.condExpr);

    if (condType.type != TypeValue::BOOL) throw std::runtime_error("Conditional expression must be return bool value");

    analyzeStmt(*fls.iterationStmt);
    
    loopDepth++;
    for (const auto& stmt : fls.block) analyzeStmt(*stmt);
    loopDepth--;
    variables.pop();
}

void SemanticAnalyzer::analyzeBreakStmt()
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("break cannot be outside function");
    if (loopDepth == 0) throw std::runtime_error("break must be inside a loop");
}

void SemanticAnalyzer::analyzeContinueStmt()
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("continue cannot be outside function");
    if (loopDepth == 0) throw std::runtime_error("continue must be inside a loop");
}

void SemanticAnalyzer::analyzeEchoStmt(AST::EchoStmt& es)
{
    analyzeExpr(*es.expr);
}

void SemanticAnalyzer::analyzeClassDeclStmt(AST::ClassDeclStmt& cds)
{
    if (classes.find(cds.name) != classes.end()) throw std::runtime_error("Class '" + cds.name + "' already declared");

    ClassInfo classInfo;
    classesStack.push(cds.name);
    classes[cds.name] = std::move(classInfo);

    for (auto& member : cds.members)
    {
        if (auto field = dynamic_cast<AST::FieldMember*>(member.get()))
        {
            analyzeFieldMember(classInfo.fields, *field);
            classes[cds.name].fields.emplace_back(static_cast<AST::FieldMember*>(member.release()));
        }
        else if (auto method = dynamic_cast<AST::MethodMember*>(member.get()))
        {
            analyzeMethodMember(cds.name, classInfo.methods, *method);
            classes[cds.name].methods.emplace_back(static_cast<AST::MethodMember*>(member.release()));
        }
        else throw std::runtime_error("Unsupported class member declaration");
    }

    classesStack.pop();
}

void SemanticAnalyzer::analyzeFieldMember(std::vector<std::unique_ptr<AST::FieldMember>>& fields, AST::FieldMember& fm)
{
    for (const auto& existing : fields) if (existing->name == fm.name) throw std::runtime_error("Field '" + fm.name + "' already declared");

    Type exprType = analyzeExpr(*fm.expr);
    if (!canImplicitlyCast(exprType, fm.type)) throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(exprType) + " to " + typeToString(fm.type));
}

void SemanticAnalyzer::analyzeMethodMember(std::string className, std::vector<std::unique_ptr<AST::MethodMember>>& methods, AST::MethodMember& mm)
{
    for (const auto& existing : methods)
    {
        if (existing->name == mm.name && existing->args.size() == mm.args.size())
        {
            bool sameArgs = true;
            for (size_t i = 0; i < mm.args.size(); i++)
            {
                if (existing->args[i].type.type != mm.args[i].type.type)
                {
                    sameArgs = false;
                    break;
                }
            }

            if (sameArgs)
            {
                std::string argsStr;
                for (size_t i = 0; i < mm.args.size(); i++)
                {
                    argsStr.append(typeToString(mm.args[i].type));
                    if (i < mm.args.size() - 1) argsStr.append(", ");
                }
                
                throw std::runtime_error("Method '" + typeToString(mm.retType) + " " + mm.name + "(" + argsStr + ")' already declared");
            }
        }
    }

    functionReturnTypes.push(mm.retType);
    variables.emplace(std::map<std::string, Type>{});
    variables.top()["this"] = Type(TypeValue::CLASS, className);
    for (const auto& arg : mm.args) variables.top()[arg.name] = arg.type;
    for (const auto& stmt : mm.block) analyzeStmt(*stmt);
    variables.pop();
    functionReturnTypes.pop();
}

Type SemanticAnalyzer::analyzeExpr(const AST::Expr& expr)
{
    if (auto lit = dynamic_cast<const AST::Literal*>(&expr)) return analyzeLiteral(*lit);
    else if (auto binary = dynamic_cast<const AST::BinaryExpr*>(&expr)) return analyzeBinaryExpr(*binary);
    else if (auto unary = dynamic_cast<const AST::UnaryExpr*>(&expr)) return analyzeUnaryExpr(*unary);
    else if (auto var = dynamic_cast<const AST::VarExpr*>(&expr)) return analizeVarExpr(*var);
    else if (auto func = dynamic_cast<const AST::FuncCallExpr*>(&expr)) return analyzeFuncCallExpr(*func);
    else if (auto n = dynamic_cast<const AST::NewExpr*>(&expr)) return analyzeNewExpr(*n);
    
    throw std::runtime_error("Unknown expression type in semantic analysis");
}

Type SemanticAnalyzer::analyzeBinaryExpr(const AST::BinaryExpr& expr)
{
    Type leftType = analyzeExpr(*expr.left);
    Type rightType = analyzeExpr(*expr.right);

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
            
            return Type(TypeValue::BOOL, "bool");

        case TokenType::AND:
        case TokenType::OR:
            if (leftType.type != TypeValue::BOOL || rightType.type != TypeValue::BOOL) throw std::runtime_error("Logical operations require boolean operands");
            
            return Type(TypeValue::BOOL, "bool");
        default: throw std::runtime_error("Unsupported binary operator in semantic analysis");
    }
}

Type SemanticAnalyzer::analyzeUnaryExpr(const AST::UnaryExpr& expr)
{
    Type operandType = analyzeExpr(*expr.expr);
    
    switch (expr.op)
    {
        case TokenType::MINUS:
            if (operandType.type != TypeValue::INT && operandType.type != TypeValue::FLOAT && operandType.type != TypeValue::DOUBLE) throw std::runtime_error("Unary minus requires numeric operand");

            return operandType;
            
        case TokenType::NOT:
            if (operandType.type != TypeValue::BOOL) throw std::runtime_error("Logical NOT requires boolean operand");

            return Type(TypeValue::BOOL, "bool");
            
        default: throw std::runtime_error("Unsupported unary operator in semantic analysis");
    }
}

Type SemanticAnalyzer::analyzeLiteral(const AST::Literal& literal)
{
    return literal.type;
}

Type SemanticAnalyzer::analizeVarExpr(const AST::VarExpr& expr)
{
    auto copy = variables;
    while (!copy.empty())
    {
        const auto& scope = copy.top();
        auto it = scope.find(expr.name);
        if (it != scope.end()) return it->second;
        copy.pop();
    }

    if (!classesStack.empty())
    {
        std::string className = classesStack.top();
        auto classIt = classes.find(className);

        if (classIt != classes.end())
            for (const auto& field : classIt->second.fields)
                if (field->name == expr.name)
                    return field->type;
    }
    
    throw std::runtime_error("Variable '" + expr.name + "' does not exists");
}

Type SemanticAnalyzer::analyzeFuncCallExpr(const AST::FuncCallExpr& expr)
{
    auto it = functions.find(expr.name);
    if (it == functions.end()) throw std::runtime_error("Function '" + expr.name + "' does not exists");

    const FunctionInfo& info = it->second;

    if (info.args.size() != expr.args.size()) throw std::runtime_error("Function '" + expr.name + "' expects " + std::to_string(info.args.size()) + " argument(s), got " + std::to_string(expr.args.size()));

    for (size_t i = 0; i < expr.args.size(); ++i)
    {
        Type passed = analyzeExpr(*expr.args[i]);
        Type expected = info.args[i].type;
        if (!canImplicitlyCast(passed, expected)) throw std::runtime_error("Type error in call to '" + expr.name + "' at argument " + std::to_string(i) + ": expected " + typeToString(expected) + ", got " + typeToString(passed));
    }

    return info.returnType;
}

Type SemanticAnalyzer::analyzeNewExpr(const AST::NewExpr& expr)
{
    if (classes.find(expr.name) == classes.end()) throw std::runtime_error("Object '" + expr.name + "' does not exists");

    if (expr.name != variablesTypes.top().name) throw std::runtime_error("Type error: Cannot implicitly cast " + expr.name + " to " + variablesTypes.top().name);
    
    return Type(TypeValue::CLASS, expr.name);
}

bool SemanticAnalyzer::canImplicitlyCast(Type l, Type r)
{
    if (l == r) return true;

    if (l.type == TypeValue::CLASS || r.type == TypeValue::CLASS) return l.name == r.name;

    if (castsTable.find(l.type) != castsTable.end())
        if (std::find(castsTable[l.type].begin(), castsTable[l.type].end(), r.type) != castsTable[l.type].end())
            return true;

    return false;
}

bool SemanticAnalyzer::isConstExpr(const AST::Expr& expr) const
{
    if (dynamic_cast<const AST::Literal*>(&expr)) return true;
    if (auto bin = dynamic_cast<const AST::BinaryExpr*>(&expr)) return isConstExpr(*bin->left) && isConstExpr(*bin->right);
    if (auto un = dynamic_cast<const AST::UnaryExpr*>(&expr)) return isConstExpr(*un->expr);
    
    return false;
}

std::string typeToString(Type type)
{
    switch (type.type)
    {
        case TypeValue::INT: return "int";
        case TypeValue::FLOAT: return "float";
        case TypeValue::DOUBLE: return "double";
        case TypeValue::CHAR: return "char";
        case TypeValue::BOOL: return "bool";
        case TypeValue::STRING: return "string";
        case TypeValue::VOID: return "void";
        case TypeValue::CLASS: return "class <" + type.name + ">";
        default: return "unknown";
    }
}