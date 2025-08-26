#pragma once
#include "../values/value.hpp"
#include "../lexer/token.hpp"
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace AST
{
    class Node
    {
    public:
        virtual ~Node() = default;
    };

    class Stmt : public Node
    {
    public:
        virtual ~Stmt() = default;
    };
    class Expr : public Node
    {
    public:
        virtual ~Expr() = default;
    };

    using StmtPtr = std::unique_ptr<Stmt>;
    using ExprPtr = std::unique_ptr<Expr>;

    class Argument
    {
    public:
        Type type;
        std::string name;

        Argument() = default;
        Argument(Type t, std::string n) : type(t), name(n) {}

        bool operator==(const Argument& other) const
        {
            return type.type == other.type.type && type.name == other.type.name && name == other.name;
        }
        
        bool operator!=(const Argument& other) const
        {
            return !(*this == other);
        }
    };

    using Arguments = std::vector<Argument>;
    using Block = std::vector<StmtPtr>;

    enum class AccessModifier
    {
        PUBLIC, PRIVATE
    };

    class Member
    {
    public:
        AccessModifier access;
        std::string name;

        Member(AccessModifier am, std::string n) : access(am), name(n) {}

        virtual ~Member() = default;
    };
    
    class FieldMember : public Member
    {
    public:
        Type type;
        ExprPtr expr;

        FieldMember(AccessModifier am, std::string n, Type t, ExprPtr e) : Member(am, n), type(t), expr(std::move(e)) {}
        
        ~FieldMember() override = default;
    };

    class MethodMember : public Member
    {
    public:
        Type retType;
        Arguments args;
        Block block;

        MethodMember(AccessModifier am, std::string n, Type rt, Arguments a, Block b) : Member(am, n), retType(rt), args(std::move(a)), block(std::move(b)) {}
        
        ~MethodMember() override = default;
    };

    struct ClassInstance
    {
        std::string name;
        std::vector<FieldMember> fields;
        std::vector<MethodMember> methods;
    };
    
    class VarDeclStmt : public Stmt
    {
    public:
        std::string name;
        Type type;
        ExprPtr expr;

        VarDeclStmt(std::string n, Type t, ExprPtr e) : name(n), type(t), expr(std::move(e)) {}

        ~VarDeclStmt() override = default;
    };

    class VarAsgnStmt : public Stmt
    {
    public:
        std::string name;
        ExprPtr expr;

        VarAsgnStmt(std::string n, ExprPtr e) : name(n), expr(std::move(e)) {}

        ~VarAsgnStmt() override = default;
    };

    class FuncDeclStmt : public Stmt
    {
    public:
        std::string name;
        Type retType;
        Arguments args;
        Block block;

        FuncDeclStmt(std::string n, Type rt, Arguments a, Block b) : name(n), retType(rt), args(std::move(a)), block(std::move(b)) {}

        ~FuncDeclStmt() override = default;
    };

    class FuncCallStmt : public Stmt
    {
    public:
        std::string name;
        std::vector<ExprPtr> args;

        FuncCallStmt(std::string n, std::vector<ExprPtr> a) : name(n), args(std::move(a)) {}

        ~FuncCallStmt() override = default;
    };

    class ReturnStmt : public Stmt
    {
public:
        ExprPtr expr;

        ReturnStmt(ExprPtr e) : expr(std::move(e)) {}

        ~ReturnStmt() override = default;
    };

    class IfElseStmt : public Stmt
    {
    public:
        ExprPtr condExpr;
        Block thenBranch;
        Block elseBranch;

        IfElseStmt(ExprPtr ce, Block tb, Block eb) : condExpr(std::move(ce)), thenBranch(std::move(tb)), elseBranch(std::move(eb)) {}

        ~IfElseStmt() override = default;
    };

    class WhileLoopStmt : public Stmt
    {
    public:
        ExprPtr condExpr;
        Block block;

        WhileLoopStmt(ExprPtr ce, Block b) : condExpr(std::move(ce)), block(std::move(b)) {}

        ~WhileLoopStmt() override = default;
    };
    
    class DoWhileLoopStmt : public Stmt
    {
    public:
        ExprPtr condExpr;
        Block block;

        DoWhileLoopStmt(ExprPtr ce, Block b) : condExpr(std::move(ce)), block(std::move(b)) {}

        ~DoWhileLoopStmt() override = default;
    };

    class ForLoopStmt : public Stmt
    {
    public:
        StmtPtr iterator;
        ExprPtr condExpr;
        StmtPtr iterationStmt;
        Block block;

        ForLoopStmt(StmtPtr i, ExprPtr ce, StmtPtr is, Block b) : iterator(std::move(i)), condExpr(std::move(ce)), iterationStmt(std::move(is)), block(std::move(b)) {}

        ~ForLoopStmt() override = default;
    };

    class BreakStmt : public Stmt
    {
    public:
        ~BreakStmt() override = default;
    };

    class ContinueStmt : public Stmt
    {
    public:
        ~ContinueStmt() override = default;
    };

    class EchoStmt : public Stmt
    {
    public:
        ExprPtr expr;

        EchoStmt(ExprPtr e) : expr(std::move(e)) {}

        ~EchoStmt() override = default;
    };

    class ClassDeclStmt : public Stmt
    {
    public:
        std::string name;
        std::vector<std::unique_ptr<Member>> members;

        ClassDeclStmt(std::string n, std::vector<std::unique_ptr<Member>> m) : name(n), members(std::move(m)) {}

        ~ClassDeclStmt() override = default;
    };

    class Literal : public Expr
    {
    public:
        Value value;
        Type type;

        Literal(Value v, Type t) : value(v), type(t) {}

        virtual ~Literal() = default;
    };

    class IntLiteral : public Literal
    {
    public:
        IntLiteral(int v) : Literal(Value(v), Type(TypeValue::INT, "int")) {}

        ~IntLiteral() override = default;
    };

    class FloatLiteral : public Literal
    {
    public:
        FloatLiteral(float v) : Literal(Value(v), Type(TypeValue::FLOAT, "float")) {}

        ~FloatLiteral() override = default;
    };

    class DoubleLiteral : public Literal
    {
    public:
        DoubleLiteral(double v) : Literal(Value(v), Type(TypeValue::DOUBLE, "double")) {}

        ~DoubleLiteral() override = default;
    };

    class CharLiteral : public Literal
    {
    public:
        CharLiteral(char v) : Literal(Value(v), Type(TypeValue::CHAR, "char")) {}
        
        ~CharLiteral() override = default;
    };

    class BoolLiteral : public Literal
    {
    public:
        BoolLiteral(bool v) : Literal(Value(v), Type(TypeValue::BOOL, "bool")) {}

        ~BoolLiteral() override = default;
    };

    class StringLiteral : public Literal
    {
    public:
        StringLiteral(std::string v) : Literal(Value(v), Type(TypeValue::STRING, "string")) {}

        ~StringLiteral() override = default;
    };

    class BinaryExpr : public Expr
    {
    public:
        TokenType op;
        ExprPtr left;
        ExprPtr right;

        BinaryExpr(TokenType op, ExprPtr l, ExprPtr r) : op(op), left(std::move(l)), right(std::move(r)) {}

        ~BinaryExpr() override = default;
    };

    class UnaryExpr : public Expr
    {
    public:
        TokenType op;
        ExprPtr expr;

        UnaryExpr(TokenType op, ExprPtr e) : op(op), expr(std::move(e)) {}

        ~UnaryExpr() override = default;
    };

    class VarExpr : public Expr
    {
    public:
        std::string name;

        VarExpr(std::string n) : name(n) {}

        ~VarExpr() override = default;
    };

    class FuncCallExpr : public Expr
    {
    public:
        std::string name;
        std::vector<ExprPtr> args;

        FuncCallExpr(std::string n, std::vector<ExprPtr> a) : name(n), args(std::move(a)) {}

        ~FuncCallExpr() override = default;
    };

    class NewExpr : public Expr
    {
    public:
        std::string name;
        std::vector<ExprPtr> args;

        NewExpr(std::string n, std::vector<ExprPtr> a) : name(n), args(std::move(a)) {}

        ~NewExpr() override = default;
    };

    class FieldAccessExpr : public Expr
    {
    public:
        ExprPtr object;
        std::string name;

        FieldAccessExpr(ExprPtr obj, std::string n) : object(std::move(obj)), name(n) {}

        ~FieldAccessExpr() override = default;
    };

    class MethodCallExpr : public Expr
    {
    public:
        ExprPtr object;
        std::string name;
        std::vector<ExprPtr> args;

        MethodCallExpr(ExprPtr obj, std::string n, std::vector<ExprPtr> a) : object(std::move(obj)), name(n), args(std::move(a)) {}

        ~MethodCallExpr() override = default;
    };

    class ThisExpr : public Expr
    {
    public:
        ThisExpr() = default;
        ~ThisExpr() override = default;
    };
}