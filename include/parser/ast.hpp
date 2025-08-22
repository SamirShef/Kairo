#pragma once
#include "../values/value.hpp"
#include "../lexer/token.hpp"
#include <memory>
#include <string>
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
        TypeValue type;
        std::string name;

        Argument(TypeValue t, std::string n) : type(t), name(n) {}

        bool operator==(const Argument& other) const
        {
            return type == other.type && name == other.name;
        }
        
        bool operator!=(const Argument& other) const
        {
            return !(*this == other);
        }
    };

    using Arguments = std::vector<Argument>;
    using Block = std::vector<StmtPtr>;

    class Function
    {
    public:
        Arguments args;
        Block block;

        Function() = default;
        Function(Arguments a, Block b) : args(std::move(a)), block(std::move(b)) {}
        Function(const Function&) = delete;
        Function(Function&&) = default;
        ~Function() = default;
    };
    
    class VarDeclStmt : public Stmt
    {
    public:
        std::string name;
        TypeValue type;
        ExprPtr expr;

        VarDeclStmt(std::string n, TypeValue t, ExprPtr e) : name(n), type(t), expr(std::move(e)) {}

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
        TypeValue retType;
        Arguments args;
        Block block;

        FuncDeclStmt(std::string n, TypeValue rt, Arguments a, Block b) : name(n), retType(rt), args(std::move(a)), block(std::move(b)) {}

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

    class EchoStmt : public Stmt
    {
    public:
        ExprPtr expr;

        EchoStmt(ExprPtr e) : expr(std::move(e)) {}

        ~EchoStmt() override = default;
    };

    class Literal : public Expr
    {
    public:
        Value value;
        TypeValue type;

        Literal(Value v, TypeValue t) : value(v), type(t) {}

        virtual ~Literal() = default;
    };

    class IntLiteral : public Literal
    {
    public:
        IntLiteral(int v) : Literal(Value(v), TypeValue::INT) {}

        ~IntLiteral() override = default;
    };

    class FloatLiteral : public Literal
    {
    public:
        FloatLiteral(float v) : Literal(Value(v), TypeValue::FLOAT) {}

        ~FloatLiteral() override = default;
    };

    class DoubleLiteral : public Literal
    {
    public:
        DoubleLiteral(double v) : Literal(Value(v), TypeValue::DOUBLE) {}

        ~DoubleLiteral() override = default;
    };

    class CharLiteral : public Literal
    {
    public:
        CharLiteral(char v) : Literal(Value(v), TypeValue::CHAR) {}
        
        ~CharLiteral() override = default;
    };

    class BoolLiteral : public Literal
    {
    public:
        BoolLiteral(bool v) : Literal(Value(v), TypeValue::BOOL) {}

        ~BoolLiteral() override = default;
    };

    class StringLiteral : public Literal
    {
    public:
        StringLiteral(std::string v) : Literal(Value(v), TypeValue::STRING) {}

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
}