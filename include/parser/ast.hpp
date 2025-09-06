#pragma once
#include "../values/value.hpp"
#include "../lexer/token.hpp"
#include <string>
#include <vector>

namespace AST
{
    class Node
    {
    public:
        virtual ~Node() = default;
        virtual Node* clone() const = 0;
    };

    class Stmt : public Node
    {
    public:
        virtual ~Stmt() = default;
        virtual Stmt* clone() const = 0;
    };
    class Expr : public Node
    {
    public:
        virtual ~Expr() = default;
        virtual Expr* clone() const = 0;
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
        virtual Member* clone() const = 0;
    };
    
    class FieldMember : public Member
    {
    public:
        Type type;
        ExprPtr expr;

        FieldMember(AccessModifier am, std::string n, Type t, ExprPtr e) : Member(am, n), type(t), expr(std::move(e)) {}
        
        ~FieldMember() override = default;
        FieldMember* clone() const override
        {
            return new FieldMember(access, name, type, expr ? std::unique_ptr<Expr>(expr->clone()) : nullptr);
        }
    };

    class MethodMember : public Member
    {
    public:
        Type retType;
        Arguments args;
        Block block;

        MethodMember(AccessModifier am, std::string n, Type rt, Arguments a, Block b) : Member(am, n), retType(rt), args(std::move(a)), block(std::move(b)) {}
        
        ~MethodMember() override = default;
        MethodMember* clone() const override
        {
            Block blockCopy;
            for (const auto& stmt : block)
                blockCopy.push_back(std::unique_ptr<Stmt>(stmt->clone()));
                
            return new MethodMember(access, name, retType, args, std::move(blockCopy));
        }
    };

    class ConstructorMember : public Member
    {
    public:
        Arguments args;
        Block block;

        ConstructorMember(AccessModifier am, Arguments a, Block b) : Member(am, "constructor"), args(std::move(a)), block(std::move(b)) {}

        ~ConstructorMember() override = default;
        ConstructorMember* clone() const override
        {
            Block blockCopy;
            for (const auto& stmt : block)
                blockCopy.push_back(std::unique_ptr<Stmt>(stmt->clone()));

            return new ConstructorMember(access, args, std::move(blockCopy));
        }
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
        VarDeclStmt* clone() const override
        {
            return new VarDeclStmt(name, type, expr ? std::unique_ptr<Expr>(expr->clone()) : nullptr);
        }
    };

    class ArrayDeclStmt : public Stmt
    {
    public:
        std::string name;
        Type elementType;
        ExprPtr size;
        ExprPtr initializer;

        ArrayDeclStmt(std::string n, Type et, ExprPtr s, ExprPtr init = nullptr) : name(n), elementType(et), size(std::move(s)), initializer(std::move(init)) {}

        ~ArrayDeclStmt() override = default;
        ArrayDeclStmt* clone() const override
        {
            return new ArrayDeclStmt(name, elementType, std::unique_ptr<Expr>(size->clone()), initializer ? std::unique_ptr<Expr>(initializer->clone()) : nullptr);
        }
    };

    class ArrayAsgnStmt : public Stmt
    {
    public:
        std::string name;
        ExprPtr index;
        ExprPtr expr;

        ArrayAsgnStmt(std::string n, ExprPtr idx, ExprPtr e) : name(n), index(std::move(idx)), expr(std::move(e)) {}

        ~ArrayAsgnStmt() override = default;
        ArrayAsgnStmt* clone() const override
        {
            return new ArrayAsgnStmt(name, std::unique_ptr<Expr>(index->clone()), std::unique_ptr<Expr>(expr->clone()));
        }
    };

    class VarAsgnStmt : public Stmt
    {
    public:
        std::string name;
        ExprPtr expr;

        VarAsgnStmt(std::string n, ExprPtr e) : name(n), expr(std::move(e)) {}

        ~VarAsgnStmt() override = default;
        VarAsgnStmt* clone() const override
        {
            return new VarAsgnStmt(name, expr ? std::unique_ptr<Expr>(expr->clone()) : nullptr);
        }
    };

    class FieldAsgnStmt : public Stmt
    {
    public:
        ExprPtr target;
        ExprPtr object;
        std::string name;
        ExprPtr expr;

        FieldAsgnStmt(ExprPtr t, ExprPtr o, std::string n, ExprPtr e) : target(std::move(t)), object(std::move(o)), name(n), expr(std::move(e)) {}

        ~FieldAsgnStmt() override = default;
        FieldAsgnStmt* clone() const override
        {
            return new FieldAsgnStmt(std::unique_ptr<Expr>(target->clone()), std::unique_ptr<Expr>(object->clone()), name, expr ? std::unique_ptr<Expr>(expr->clone()) : nullptr);
        }
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
        FuncDeclStmt* clone() const override
        {
            Block blockCopy;
            for (const auto& stmt : block)
                blockCopy.push_back(std::unique_ptr<Stmt>(stmt->clone()));
                
            return new FuncDeclStmt(name, retType, args, std::move(blockCopy));
        }
    };

    class FuncCallStmt : public Stmt
    {
    public:
        std::string name;
        std::vector<ExprPtr> args;

        FuncCallStmt(std::string n, std::vector<ExprPtr> a) : name(n), args(std::move(a)) {}

        ~FuncCallStmt() override = default;
        FuncCallStmt* clone() const override
        {
            std::vector<ExprPtr> argsCopy;
            for (const auto& arg : args)
                argsCopy.push_back(std::unique_ptr<Expr>(arg->clone()));
                
            return new FuncCallStmt(name, std::move(argsCopy));
        }
    };

    class MethodCallStmt : public Stmt
    {
    public:
        ExprPtr object;
        std::string name;
        std::vector<ExprPtr> args;

        MethodCallStmt(ExprPtr o, std::string n, std::vector<ExprPtr> a) : object(std::move(o)), name(n), args(std::move(a)) {}

        ~MethodCallStmt() override = default;
        MethodCallStmt* clone() const override
        {
            std::vector<ExprPtr> argsCopy;
            for (const auto& arg : args)
                argsCopy.push_back(std::unique_ptr<Expr>(arg->clone()));

            return new MethodCallStmt(std::unique_ptr<Expr>(object->clone()), name, std::move(argsCopy));
        }
    };

    class ReturnStmt : public Stmt
    {
public:
        ExprPtr expr;

        ReturnStmt(ExprPtr e) : expr(std::move(e)) {}

        ~ReturnStmt() override = default;
        ReturnStmt* clone() const override
        {
            return new ReturnStmt(expr ? std::unique_ptr<Expr>(expr->clone()) : nullptr);
        }
    };

    class IfElseStmt : public Stmt
    {
    public:
        ExprPtr condExpr;
        Block thenBranch;
        Block elseBranch;

        IfElseStmt(ExprPtr ce, Block tb, Block eb) : condExpr(std::move(ce)), thenBranch(std::move(tb)), elseBranch(std::move(eb)) {}

        ~IfElseStmt() override = default;
        IfElseStmt* clone() const override
        {
            Block thenCopy, elseCopy;
            for (const auto& stmt : thenBranch)
                thenCopy.push_back(std::unique_ptr<Stmt>(stmt->clone()));
                
            for (const auto& stmt : elseBranch)
                elseCopy.push_back(std::unique_ptr<Stmt>(stmt->clone()));
                
            return new IfElseStmt(std::unique_ptr<Expr>(condExpr->clone()), std::move(thenCopy), std::move(elseCopy));
        }
    };

    class WhileLoopStmt : public Stmt
    {
    public:
        ExprPtr condExpr;
        Block block;

        WhileLoopStmt(ExprPtr ce, Block b) : condExpr(std::move(ce)), block(std::move(b)) {}

        ~WhileLoopStmt() override = default;
        WhileLoopStmt* clone() const override
        {
            Block blockCopy;
            for (const auto& stmt : block)
                blockCopy.push_back(std::unique_ptr<Stmt>(stmt->clone()));

            return new WhileLoopStmt(std::unique_ptr<Expr>(condExpr->clone()), std::move(blockCopy));
        }
    };
    
    class DoWhileLoopStmt : public Stmt
    {
    public:
        ExprPtr condExpr;
        Block block;

        DoWhileLoopStmt(ExprPtr ce, Block b) : condExpr(std::move(ce)), block(std::move(b)) {}

        ~DoWhileLoopStmt() override = default;
        DoWhileLoopStmt* clone() const override
        {
            Block blockCopy;
            for (const auto& stmt : block)
                blockCopy.push_back(std::unique_ptr<Stmt>(stmt->clone()));

            return new DoWhileLoopStmt(std::unique_ptr<Expr>(condExpr->clone()), std::move(blockCopy));
        }
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
        ForLoopStmt* clone() const override
        {
            Block blockCopy;
            for (const auto& stmt : block)
                blockCopy.push_back(std::unique_ptr<Stmt>(stmt->clone()));
                
            return new ForLoopStmt(iterator ? std::unique_ptr<Stmt>(iterator->clone()) : nullptr, condExpr ? std::unique_ptr<Expr>(condExpr->clone()) : nullptr,
                iterationStmt ? std::unique_ptr<Stmt>(iterationStmt->clone()) : nullptr, std::move(blockCopy));
        }
    };

    class BreakStmt : public Stmt
    {
    public:
        ~BreakStmt() override = default;
        BreakStmt* clone() const override
        {
            return new BreakStmt();
        }
    };

    class ContinueStmt : public Stmt
    {
    public:
        ~ContinueStmt() override = default;
        ContinueStmt* clone() const override
        {
            return new ContinueStmt();
        }
    };

    class EchoStmt : public Stmt
    {
    public:
        ExprPtr expr;

        EchoStmt(ExprPtr e) : expr(std::move(e)) {}

        ~EchoStmt() override = default;
        EchoStmt* clone() const override
        {
            return new EchoStmt(std::unique_ptr<Expr>(expr->clone()));
        }
    };

    class ClassDeclStmt : public Stmt
    {
    public:
        std::string name;
        std::vector<std::unique_ptr<Member>> members;

        ClassDeclStmt(std::string n, std::vector<std::unique_ptr<Member>> m) : name(n), members(std::move(m)) {}

        ~ClassDeclStmt() override = default;
        ClassDeclStmt* clone() const override
        {
            std::vector<std::unique_ptr<Member>> membersCopy;
            for (const auto& member : members)
            {
                if (auto field = dynamic_cast<const FieldMember*>(member.get()))
                    membersCopy.push_back(std::make_unique<FieldMember>(field->access, field->name, field->type, field->expr ? std::unique_ptr<Expr>(field->expr->clone()) : nullptr));
                else if (auto method = dynamic_cast<const MethodMember*>(member.get()))
                {
                    Block blockCopy;
                    for (const auto& stmt : method->block)
                        blockCopy.push_back(std::unique_ptr<Stmt>(stmt->clone()));
                    
                    membersCopy.push_back(std::make_unique<MethodMember>(method->access, method->name, method->retType, method->args, std::move(blockCopy)));
                }
                else if (auto ctor = dynamic_cast<const ConstructorMember*>(member.get()))
                {
                    Block blockCopy;
                    for (const auto& stmt : ctor->block)
                        blockCopy.push_back(std::unique_ptr<Stmt>(stmt->clone()));

                    membersCopy.push_back(std::make_unique<ConstructorMember>(ctor->access, ctor->args, std::move(blockCopy)));
                }
            }
            return new ClassDeclStmt(name, std::move(membersCopy));
        }
    };

    class TraitMethodMember : public Member
    {
    public:
        Type retType;
        Arguments args;

        TraitMethodMember(AccessModifier am, std::string n, Type rt, Arguments a) : Member(am, n), retType(rt), args(std::move(a)) {}
        
        ~TraitMethodMember() override = default;
        TraitMethodMember* clone() const override
        {
            return new TraitMethodMember(access, name, retType, args);
        }
    };

    class TraitDeclStmt : public Stmt
    {
    public:
        std::string name;
        std::vector<std::unique_ptr<Member>> methods;

        TraitDeclStmt(std::string n, std::vector<std::unique_ptr<Member>> m) : name(n), methods(std::move(m)) {}

        ~TraitDeclStmt() override = default;
        TraitDeclStmt* clone() const override
        {
            std::vector<std::unique_ptr<Member>> methodsCopy;
            for (const auto& method : methods)
                if (auto traitMethod = dynamic_cast<const TraitMethodMember*>(method.get()))
                    methodsCopy.push_back(std::make_unique<TraitMethodMember>(traitMethod->access, traitMethod->name, traitMethod->retType, traitMethod->args));

            return new TraitDeclStmt(name, std::move(methodsCopy));
        }
    };

    class TraitImplStmt : public Stmt
    {
    public:
        std::string traitName;
        std::string className;
        std::vector<std::unique_ptr<Member>> implementations;

        TraitImplStmt(std::string tn, std::string cn, std::vector<std::unique_ptr<Member>> impl) : traitName(tn), className(cn), implementations(std::move(impl)) {}

        ~TraitImplStmt() override = default;
        TraitImplStmt* clone() const override
        {
            std::vector<std::unique_ptr<Member>> implCopy;
            for (const auto& impl : implementations)
            {
                if (auto method = dynamic_cast<const MethodMember*>(impl.get()))
                {
                    Block blockCopy;
                    for (const auto& stmt : method->block)
                        blockCopy.push_back(std::unique_ptr<Stmt>(stmt->clone()));
                    
                    implCopy.push_back(std::make_unique<MethodMember>(method->access, method->name, method->retType, method->args, std::move(blockCopy)));
                }
            }
            
            return new TraitImplStmt(traitName, className, std::move(implCopy));
        }
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
        IntLiteral* clone() const override
        {
            return new IntLiteral(std::get<int>(value.value));
        }
    };

    class FloatLiteral : public Literal
    {
    public:
        FloatLiteral(float v) : Literal(Value(v), Type(TypeValue::FLOAT, "float")) {}

        ~FloatLiteral() override = default;
        FloatLiteral* clone() const override
        {
            return new FloatLiteral(std::get<float>(value.value));
        }
    };

    class DoubleLiteral : public Literal
    {
    public:
        DoubleLiteral(double v) : Literal(Value(v), Type(TypeValue::DOUBLE, "double")) {}

        ~DoubleLiteral() override = default;
        DoubleLiteral* clone() const override
        {
            return new DoubleLiteral(std::get<double>(value.value));
        }
    };

    class CharLiteral : public Literal
    {
    public:
        CharLiteral(char v) : Literal(Value(v), Type(TypeValue::CHAR, "char")) {}
        
        ~CharLiteral() override = default;
        CharLiteral* clone() const override
        {
            return new CharLiteral(std::get<char>(value.value));
        }
    };

    class BoolLiteral : public Literal
    {
    public:
        BoolLiteral(bool v) : Literal(Value(v), Type(TypeValue::BOOL, "bool")) {}

        ~BoolLiteral() override = default;
        BoolLiteral* clone() const override
        {
            return new BoolLiteral(std::get<bool>(value.value));
        }
    };

    class StringLiteral : public Literal
    {
    public:
        StringLiteral(std::string v) : Literal(Value(v), Type(TypeValue::STRING, "string")) {}

        ~StringLiteral() override = default;
        StringLiteral* clone() const override
        {
            return new StringLiteral(std::get<std::string>(value.value));
        }
    };

    class ArrayLiteral : public Literal
    {
    public:
        std::vector<ExprPtr> elements;
        Type elementType;
        ExprPtr size; // Размер массива (может быть nullptr)

        ArrayLiteral(std::vector<ExprPtr> elems, Type et, ExprPtr s = nullptr) : Literal(Value(std::vector<Value>()), Type::createArrayType(std::make_shared<Type>(et))), elements(std::move(elems)), elementType(et), size(std::move(s)) {}

        ~ArrayLiteral() override = default;
        ArrayLiteral* clone() const override
        {
            std::vector<ExprPtr> elementsCopy;
            for (const auto& elem : elements)
                elementsCopy.push_back(std::unique_ptr<Expr>(elem->clone()));

            return new ArrayLiteral(std::move(elementsCopy), elementType, size ? std::unique_ptr<Expr>(size->clone()) : nullptr);
        }
    };

    class BinaryExpr : public Expr
    {
    public:
        TokenType op;
        ExprPtr left;
        ExprPtr right;

        BinaryExpr(TokenType op, ExprPtr l, ExprPtr r) : op(op), left(std::move(l)), right(std::move(r)) {}

        ~BinaryExpr() override = default;
        BinaryExpr* clone() const override
        {
            return new BinaryExpr(op, std::unique_ptr<Expr>(left->clone()), std::unique_ptr<Expr>(right->clone()));
        }
    };

    class UnaryExpr : public Expr
    {
    public:
        TokenType op;
        ExprPtr expr;

        UnaryExpr(TokenType op, ExprPtr e) : op(op), expr(std::move(e)) {}

        ~UnaryExpr() override = default;
        UnaryExpr* clone() const override
        {
            return new UnaryExpr(op, std::unique_ptr<Expr>(expr->clone()));
        }
    };

    class VarExpr : public Expr
    {
    public:
        std::string name;

        VarExpr(std::string n) : name(n) {}

        ~VarExpr() override = default;
        VarExpr* clone() const override
        {
            return new VarExpr(name);
        }
    };

    class ArrayExpr : public Expr
    {
    public:
        std::string name;
        ExprPtr index;

        ArrayExpr(std::string n, ExprPtr idx) : name(n), index(std::move(idx)) {}

        ~ArrayExpr() override = default;
        ArrayExpr* clone() const override
        {
            return new ArrayExpr(name, std::unique_ptr<Expr>(index->clone()));
        }
    };

    class FuncCallExpr : public Expr
    {
    public:
        std::string name;
        std::vector<ExprPtr> args;

        FuncCallExpr(std::string n, std::vector<ExprPtr> a) : name(n), args(std::move(a)) {}

        ~FuncCallExpr() override = default;
        FuncCallExpr* clone() const override
        {
            std::vector<ExprPtr> argsCopy;
            for (const auto& arg : args)
            {
                argsCopy.push_back(std::unique_ptr<Expr>(arg->clone()));
            }
            return new FuncCallExpr(name, std::move(argsCopy));
        }
    };

    class NewExpr : public Expr
    {
    public:
        std::string name;
        std::vector<ExprPtr> args;

        NewExpr(std::string n, std::vector<ExprPtr> a) : name(n), args(std::move(a)) {}

        ~NewExpr() override = default;
        NewExpr* clone() const override
        {
            std::vector<ExprPtr> argsCopy;
            for (const auto& arg : args)
            {
                argsCopy.push_back(std::unique_ptr<Expr>(arg->clone()));
            }
            return new NewExpr(name, std::move(argsCopy));
        }
    };

    class FieldAccessExpr : public Expr
    {
    public:
        ExprPtr object;
        std::string name;

        FieldAccessExpr(ExprPtr obj, std::string n) : object(std::move(obj)), name(n) {}

        ~FieldAccessExpr() override = default;
        FieldAccessExpr* clone() const override
        {
            return new FieldAccessExpr(std::unique_ptr<Expr>(object->clone()), name);
        }
    };

    class MethodCallExpr : public Expr
    {
    public:
        ExprPtr object;
        std::string name;
        std::vector<ExprPtr> args;

        MethodCallExpr(ExprPtr obj, std::string n, std::vector<ExprPtr> a) : object(std::move(obj)), name(n), args(std::move(a)) {}

        ~MethodCallExpr() override = default;
        MethodCallExpr* clone() const override
        {
            std::vector<ExprPtr> argsCopy;
            for (const auto& arg : args)
            {
                argsCopy.push_back(std::unique_ptr<Expr>(arg->clone()));
            }
            return new MethodCallExpr(std::unique_ptr<Expr>(object->clone()), name, std::move(argsCopy));
        }
    };

    class ThisExpr : public Expr
    {
    public:
        ExprPtr expr;
    
        ThisExpr(ExprPtr e) : expr(std::move(e)) {}
        
        ~ThisExpr() override = default;
        ThisExpr* clone() const override
        {
            return new ThisExpr(expr ? std::unique_ptr<Expr>(expr->clone()) : nullptr);
        }
    };
}