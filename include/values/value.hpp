#pragma once
#include <memory>
#include <string>
#include <variant>

enum class TypeValue
{
    INT, FLOAT, DOUBLE, CHAR, BOOL, STRING, VOID, CLASS
};

struct Type
{
    TypeValue type;
    std::string name;

    Type() = default;
    Type(TypeValue t, std::string n) : type(t), name(n) {}

    bool operator ==(const Type& other) const
    {
        return type == other.type && name == other.name;
    }

    bool operator !=(const Type& other) const
    {
        return !(*this == other);
    }
};

namespace AST
{
    struct ClassInstance;
}

struct Value
{
    std::variant<int, float, double, char, bool, std::string, std::shared_ptr<AST::ClassInstance>> value;

    Value(int v) : value(v) {}
    Value(float v) : value(v) {}
    Value(double v) : value(v) {}
    Value(char v) : value(v) {}
    Value(bool v) : value(v) {}
    Value(std::string v) : value(v) {}
};