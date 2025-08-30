#pragma once
#include <memory>
#include <string>
#include <variant>
#include <vector>

enum class TypeValue
{
    INT, FLOAT, DOUBLE, CHAR, BOOL, STRING, VOID, CLASS, ARRAY
};

struct Type
{
    TypeValue type;
    std::string name;
    std::shared_ptr<Type> elementType;
    size_t arraySize;

    Type() = default;
    Type(TypeValue t, std::string n) : type(t), name(n), elementType(nullptr), arraySize(0) {}
    Type(TypeValue t, std::string n, std::shared_ptr<Type> et, size_t size = 0) : type(t), name(n), elementType(et), arraySize(size) {}

    bool operator ==(const Type& other) const
    {
        if (type != other.type || name != other.name) return false;
        if (type == TypeValue::ARRAY)
        {
            if (!elementType && !other.elementType) return true;

            if (!elementType || !other.elementType) return false;

            return *elementType == *other.elementType && arraySize == other.arraySize;
        }

        return true;
    }

    bool operator !=(const Type& other) const
    {
        return !(*this == other);
    }

    static Type createArrayType(std::shared_ptr<Type> elementType, size_t size = 0)
    {
        std::string arrayName = elementType->name + "[]";
        
        return Type(TypeValue::ARRAY, arrayName, elementType, size);
    }
};

namespace AST
{
    struct ClassInstance;
}

struct Value
{
    std::variant<int, float, double, char, bool, std::string, std::shared_ptr<AST::ClassInstance>, std::vector<Value>> value;

    Value(int v) : value(v) {}
    Value(float v) : value(v) {}
    Value(double v) : value(v) {}
    Value(char v) : value(v) {}
    Value(bool v) : value(v) {}
    Value(std::string v) : value(v) {}
    Value(std::vector<Value> v) : value(std::move(v)) {}
};