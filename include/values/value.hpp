#pragma once
#include <string>
#include <variant>

enum class TypeValue
{
    INT, FLOAT, DOUBLE, CHAR, BOOL, STRING, VOID
};

struct Value
{
    std::variant<int, float, double, char, bool, std::string> value;
    TypeValue type;

    Value(int v) : value(v) {}
    Value(float v) : value(v) {}
    Value(double v) : value(v) {}
    Value(char v) : value(v) {}
    Value(bool v) : value(v) {}
    Value(std::string v) : value(v) {}

    Value add(Value);
    Value subtract(Value);
    Value multiply(Value);
    Value divide(Value);
    Value modulo(Value);
    Value equals(Value);
    Value notEquals(Value);
    Value greater(Value);
    Value greaterEquals(Value);
    Value less(Value);
    Value lessEquals(Value);
    Value logicalAnd(Value);
    Value logicalOr(Value);
};