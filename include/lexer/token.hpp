#pragma once
#include <string>

enum class TokenType
{
    IDENTIFIER,
    
    PLUS, MINUS, MULTIPLY, DIVIDE, MODULO, ASSIGN,
    PLUS_ASSIGN, MINUS_ASSIGN, MULTIPLY_ASSIGN, DIVIDE_ASSIGN, MODULO_ASSIGN,
    
    EQUALS, NOT_EQUALS, GREATER, GREATER_EQUALS, LESS, LESS_EQUALS, NOT, AND, OR,
    
    INT, FLOAT, DOUBLE, CHAR, BOOL, STRING, VOID,
    INT_LIT, FLOAT_LIT, DOUBLE_LIT, CHAR_LIT, BOOL_LIT, STRING_LIT,
    
    FUNC, VAR, RETURN, IF, ELSE, WHILE, DO, FOR, BREAK, CONTINUE, CLASS, PUBLIC, PRIVATE, NEW, THIS, CONSTRUCTOR, ECHO,
    
    LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET, COLON, SEMICOLON, DOT, COMMA, NEXT,

    END_OF_FILE
};

struct Token
{
    TokenType type;
    std::string value;
    int line;
    int column;

    Token(TokenType t, std::string v, const int l, const int c)
    {
        type = t;
        value = v;
        line = l;
        column = c;
    }

    std::string to_string()
    {
        return std::to_string((int)type) + " : '" + value + "' (" + std::to_string(line) + ":" + std::to_string(column) + ")";
    }
};