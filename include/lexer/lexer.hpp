#pragma once
#include "token.hpp"
#include <map>
#include <vector>

class Lexer
{
private:
    std::string source;
    int pos;
    int line, column;
    std::map<std::string, TokenType> keywords;

    Token tokenizeIdentifierOrKeywordOrBoolLiteral();
    Token tokenizeStringLiteral();
    Token tokenizeNumberLiteral();
    Token tokenizeCharLiteral();
    Token tokenizeOperator();
    char processEscapeSequence();

    const char peek();
    const char peek(int);
    const char advance();

    void scipComments();
    void scipMultilineComments();
    std::string encodeUtf8(int unicode);

public:
    Lexer(std::string);

    std::vector<Token> tokenize();
};