#include "../../include/lexer/lexer.hpp"
#include <cctype>
#include <stdexcept>
#include <vector>
#include <string>

Lexer::Lexer(const std::string initSource)
{
    source = initSource;
    pos = 0;
    line = column = 1;

    keywords["int"] = TokenType::INT;
    keywords["float"] = TokenType::FLOAT;
    keywords["double"] = TokenType::DOUBLE;
    keywords["char"] = TokenType::CHAR;
    keywords["bool"] = TokenType::BOOL;
    keywords["void"] = TokenType::VOID;
    keywords["var"] = TokenType::VAR;
    keywords["func"] = TokenType::FUNC;
    keywords["return"] = TokenType::RETURN;
    keywords["if"] = TokenType::IF;
    keywords["else"] = TokenType::ELSE;
    keywords["echo"] = TokenType::ECHO;
}

std::vector<Token> Lexer::tokenize()
{
    std::vector<Token> tokens;

    while (pos < source.length())
    {
        const char c = peek();
        if (std::isspace(c) || c == '\n') advance();
        else if (peek() == '/')
        {
            if (peek(1) == '/')
            {
                if (peek(2) == '/') scipMultilineComments();
                else scipComments();
            }
        }
        else if (std::isalpha(c) || c == '_') tokens.push_back(tokenizeIdentifierOrKeywordOrBoolLiteral());
        else if (std::isdigit(c)) tokens.push_back(tokenizeNumberLiteral());
        else if (c == '"') tokens.push_back(tokenizeStringLiteral());
        else if (c == '\'') tokens.push_back(tokenizeCharLiteral());
        else tokens.push_back(tokenizeOperator());
    }

    tokens.push_back(Token(TokenType::END_OF_FILE, "", line, column));

    return tokens;
}

Token Lexer::tokenizeIdentifierOrKeywordOrBoolLiteral()
{
    std::string id;

    int tmpLine = line;
    int tmpColumn = column;

    while (pos < source.length() && (std::isalpha(peek()) || std::isdigit(peek()) || peek() == '_')) id += advance();

    if (keywords.find(id) != keywords.end()) return Token(keywords[id], id, tmpLine, tmpColumn);
    else if (id == "false" || id == "true") return Token(TokenType::BOOL_LIT, id, tmpLine, tmpColumn);

    return Token(TokenType::IDENTIFIER, id, tmpLine, tmpColumn);
}

Token Lexer::tokenizeStringLiteral()
{
    std::string lit;

    int tmpLine = line;
    int tmpColumn = column;

    advance();
    while (pos < source.length() && peek() != '"') lit += advance();

    if (pos >= source.length()) throw std::runtime_error("Invalid string literal (the closing quotation mark is missing)");
    advance();

    return Token(TokenType::STRING_LIT, lit, tmpLine, tmpColumn);
}

Token Lexer::tokenizeNumberLiteral()
{
    std::string number;
    bool hasDot = false;
    char suffix = '\0';

    int tmpLine = line;
    int tmpColumn = column;

    while (std::isdigit(peek()) || peek() == '.')
    {
        if (peek() == '.' && number.find('.') != std::string::npos) throw std::runtime_error("Invalid number literal (twice dot)");
        else if (peek() == '.') hasDot = true;

        number += advance();
    }

    if (number[number.length() - 1] == '.') throw std::runtime_error("Invalid number literal (has dot, but hasn't fractional part)");

    if (hasDot && pos < source.length() && peek() != 'f' && peek() != 'd') throw std::runtime_error("Invalid number literal (floating number without suffix) -> " + std::to_string(tmpLine) + ":" + std::to_string(tmpColumn));

    if (peek() == 'f')
    {
        advance();
        return Token(TokenType::FLOAT_LIT, number, tmpLine, tmpColumn);
    }
    else if (peek() == 'd')
    {
        advance();
        return Token(TokenType::DOUBLE_LIT, number, tmpLine, tmpColumn);
    }

    return Token(TokenType::INT_LIT, number, tmpLine, tmpColumn);
}

Token Lexer::tokenizeCharLiteral()
{
    std::string lit;

    int tmpLine = line;
    int tmpColumn = column;

    advance();
    while (pos < source.length() && peek() != '\'') lit += advance();

    if (pos >= source.length()) throw std::runtime_error("Invalid character literal (the closing quotation mark is missing)");
    advance();

    if (lit.length() > 1) throw std::runtime_error("Invalid character literal (must have length is 1)");
    else if (lit.length() == 0) throw std::runtime_error("Invalid character literal (empty character constant)");

    return Token(TokenType::CHAR_LIT, lit, tmpLine, tmpColumn);
}

Token Lexer::tokenizeOperator()
{
    char c = peek();

    int tmpLine = line;
    int tmpColumn = column;

    switch (c)
    {
        case '+':
            if (peek(1) == '=')
            {
                advance(); advance();
                return Token(TokenType::PLUS_ASSIGN, "+=", tmpLine, tmpColumn);
            }

            advance();
            return Token(TokenType::PLUS, "+", tmpLine, tmpColumn);
        case '-':
            if (peek(1) == '=')
            {
                advance(); advance();
                return Token(TokenType::MINUS_ASSIGN, "-=", tmpLine, tmpColumn);
            }

            advance();
            return Token(TokenType::MINUS, "-", tmpLine, tmpColumn);
        case '*':
            if (peek(1) == '=')
            {
                advance(); advance();
                return Token(TokenType::MULTIPLY_ASSIGN, "*=", tmpLine, tmpColumn);
            }

            advance();
            return Token(TokenType::MULTIPLY, "*", tmpLine, tmpColumn);
        case '/':
            if (peek(1) == '=')
            {
                advance(); advance();
                return Token(TokenType::DIVIDE_ASSIGN, "/=", tmpLine, tmpColumn);
            }

            advance();
            return Token(TokenType::DIVIDE, "/", tmpLine, tmpColumn);
        case '%':
            if (peek(1) == '=')
            {
                advance(); advance();
                return Token(TokenType::MODULO_ASSIGN, "%=", tmpLine, tmpColumn);
            }
            advance();
            return Token(TokenType::MODULO, "%", tmpLine, tmpColumn);
        case '=':
            if (peek(1) == '=')
            {
                advance(); advance();
                return Token(TokenType::EQUALS, "==", tmpLine, tmpColumn);
            }

            advance();
            return Token(TokenType::ASSIGN, "=", tmpLine, tmpColumn);
        case '>':
            if (peek(1) == '=')
            {
                advance(); advance();
                return Token(TokenType::GREATER_EQUALS, ">=", tmpLine, tmpColumn);
            }

            advance();
            return Token(TokenType::GREATER, ">", tmpLine, tmpColumn);
        case '<':
            if (peek(1) == '=')
            {
                advance(); advance();
                return Token(TokenType::LESS_EQUALS, "<=", tmpLine, tmpColumn);
            }

            advance();
            return Token(TokenType::LESS, "<", tmpLine, tmpColumn);
        case '!':
            if (peek(1) == '=')
            {
                advance(); advance();
                return Token(TokenType::NOT_EQUALS, "!=", tmpLine, tmpColumn);
            }

            advance();
            return Token(TokenType::NOT, "!", tmpLine, tmpColumn);
        case '&':
            if (peek(1) == '&')
            {
                advance(); advance();
                return Token(TokenType::AND, "&&", tmpLine, tmpColumn);
            }

            throw std::runtime_error("Unsupported operator: '&' (maybe you mean '&&'?)");
        case '|':
            if (peek(1) == '|')
            {
                advance(); advance();
                return Token(TokenType::OR, "||", tmpLine, tmpColumn);
            }

            throw std::runtime_error("Unsupported operator: '|' (maybe you mean '||'?)");
        case ':':
            advance();
            return Token(TokenType::COLON, ":", tmpLine, tmpColumn);
        case ';':
            advance();
            return Token(TokenType::SEMICOLON, ";", tmpLine, tmpColumn);
        case '.':
            advance();
            return Token(TokenType::DOT, ".", tmpLine, tmpColumn);
        case ',':
            advance();
            return Token(TokenType::COMMA, ",", tmpLine, tmpColumn);
        case '(':
            advance();
            return Token(TokenType::LPAREN, "(", tmpLine, tmpColumn);
        case ')':
            advance();
            return Token(TokenType::RPAREN, ")", tmpLine, tmpColumn);
        case '{':
            advance();
            return Token(TokenType::LBRACE, "{", tmpLine, tmpColumn);
        case '}':
            advance();
            return Token(TokenType::RBRACE, "}", tmpLine, tmpColumn);
        case '[':
            advance();
            return Token(TokenType::LBRACKET, "[", tmpLine, tmpColumn);
        case ']':
            advance();
            return Token(TokenType::RBRACKET, "]", tmpLine, tmpColumn);
        default:
            throw std::runtime_error("Unsupported operator: '" + std::to_string(c) + "'");
    }
}

void Lexer::scipComments()
{
    advance();  // scip '/'
    advance();  // scip '/'

    while (pos < source.length() && peek() != '\n') advance();
}

void Lexer::scipMultilineComments()
{
    advance();  // scip '/'
    advance();  // scip '/'
    advance();  // scip '/'
    
    while (pos < source.length() && (peek() != '/' || peek(1) != '/' || peek(2) != '/')) advance();
    if (pos >= source.length()) throw std::runtime_error("Invalid multiline commen (missing closing part '///')");
    
    advance();  // scip '/'
    advance();  // scip '/'
    advance();  // scip '/'
}

const char Lexer::peek()
{
    return peek(0);
}

const char Lexer::peek(int relativePos)
{
    if (pos + relativePos >= source.length()) throw std::runtime_error("Source length: " + std::to_string(source.length()) + ", got pos: " + std::to_string(pos) + ", rpos: " + std::to_string(relativePos));

    return source[pos + relativePos];
}

const char Lexer::advance()
{
    if (peek() == '\n')
    {
        line++;
        column = 1;
    }
    else column++;
    pos++;

    return peek(-1);
}