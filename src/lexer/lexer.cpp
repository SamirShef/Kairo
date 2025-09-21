#include "../../include/lexer/lexer.hpp"
#include <stdexcept>
#include <vector>

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
    keywords["string"] = TokenType::STRING;
    keywords["void"] = TokenType::VOID;
    keywords["var"] = TokenType::VAR;
    keywords["func"] = TokenType::FUNC;
    keywords["return"] = TokenType::RETURN;
    keywords["if"] = TokenType::IF;
    keywords["else"] = TokenType::ELSE;
    keywords["while"] = TokenType::WHILE;
    keywords["do"] = TokenType::DO;
    keywords["for"] = TokenType::FOR;
    keywords["break"] = TokenType::BREAK;
    keywords["continue"] = TokenType::CONTINUE;
    keywords["class"] = TokenType::CLASS;
    keywords["pub"] = TokenType::PUBLIC;
    keywords["priv"] = TokenType::PRIVATE;
    keywords["new"] = TokenType::NEW;
    keywords["this"] = TokenType::THIS;
    keywords["constructor"] = TokenType::CONSTRUCTOR;
    keywords["echo"] = TokenType::ECHO;
    keywords["trait"] = TokenType::TRAIT;
    keywords["impl"] = TokenType::IMPL;
    keywords["const"] = TokenType::CONST;
    keywords["include"] = TokenType::INCLUDE;
    keywords["sizeof"] = TokenType::SIZEOF;
}

std::vector<Token> Lexer::tokenize()
{
    std::vector<Token> tokens;

    while (pos < source.length())
    {
        const char c = peek();
        if (std::isspace(c) || c == '\n') advance();
        else if (peek() == '/' && peek(1) == '/')
        {
            if (peek(2) == '/') scipMultilineComments();
            else scipComments();
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
    while (pos < source.length() && peek() != '"') 
    {
        if (peek() == '\\')
        {
            advance();
            if (pos >= source.length()) throw std::runtime_error("Invalid escape sequence in string literal");
            
            char escapeChar = peek();
            if (escapeChar == 'u')
            {
                advance();
                if (pos + 4 > source.length()) throw std::runtime_error("Invalid Unicode escape sequence");
                
                std::string hexStr;
                for (int i = 0; i < 4; i++) hexStr += advance();
                
                try 
                {
                    int unicode = std::stoi(hexStr, nullptr, 16);
                    if (unicode > 0x10FFFF) throw std::runtime_error("Unicode escape sequence \\u" + hexStr + " is out of range (max U+10FFFF)");
                    
                    lit += encodeUtf8(unicode);
                }
                catch (const std::exception&)
                {
                    throw std::runtime_error("Invalid Unicode escape sequence: \\u" + hexStr);
                }
            }
            else if (escapeChar == 'U')
            {
                advance();
                if (pos + 8 > source.length()) throw std::runtime_error("Invalid Unicode escape sequence");
                
                std::string hexStr;
                for (int i = 0; i < 8; i++) hexStr += advance();
                
                try 
                {
                    int unicode = std::stoi(hexStr, nullptr, 16);
                    if (unicode > 0x10FFFF) throw std::runtime_error("Unicode escape sequence \\U" + hexStr + " is out of range (max U+10FFFF)");
                    
                    lit += encodeUtf8(unicode);
                }
                catch (const std::exception&)
                {
                    throw std::runtime_error("Invalid Unicode escape sequence: \\U" + hexStr);
                }
            }
            else lit += processEscapeSequence();
        }
        else lit += advance();
    }

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
    while (pos < source.length() && peek() != '\'') 
    {
        if (peek() == '\\')
        {
            advance();
            if (pos >= source.length()) throw std::runtime_error("Invalid escape sequence in character literal");
            lit += processEscapeSequence();
        }
        else lit += advance();
    }

    if (pos >= source.length()) throw std::runtime_error("Invalid character literal (the closing quotation mark is missing)");
    advance();

    if (lit.length() == 0) throw std::runtime_error("Invalid character literal (empty character constant)");
    if (lit.length() > 4) throw std::runtime_error("Invalid character literal (UTF-8 character too long)");

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
            else if (peek(1) == '+')
            {
                advance(); advance();
                return Token(TokenType::INCREMENT, "++", tmpLine, tmpColumn);
            }

            advance();
            return Token(TokenType::PLUS, "+", tmpLine, tmpColumn);
        case '-':
            if (peek(1) == '=')
            {
                advance(); advance();
                return Token(TokenType::MINUS_ASSIGN, "-=", tmpLine, tmpColumn);
            }
            else if (peek(1) == '>')
            {
                advance(); advance();
                return Token(TokenType::NEXT, "->", tmpLine, tmpColumn);
            }
            else if (peek(1) == '-')
            {
                advance(); advance();
                return Token(TokenType::DECREMENT, "--", tmpLine, tmpColumn);
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

char Lexer::processEscapeSequence()
{
    char c = advance();
    
    switch (c)
    {
        case 'n': return '\n';
        case 't': return '\t';
        case 'r': return '\r';
        case '\\': return '\\';
        case '\'': return '\'';
        case '"': return '"';
        case '0': return '\0';
        case 'a': return '\a';
        case 'b': return '\b';
        case 'f': return '\f';
        case 'v': return '\v';
        case 'x':
        {
            if (pos + 2 > source.length()) throw std::runtime_error("Invalid hexadecimal escape sequence");
            
            std::string hexStr;
            hexStr += advance();
            hexStr += advance();
            
            try 
            {
                return (char)(std::stoi(hexStr, nullptr, 16));
            }
            catch (const std::exception&)
            {
                throw std::runtime_error("Invalid hexadecimal escape sequence: \\x" + hexStr);
            }
        }
        case 'u':
        {
            if (pos + 4 > source.length()) throw std::runtime_error("Invalid Unicode escape sequence");
            
            std::string hexStr;
            for (int i = 0; i < 4; i++) hexStr += advance();
            
            try 
            {
                int unicode = std::stoi(hexStr, nullptr, 16);
                
                if (unicode > 0x10FFFF) throw std::runtime_error("Unicode escape sequence \\u" + hexStr + " is out of range (max U+10FFFF)");
                
                if (unicode <= 127) return (char)(unicode);
                
                return (char)(unicode & 0xFF);
            }
            catch (const std::exception&)
            {
                throw std::runtime_error("Invalid Unicode escape sequence: \\u" + hexStr);
            }
        }
        case 'U':
        {
            if (pos + 8 > source.length()) throw std::runtime_error("Invalid Unicode escape sequence");
            
            std::string hexStr;
            for (int i = 0; i < 8; i++) hexStr += advance();
            
            try
            {
                int unicode = std::stoi(hexStr, nullptr, 16);
                
                if (unicode > 0x10FFFF) throw std::runtime_error("Unicode escape sequence \\U" + hexStr + " is out of range (max U+10FFFF)");
                
                if (unicode <= 127) return (char)(unicode);
                
                return (char)(unicode & 0xFF);
            }
            catch (const std::exception&)
            {
                throw std::runtime_error("Invalid Unicode escape sequence: \\U" + hexStr);
            }
        }
        default: throw std::runtime_error("Invalid escape sequence: \\" + std::string(1, c));
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

std::string Lexer::encodeUtf8(int unicode)
{
    std::vector<char> utf8;
    
    if (unicode <= 0x7F) utf8.push_back((char)(unicode));
    else if (unicode <= 0x7FF)
    {
        utf8.push_back((char)(0xC0 | (unicode >> 6)));
        utf8.push_back((char)(0x80 | (unicode & 0x3F)));
    }
    else if (unicode <= 0xFFFF)
    {
        utf8.push_back((char)(0xE0 | (unicode >> 12)));
        utf8.push_back((char)(0x80 | ((unicode >> 6) & 0x3F)));
        utf8.push_back((char)(0x80 | (unicode & 0x3F)));
    }
    else if (unicode <= 0x10FFFF)
    {
        utf8.push_back((char)(0xF0 | (unicode >> 18)));
        utf8.push_back((char)(0x80 | ((unicode >> 12) & 0x3F)));
        utf8.push_back((char)(0x80 | ((unicode >> 6) & 0x3F)));
        utf8.push_back((char)(0x80 | (unicode & 0x3F)));
    }
    else throw std::runtime_error("Unicode code point out of range: " + std::to_string(unicode));
    
    return std::string(utf8.begin(), utf8.end());
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