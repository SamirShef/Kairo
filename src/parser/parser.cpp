#include "../../include/parser/parser.hpp"
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

std::vector<AST::StmtPtr> Parser::parse()
{
    std::vector<AST::StmtPtr> stmts;

    while (peek().type != TokenType::END_OF_FILE) stmts.push_back(parseStmt());

    return stmts;
}

AST::StmtPtr Parser::parseStmt()
{
    if (match(TokenType::VAR)) return parseVarDeclStmt();
    else if (match(TokenType::FUNC)) return parseFuncDeclStmt();
    else if (match(TokenType::IDENTIFIER))
    {
        if (peek().type == TokenType::LPAREN) return parseFuncCallStmt();
        return parseVarAsgnStmt(false);
    }
    else if (match(TokenType::RETURN)) return parseReturnStmt();
    else if (match(TokenType::IF)) return parseIfElseStmt();
    else if (match(TokenType::WHILE)) return parseWhileLoopStmt();
    else if (match(TokenType::DO)) return parseDoWhileLoopStmt();
    else if (match(TokenType::FOR)) return parseForLoopStmt();
    else if (match(TokenType::BREAK)) return parseBreakStmt();
    else if (match(TokenType::CONTINUE)) return parseContinueStmt();
    else if (match(TokenType::ECHO)) return parseEchoStmt();
    else if (match(TokenType::CLASS)) return parseClassDeclStmt();
    else throw std::runtime_error("Unexpected token: '" + peek().value + "'" + " -> " + std::to_string(peek().line) + ":" + std::to_string(peek().column));
}

AST::StmtPtr Parser::parseVarDeclStmt()
{
    Type type = consumeType();
    Token id = consume(TokenType::IDENTIFIER, "Expected identificator");

    consume(TokenType::ASSIGN, "Expected '='");

    AST::ExprPtr expr = parseExpr();
    consume(TokenType::SEMICOLON, "Expected ';'");

    return std::make_unique<AST::VarDeclStmt>(id.value, type, std::move(expr));
}

AST::StmtPtr Parser::parseFuncDeclStmt()
{
    Type retType = Type(TypeValue::VOID, "void");
    if (peek().type != TokenType::IDENTIFIER) retType = consumeType();
    
    Token id = consume(TokenType::IDENTIFIER, "Expected identificator");

    consume(TokenType::LPAREN, "Expected '('");

    AST::Arguments args;
    while (!match(TokenType::RPAREN))
    {
        Type aType = consumeType();
        Token aName = consume(TokenType::IDENTIFIER, "Expected identificator");
        args.push_back(AST::Argument(aType, aName.value));

        if (peek().type != TokenType::RPAREN) consume(TokenType::COMMA, "Expected ','");
    }
    
    consume(TokenType::LBRACE, "Expected '{'");
    AST::Block block;
    while (!match(TokenType::RBRACE)) block.push_back(parseStmt());
    
    return std::make_unique<AST::FuncDeclStmt>(id.value, retType, std::move(args), std::move(block));
}

AST::StmtPtr Parser::parseFuncCallStmt()
{
    Token id = peek(-1);
    consume(TokenType::LPAREN, "Expected '('");
    
    std::vector<AST::ExprPtr> args;
    while (!match(TokenType::RPAREN))
    {
        args.push_back(parseExpr());
        if (peek().type != TokenType::RPAREN) consume(TokenType::COMMA, "Expected ','");
    }

    consume(TokenType::SEMICOLON, "Expected ';'");

    return std::make_unique<AST::FuncCallStmt>(id.value, std::move(args));
}

AST::StmtPtr Parser::parseVarAsgnStmt(bool fromForLoop)
{
    Token id = peek(-1);
    AST::ExprPtr expr = nullptr;

    if (match(TokenType::ASSIGN)) expr = parseExpr();
    else expr = createCompoundAssignmentOperator(id);

    if (!fromForLoop) consume(TokenType::SEMICOLON, "Expected ';'");

    return std::make_unique<AST::VarAsgnStmt>(id.value, std::move(expr));
}

AST::StmtPtr Parser::parseReturnStmt()
{
    AST::ExprPtr expr = nullptr;
    if (peek().type != TokenType::SEMICOLON) expr = parseExpr();
    consume(TokenType::SEMICOLON, "Expected ';'");

    return std::make_unique<AST::ReturnStmt>(std::move(expr));
}

AST::StmtPtr Parser::parseIfElseStmt()
{
    consume(TokenType::LPAREN, "Expected '('");
    AST::ExprPtr condExpr = parseExpr();
    consume(TokenType::RPAREN, "Expected ')'");

    AST::Block thenBranch;
    if (!match(TokenType::LBRACE)) thenBranch.push_back(parseStmt());
    else while (!match(TokenType::RBRACE)) thenBranch.push_back(parseStmt());

    AST::Block elseBranch;
    if (match(TokenType::ELSE))
    {
        if (!match(TokenType::LBRACE)) elseBranch.push_back(parseStmt());
        else while (!match(TokenType::RBRACE)) elseBranch.push_back(parseStmt());
    }

    return std::make_unique<AST::IfElseStmt>(std::move(condExpr), std::move(thenBranch), std::move(elseBranch));
}

AST::StmtPtr Parser::parseWhileLoopStmt()
{
    consume(TokenType::LPAREN, "Expected '('");
    AST::ExprPtr condExpr = parseExpr();
    consume(TokenType::RPAREN, "Expected ')'");

    AST::Block block;
    if (!match(TokenType::LBRACE)) block.push_back(parseStmt());
    else while (!match(TokenType::RBRACE)) block.push_back(parseStmt());

    return std::make_unique<AST::WhileLoopStmt>(std::move(condExpr), std::move(block));
}

AST::StmtPtr Parser::parseDoWhileLoopStmt()
{
    AST::Block block;
    if (!match(TokenType::LBRACE)) block.push_back(parseStmt());
    else while (!match(TokenType::RBRACE)) block.push_back(parseStmt());

    consume(TokenType::NEXT, "Expected '->'");
    
    consume(TokenType::LPAREN, "Expected '('");
    AST::ExprPtr condExpr = parseExpr();
    consume(TokenType::RPAREN, "Expected ')'");

    consume(TokenType::SEMICOLON, "Expected ';'");

    return std::make_unique<AST::DoWhileLoopStmt>(std::move(condExpr), std::move(block));
}

AST::StmtPtr Parser::parseForLoopStmt()
{
    consume(TokenType::LPAREN, "Expected '('");

    AST::StmtPtr iterator;
    if (match(TokenType::IDENTIFIER)) iterator = parseVarAsgnStmt(false);
    else iterator = parseVarDeclStmt();

    AST::ExprPtr condExpr = parseExpr();

    consume(TokenType::SEMICOLON, "Expected ';'");

    pos++;  // skip identifier;
    AST::StmtPtr iterationStmt = parseVarAsgnStmt(true);

    consume(TokenType::RPAREN, "Expected ')'");

    AST::Block block;
    if (!match(TokenType::LBRACE)) block.push_back(parseStmt());
    else while (!match(TokenType::RBRACE)) block.push_back(parseStmt());

    return std::make_unique<AST::ForLoopStmt>(std::move(iterator), std::move(condExpr), std::move(iterationStmt), std::move(block));
}

AST::StmtPtr Parser::parseBreakStmt()
{
    consume(TokenType::SEMICOLON, "Expected ';'");

    return std::make_unique<AST::BreakStmt>();
}

AST::StmtPtr Parser::parseContinueStmt()
{
    consume(TokenType::SEMICOLON, "Expected ';'");

    return std::make_unique<AST::ContinueStmt>();
}

AST::StmtPtr Parser::parseClassDeclStmt()
{
    Token id = consume(TokenType::IDENTIFIER, "Expected identificator");

    consume(TokenType::LBRACE, "Expected '{'");

    std::vector<std::unique_ptr<AST::Member>> members;

    while (!match(TokenType::RBRACE))
    {
        AST::AccessModifier access = AST::AccessModifier::PRIVATE;
        if (match(TokenType::PUBLIC)) access = AST::AccessModifier::PUBLIC;
        else if (match(TokenType::PRIVATE)) {}

        if (match(TokenType::VAR)) members.push_back(parseFieldDecl(access));
        else if (match(TokenType::FUNC)) members.push_back(parseMethodDecl(access));
        else throw std::runtime_error("Unsupported class member declaration: '" + peek().value + "'");
    }

    return std::make_unique<AST::ClassDeclStmt>(id.value, std::move(members));
}

std::unique_ptr<AST::Member> Parser::parseFieldDecl(AST::AccessModifier& access)
{
    Type type = consumeType();
    Token id = consume(TokenType::IDENTIFIER, "Expected identificator");

    consume(TokenType::ASSIGN, "Expected '='");

    AST::ExprPtr expr = parseExpr();

    consume(TokenType::SEMICOLON, "Expected ';'");
    
    return std::make_unique<AST::FieldMember>(access, id.value, type, std::move(expr));
}

std::unique_ptr<AST::Member> Parser::parseMethodDecl(AST::AccessModifier& access)
{
    Type retType = Type(TypeValue::VOID, "void");
    if (peek().type != TokenType::IDENTIFIER) retType = consumeType();
    
    Token id = consume(TokenType::IDENTIFIER, "Expected identificator");

    consume(TokenType::LPAREN, "Expected '('");

    AST::Arguments args;
    while (!match(TokenType::RPAREN))
    {
        Type aType = consumeType();
        Token aName = consume(TokenType::IDENTIFIER, "Expected identificator");
        args.push_back(AST::Argument(aType, aName.value));

        if (peek().type != TokenType::RPAREN) consume(TokenType::COMMA, "Expected ','");
    }
    
    consume(TokenType::LBRACE, "Expected '{'");
    AST::Block block;
    while (!match(TokenType::RBRACE)) block.push_back(parseStmt());

    return std::make_unique<AST::MethodMember>(access, id.value, retType, std::move(args), std::move(block));
}

AST::StmtPtr Parser::parseEchoStmt()
{
    AST::ExprPtr expr = parseExpr();
    consume(TokenType::SEMICOLON, "Expected ';'");

    return std::make_unique<AST::EchoStmt>(std::move(expr));
}

Type Parser::consumeType()
{
    Token t = peek();
    switch (t.type)
    {
        case TokenType::INT: pos++; return Type(TypeValue::INT, "int");
        case TokenType::FLOAT: pos++; return Type(TypeValue::FLOAT, "float");
        case TokenType::DOUBLE: pos++; return Type(TypeValue::DOUBLE, "double");
        case TokenType::CHAR: pos++; return Type(TypeValue::CHAR, "char");
        case TokenType::BOOL: pos++; return Type(TypeValue::BOOL, "bool");
        case TokenType::VOID: pos++; return Type(TypeValue::VOID, "void");
        case TokenType::IDENTIFIER: pos++; return Type(TypeValue::CLASS, t.value);
        default: throw std::runtime_error("Unsupported type: '" + t.value + "'" + " -> " + std::to_string(t.line) + ":" + std::to_string(t.column));
    }
}

AST::ExprPtr Parser::createCompoundAssignmentOperator(Token& id)
{
    Token op = peek();
    pos++;
    switch (op.type)
    {
        case TokenType::PLUS_ASSIGN: return std::make_unique<AST::BinaryExpr>(TokenType::PLUS, std::make_unique<AST::VarExpr>(id.value), std::move(parseExpr()));
        case TokenType::MINUS_ASSIGN: return std::make_unique<AST::BinaryExpr>(TokenType::MINUS, std::make_unique<AST::VarExpr>(id.value), std::move(parseExpr()));
        case TokenType::MULTIPLY_ASSIGN: return std::make_unique<AST::BinaryExpr>(TokenType::MULTIPLY, std::make_unique<AST::VarExpr>(id.value), std::move(parseExpr()));
        case TokenType::DIVIDE_ASSIGN: return std::make_unique<AST::BinaryExpr>(TokenType::DIVIDE, std::make_unique<AST::VarExpr>(id.value), std::move(parseExpr()));
        case TokenType::MODULO_ASSIGN: return std::make_unique<AST::BinaryExpr>(TokenType::MODULO, std::make_unique<AST::VarExpr>(id.value), std::move(parseExpr()));
        default: throw  std::runtime_error("Token '" + op.value + "' does not a compound assignment operator");
    }
}

AST::ExprPtr Parser::parseExpr()
{
    return parseLogicalAnd();
}

AST::ExprPtr Parser::parseLogicalAnd()
{
    AST::ExprPtr expr = parseLogicalOr();

    while (match(TokenType::AND)) expr = std::make_unique<AST::BinaryExpr>(TokenType::AND, std::move(expr), std::move(parseLogicalOr()));

    return expr;
}

AST::ExprPtr Parser::parseLogicalOr()
{
    AST::ExprPtr expr = parseEquality();

    while (match(TokenType::OR)) expr = std::make_unique<AST::BinaryExpr>(TokenType::OR, std::move(expr), std::move(parseEquality()));

    return expr;
}

AST::ExprPtr Parser::parseEquality()
{
    AST::ExprPtr expr = parseComparation();

    while (true)
    {
        if (match(TokenType::EQUALS)) expr = std::make_unique<AST::BinaryExpr>(TokenType::EQUALS, std::move(expr), std::move(parseComparation()));
        else if (match(TokenType::NOT_EQUALS)) expr = std::make_unique<AST::BinaryExpr>(TokenType::NOT_EQUALS, std::move(expr), std::move(parseComparation()));
        else break;
    }

    return expr;
}

AST::ExprPtr Parser::parseComparation()
{
    AST::ExprPtr expr = parseAdditive();

    while (true)
    {
        if (match(TokenType::GREATER)) expr = std::make_unique<AST::BinaryExpr>(TokenType::GREATER, std::move(expr), std::move(parseAdditive()));
        else if (match(TokenType::GREATER_EQUALS)) expr = std::make_unique<AST::BinaryExpr>(TokenType::GREATER_EQUALS, std::move(expr),std::move(parseAdditive()));
        else if (match(TokenType::LESS)) expr = std::make_unique<AST::BinaryExpr>(TokenType::LESS, std::move(expr), std::move(parseAdditive()));
        else if (match(TokenType::LESS_EQUALS)) expr = std::make_unique<AST::BinaryExpr>(TokenType::LESS_EQUALS, std::move(expr), std::move(parseAdditive()));
        else break;
    }

    return expr;
}

AST::ExprPtr Parser::parseAdditive()
{
    AST::ExprPtr expr = parseMultiply();

    while (true)
    {
        if (match(TokenType::PLUS)) expr = std::make_unique<AST::BinaryExpr>(TokenType::PLUS, std::move(expr), std::move(parseMultiply()));
        else if (match(TokenType::MINUS)) expr = std::make_unique<AST::BinaryExpr>(TokenType::MINUS, std::move(expr), std::move(parseMultiply()));
        else break;
    }

    return expr;
}

AST::ExprPtr Parser::parseMultiply()
{
    AST::ExprPtr expr = parseUnary();

    while (true)
    {
        if (match(TokenType::MULTIPLY)) expr = std::make_unique<AST::BinaryExpr>(TokenType::MULTIPLY, std::move(expr),std::move(parseUnary()));
        else if (match(TokenType::DIVIDE)) expr = std::make_unique<AST::BinaryExpr>(TokenType::DIVIDE, std::move(expr),std::move(parseUnary()));
        else if (match(TokenType::MODULO)) expr = std::make_unique<AST::BinaryExpr>(TokenType::MODULO, std::move(expr), std::move(parseUnary()));
        else break;
    }

    return expr;
}

AST::ExprPtr Parser::parseUnary()
{
    while (true)
    {
        if (match(TokenType::NOT)) return std::make_unique<AST::UnaryExpr>(TokenType::NOT, std::move(parsePrimary()));
        else if (match(TokenType::MINUS)) return std::make_unique<AST::UnaryExpr>(TokenType::MINUS, std::move(parsePrimary()));
        else break;
    }

    return parsePrimary();
}

AST::ExprPtr Parser::parsePrimary()
{
    if (match(TokenType::NEW))
    {
        Token id = consume(TokenType::IDENTIFIER, "Expected identificator");
        consume(TokenType::LPAREN, "Expected '('");
        
        std::vector<AST::ExprPtr> args;
        while (!match(TokenType::RPAREN))
        {
            args.push_back(parseExpr());
            if (peek().type != TokenType::RPAREN) consume(TokenType::COMMA, "Expected ','");
        }

        return std::make_unique<AST::NewExpr>(id.value, std::move(args));
    }
    
    if (match(TokenType::LPAREN))
    {
        AST::ExprPtr expr = parseExpr();
        consume(TokenType::RPAREN, "Expected ')'");
        
        return expr;
    }

    Token c = peek();

    switch (c.type)
    {
        case TokenType::IDENTIFIER:
            pos++;
            if (match(TokenType::LPAREN))
            {
                std::vector<AST::ExprPtr> args;
                while (!match(TokenType::RPAREN))
                {
                    args.push_back(parseExpr());
                    if (peek().type != TokenType::RPAREN) consume(TokenType::COMMA, "Expected ','");
                }

                return std::make_unique<AST::FuncCallExpr>(c.value, std::move(args));
            }

            return std::make_unique<AST::VarExpr>(c.value);
        case TokenType::INT_LIT:
            pos++;
            return std::make_unique<AST::IntLiteral>(std::stoi(c.value));
        case TokenType::FLOAT_LIT:
            pos++;
            return std::make_unique<AST::FloatLiteral>(std::stof(c.value));
        case TokenType::DOUBLE_LIT:
            pos++;
            return std::make_unique<AST::DoubleLiteral>(std::stod(c.value));
        case TokenType::CHAR_LIT:
            pos++;
            return std::make_unique<AST::CharLiteral>(c.value[0]);
        case TokenType::BOOL_LIT:
            pos++;
            return std::make_unique<AST::BoolLiteral>(c.value == "false" ? false : true);
        case TokenType::STRING_LIT:
            pos++;
            return std::make_unique<AST::StringLiteral>(c.value);
        default:
            throw std::runtime_error("Unexpected token: '" + c.value + "'" + " -> " + std::to_string(c.line) + ":" + std::to_string(c.column));
    }
}

Token Parser::peek()
{
    return peek(0);
}

Token Parser::peek(int relativePos)
{
    if (pos + relativePos >= tokens.size()) throw std::runtime_error("Source length: " + std::to_string(tokens.size()) + ", got pos: " + std::to_string(pos) + ", rpos: " + std::to_string(relativePos));

    return tokens[pos + relativePos];
}

Token Parser::consume(TokenType type, const std::string& errMessage)
{
    if (match(type)) return peek(-1);

    throw std::runtime_error(errMessage + " -> " + std::to_string(peek().line) + ":" + std::to_string(peek().column));
}

bool Parser::match(TokenType type)
{
    if (peek().type == type)
    {
        pos++;
        
        return true;
    }

    return false;
}