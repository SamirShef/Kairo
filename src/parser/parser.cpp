#include "../../include/parser/parser.hpp"
#include <stdexcept>

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
        else if (peek().type == TokenType::NEXT) return parseObjectChainStmt();
        else if (peek().type == TokenType::LBRACKET) return parseVarAsgnStmt(false);
        
        return parseVarAsgnStmt(false);
    }
    else if (match(TokenType::THIS)) return parseThisStmt();
    else if (match(TokenType::RETURN)) return parseReturnStmt();
    else if (match(TokenType::IF)) return parseIfElseStmt();
    else if (match(TokenType::WHILE)) return parseWhileLoopStmt();
    else if (match(TokenType::DO)) return parseDoWhileLoopStmt();
    else if (match(TokenType::FOR)) return parseForLoopStmt();
    else if (match(TokenType::BREAK)) return parseBreakStmt();
    else if (match(TokenType::CONTINUE)) return parseContinueStmt();
    else if (match(TokenType::ECHO)) return parseEchoStmt();
    else if (match(TokenType::CLASS)) return parseClassDeclStmt();
    else if (match(TokenType::TRAIT)) return parseTraitDeclStmt();
    else if (match(TokenType::IMPL)) return parseTraitImplStmt();
    else throw std::runtime_error("Unexpected token: '" + peek().value + "'" + " -> " + std::to_string(peek().line) + ":" + std::to_string(peek().column));
}

AST::StmtPtr Parser::parseVarDeclStmt()
{
    Type baseType = consumeType();
    
    if (peek().type == TokenType::LBRACKET)
    {
        auto [arrayType, size] = parseArrayType();
        Token id = consume(TokenType::IDENTIFIER, "Expected identificator");
        
        AST::ExprPtr initializer = nullptr;
        if (match(TokenType::ASSIGN))
        {
            if (peek().type == TokenType::LBRACKET) initializer = parseArrayLiteral();
            else initializer = parseExpr();
        }
        
        consume(TokenType::SEMICOLON, "Expected ';'");
        
        return std::make_unique<AST::ArrayDeclStmt>(id.value, baseType, std::move(size), std::move(initializer));
    }
    
    Token id = consume(TokenType::IDENTIFIER, "Expected identificator");

    AST::ExprPtr expr = nullptr;
    if (match(TokenType::ASSIGN)) expr = parseExpr();
    consume(TokenType::SEMICOLON, "Expected ';'");

    return std::make_unique<AST::VarDeclStmt>(id.value, baseType, std::move(expr));
}
 
std::pair<Type, AST::ExprPtr> Parser::parseArrayType()
{
    consume(TokenType::LBRACKET, "Expected '['");
    
    AST::ExprPtr size = parseExpr();
    
    consume(TokenType::RBRACKET, "Expected ']'");
    
    return { Type(), std::move(size) };
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

AST::StmtPtr Parser::parseThisStmt()
{
    if (peek().type != TokenType::NEXT) throw std::runtime_error("Unexpected token after 'this' in statement");

    AST::ExprPtr base = std::make_unique<AST::ThisExpr>(nullptr);
    AST::ExprPtr object = parseCallsChain(std::move(base));

    if (peek().type == TokenType::ASSIGN)
    {
        Token fieldName = peek(-1);
        
        consume(TokenType::ASSIGN, "Expected '='");
        
        AST::ExprPtr expr = parseExpr();
        
        consume(TokenType::SEMICOLON, "Expected ';'");
        
        AST::ExprPtr target = std::make_unique<AST::ThisExpr>(nullptr);

        return std::make_unique<AST::FieldAsgnStmt>(std::move(target), std::move(object), fieldName.value, std::move(expr));
    }

    if (auto call = dynamic_cast<AST::MethodCallExpr*>(object.get()))
    {
        AST::ExprPtr objExpr = std::move(call->object);
        std::string name = call->name;
        std::vector<AST::ExprPtr> args;
        for (auto& a : call->args) args.push_back(std::move(a));
        
        consume(TokenType::SEMICOLON, "Expected ';'");
        
        return std::make_unique<AST::MethodCallStmt>(std::move(objExpr), name, std::move(args));
    }

    throw std::runtime_error("Only field assignment or method call are allowed after 'this' in statement");
}

AST::StmtPtr Parser::parseObjectChainStmt()
{
    Token id = peek(-1);

    AST::ExprPtr target = std::make_unique<AST::VarExpr>(id.value);
    AST::ExprPtr object = parseCallsChain(std::make_unique<AST::VarExpr>(id.value));
    
    if (match(TokenType::ASSIGN))
    {
        Token fieldName = peek(-2);

        AST::ExprPtr expr = parseExpr();
        
        consume(TokenType::SEMICOLON, "Expected ';'");

        return std::make_unique<AST::FieldAsgnStmt>(std::move(target), std::move(object), fieldName.value, std::move(expr));
    }
    
    if (auto call = dynamic_cast<AST::MethodCallExpr*>(object.get()))
    {
        AST::ExprPtr objExpr = std::move(call->object);
        std::string name = call->name;
        std::vector<AST::ExprPtr> args;
        for (auto& a : call->args) args.push_back(std::move(a));
        
        consume(TokenType::SEMICOLON, "Expected ';'");
        
        return std::make_unique<AST::MethodCallStmt>(std::move(objExpr), name, std::move(args));
    }
    
    throw std::runtime_error("Object chain must be field assignment or method call statement");
}



AST::StmtPtr Parser::parseVarAsgnStmt(bool fromForLoop)
{
    Token id = peek(-1);
    
    if (match(TokenType::LBRACKET))
    {
        AST::ExprPtr index = parseExpr();
        consume(TokenType::RBRACKET, "Expected ']'");

        if (peek().type == TokenType::NEXT)
        {
            AST::ExprPtr arrayExpr = std::make_unique<AST::ArrayExpr>(id.value, std::move(index));
            AST::ExprPtr object = parseCallsChain(std::move(arrayExpr));
            
            if (auto call = dynamic_cast<AST::MethodCallExpr*>(object.get()))
            {
                AST::ExprPtr objExpr = std::move(call->object);
                std::string name = call->name;
                std::vector<AST::ExprPtr> args;
                for (auto& a : call->args) args.push_back(std::move(a));
                
                if (!fromForLoop) consume(TokenType::SEMICOLON, "Expected ';'");
                
                return std::make_unique<AST::MethodCallStmt>(std::move(objExpr), name, std::move(args));
            }
            
            throw std::runtime_error("Array element access must result in method call");
        }

        consume(TokenType::ASSIGN, "Expected '='");
        
        AST::ExprPtr expr = parseExpr();
        
        if (!fromForLoop) consume(TokenType::SEMICOLON, "Expected ';'");
        
        return std::make_unique<AST::ArrayAsgnStmt>(id.value, std::move(index), std::move(expr));
    }
    
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

    pos++;  // skip identifier
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

AST::StmtPtr Parser::parseEchoStmt()
{
    AST::ExprPtr expr = parseExpr();
    consume(TokenType::SEMICOLON, "Expected ';'");

    return std::make_unique<AST::EchoStmt>(std::move(expr));
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

        if (match(TokenType::VAR)) members.push_back(std::move(parseFieldDecl(access)));
        else if (match(TokenType::FUNC)) members.push_back(std::move(parseMethodDecl(access)));
        else if (match(TokenType::CONSTRUCTOR)) members.push_back(std::move(parseConstructorDecl(access)));
        else throw std::runtime_error("Unsupported class member declaration: '" + peek().value + "'");
    }

    return std::make_unique<AST::ClassDeclStmt>(id.value, std::move(members));
}

std::unique_ptr<AST::Member> Parser::parseFieldDecl(AST::AccessModifier& access)
{
    Type type = consumeType();
    Token id = consume(TokenType::IDENTIFIER, "Expected identificator");

    AST::ExprPtr expr = nullptr;
    if (match(TokenType::ASSIGN)) expr = parseExpr();

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

std::unique_ptr<AST::Member> Parser::parseConstructorDecl(AST::AccessModifier& access)
{
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

    return std::make_unique<AST::ConstructorMember>(access, std::move(args), std::move(block));
}

AST::StmtPtr Parser::parseTraitDeclStmt()
{
    Token id = consume(TokenType::IDENTIFIER, "Expected identificator");

    consume(TokenType::LBRACE, "Expected '{'");

    std::vector<std::unique_ptr<AST::Member>> methods;

    while (!match(TokenType::RBRACE))
    {
        AST::AccessModifier access = AST::AccessModifier::PUBLIC;
        if (match(TokenType::PUBLIC)) access = AST::AccessModifier::PUBLIC;
        else if (match(TokenType::PRIVATE)) access = AST::AccessModifier::PRIVATE;

        if (match(TokenType::FUNC)) methods.push_back(std::move(parseTraitMethodDecl(access)));
        else throw std::runtime_error("Unsupported trait member declaration: '" + peek().value + "'");
    }

    return std::make_unique<AST::TraitDeclStmt>(id.value, std::move(methods));
}

AST::StmtPtr Parser::parseTraitImplStmt()
{
    Token traitName = consume(TokenType::IDENTIFIER, "Expected trait name");
    consume(TokenType::FOR, "Expected 'for'");
    
    Type targetType = consumeType();

    consume(TokenType::LBRACE, "Expected '{'");

    std::vector<std::unique_ptr<AST::Member>> implementations;

    while (!match(TokenType::RBRACE))
    {
        AST::AccessModifier access = AST::AccessModifier::PUBLIC;
        if (match(TokenType::PUBLIC)) access = AST::AccessModifier::PUBLIC;
        else if (match(TokenType::PRIVATE)) access = AST::AccessModifier::PRIVATE;

        if (match(TokenType::FUNC)) implementations.push_back(std::move(parseMethodDecl(access)));
        else throw std::runtime_error("Unsupported trait implementation member: '" + peek().value + "'");
    }

    return std::make_unique<AST::TraitImplStmt>(traitName.value, targetType.name, std::move(implementations));
}

std::unique_ptr<AST::Member> Parser::parseTraitMethodDecl(AST::AccessModifier& access)
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
    
    consume(TokenType::SEMICOLON, "Expected ';'");

    return std::make_unique<AST::TraitMethodMember>(access, id.value, retType, std::move(args));
}

Type Parser::consumeType()
{
    Token t = peek();
    Type baseType;
    
    switch (t.type)
    {
        case TokenType::INT: pos++; baseType = Type(TypeValue::INT, "int"); break;
        case TokenType::FLOAT: pos++; baseType = Type(TypeValue::FLOAT, "float"); break;
        case TokenType::DOUBLE: pos++; baseType = Type(TypeValue::DOUBLE, "double"); break;
        case TokenType::CHAR: pos++; baseType = Type(TypeValue::CHAR, "char"); break;
        case TokenType::BOOL: pos++; baseType = Type(TypeValue::BOOL, "bool"); break;
        case TokenType::STRING: pos++; baseType = Type(TypeValue::STRING, "string"); break;
        case TokenType::VOID: pos++; baseType = Type(TypeValue::VOID, "void"); break;
        case TokenType::IDENTIFIER: pos++; baseType = Type(TypeValue::CLASS, t.value); break;
        default: throw std::runtime_error("Unsupported type: '" + t.value + "'" + " -> " + std::to_string(t.line) + ":" + std::to_string(t.column));
    }
    
    if (match(TokenType::LBRACKET))
    {
        consume(TokenType::RBRACKET, "Expected ']' after '['");

        return Type::createArrayType(std::make_shared<Type>(baseType));
    }
    
    return baseType;
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
    if (match(TokenType::NEW)) return parseNewExpr();

    if (match(TokenType::THIS))
    {
        AST::ExprPtr expr = parseThisExpr();
        if (peek().type == TokenType::NEXT) return parseCallsChain(std::move(expr));
        
        return expr;
    }
    
    if (match(TokenType::LPAREN))
    {
        AST::ExprPtr expr = parseExpr();
        consume(TokenType::RPAREN, "Expected ')'");
        
        return expr;
    }

    Token token = peek();

    switch (token.type)
    {
        case TokenType::IDENTIFIER:
        {
            pos++;
            if (match(TokenType::LPAREN))
            {
                std::vector<AST::ExprPtr> args;
                while (!match(TokenType::RPAREN))
                {
                    args.push_back(parseExpr());
                    if (peek().type != TokenType::RPAREN) consume(TokenType::COMMA, "Expected ','");
                }

                AST::ExprPtr expr = std::make_unique<AST::FuncCallExpr>(token.value, std::move(args));

                if (peek().type == TokenType::NEXT) return parseCallsChain(std::move(expr));

                return std::move(expr);
            }

            AST::ExprPtr expr = std::make_unique<AST::VarExpr>(token.value);
            
            if (match(TokenType::LBRACKET))
            {
                AST::ExprPtr index = parseExpr();

                consume(TokenType::RBRACKET, "Expected ']'");

                expr = std::make_unique<AST::ArrayExpr>(token.value, std::move(index));
                
                if (peek().type == TokenType::NEXT) return parseCallsChain(std::move(expr));
            }
            
            if (peek().type == TokenType::NEXT) return parseCallsChain(std::move(expr));

            return expr;
        }
        case TokenType::INT_LIT:
        {
            pos++;
            AST::ExprPtr expr = std::make_unique<AST::IntLiteral>(std::stoi(token.value));
            if (peek().type == TokenType::NEXT) return parseCallsChain(std::move(expr));

            return expr;
        }
        case TokenType::FLOAT_LIT:
        {
            pos++;
            AST::ExprPtr expr = std::make_unique<AST::FloatLiteral>(std::stof(token.value));
            if (peek().type == TokenType::NEXT) return parseCallsChain(std::move(expr));

            return expr;
        }
        case TokenType::DOUBLE_LIT:
        {
            pos++;
            AST::ExprPtr expr = std::make_unique<AST::DoubleLiteral>(std::stod(token.value));
            if (peek().type == TokenType::NEXT) return parseCallsChain(std::move(expr));

            return expr;
        }
        case TokenType::CHAR_LIT:
        {
            pos++;
            AST::ExprPtr expr = std::make_unique<AST::CharLiteral>(token.value[0]);
            if (peek().type == TokenType::NEXT) return parseCallsChain(std::move(expr));

            return expr;
        }
        case TokenType::BOOL_LIT:
        {
            pos++;
            AST::ExprPtr expr = std::make_unique<AST::BoolLiteral>(token.value == "false" ? false : true);
            if (peek().type == TokenType::NEXT) return parseCallsChain(std::move(expr));

            return expr;
        }
        case TokenType::STRING_LIT:
        {
            pos++;
            AST::ExprPtr expr = std::make_unique<AST::StringLiteral>(token.value);
            if (peek().type == TokenType::NEXT) return parseCallsChain(std::move(expr));

            return expr;
        }
        case TokenType::LBRACKET:
        {
            pos++;
            AST::ExprPtr expr = parseArrayLiteral();
            if (peek().type == TokenType::NEXT) return parseCallsChain(std::move(expr));
            
            return expr;
        }
        default:
            throw std::runtime_error("Unexpected token: '" + token.value + "'" + " -> " + std::to_string(token.line) + ":" + std::to_string(token.column));
    }
}

AST::ExprPtr Parser::parseNewExpr()
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

AST::ExprPtr Parser::parseArrayLiteral()
{
    std::vector<AST::ExprPtr> elements;
    Type elementType;
    bool firstElement = true;
    
    while (!match(TokenType::RBRACKET))
    {
        AST::ExprPtr element = parseExpr();
        
        if (firstElement)
        {
            if (auto intLit = dynamic_cast<AST::IntLiteral*>(element.get())) elementType = Type(TypeValue::INT, "int");
            else if (auto floatLit = dynamic_cast<AST::FloatLiteral*>(element.get())) elementType = Type(TypeValue::FLOAT, "float");
            else if (auto doubleLit = dynamic_cast<AST::DoubleLiteral*>(element.get())) elementType = Type(TypeValue::DOUBLE, "double");
            else if (auto charLit = dynamic_cast<AST::CharLiteral*>(element.get())) elementType = Type(TypeValue::CHAR, "char");
            else if (auto boolLit = dynamic_cast<AST::BoolLiteral*>(element.get())) elementType = Type(TypeValue::BOOL, "bool");
            else if (auto stringLit = dynamic_cast<AST::StringLiteral*>(element.get())) elementType = Type(TypeValue::STRING, "string");
            else if (auto newExpr = dynamic_cast<AST::NewExpr*>(element.get())) elementType = Type(TypeValue::CLASS, newExpr->name);
            else throw std::runtime_error("Unsupported element type in array literal");

            firstElement = false;
        }
        
        elements.push_back(std::move(element));
        
        if (peek().type != TokenType::RBRACKET) consume(TokenType::COMMA, "Expected ','");
    }
    
    return std::make_unique<AST::ArrayLiteral>(std::move(elements), elementType);
}

AST::ExprPtr Parser::parseCallsChain(AST::ExprPtr expr)
{
    while (match(TokenType::NEXT))
    {
        Token id = consume(TokenType::IDENTIFIER, "Expected identificator");

        if (match(TokenType::LPAREN)) expr = parseMethodCallExpr(std::move(expr), id.value);
        else expr = parseFieldAccessExpr(std::move(expr), id.value);
    }

    return std::move(expr);
}

AST::ExprPtr Parser::parseFieldAccessExpr(AST::ExprPtr obj, std::string& name)
{
    return std::make_unique<AST::FieldAccessExpr>(std::move(obj), name);
}

AST::ExprPtr Parser::parseMethodCallExpr(AST::ExprPtr obj, std::string& name)
{
    std::vector<AST::ExprPtr> args;
    while (!match(TokenType::RPAREN))
    {
        args.push_back(parseExpr());
        if (peek().type != TokenType::RPAREN) consume(TokenType::COMMA, "Expected ','");
    }
    
    return std::make_unique<AST::MethodCallExpr>(std::move(obj), name, std::move(args));
}

AST::ExprPtr Parser::parseThisExpr()
{
    return std::make_unique<AST::ThisExpr>(nullptr);
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