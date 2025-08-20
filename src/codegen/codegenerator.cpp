#include "../../include/codegen/codegenerator.hpp"
#include <iostream>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <stdexcept>
#include <vector>

CodeGenerator::CodeGenerator(const std::string& moduleName) : context(), builder(context), module(std::make_unique<llvm::Module>(moduleName, context))
{
    scopeStack.emplace(std::map<std::string, llvm::Value*>{});
}

void CodeGenerator::pushScope()
{
    scopeStack.emplace(std::map<std::string, llvm::Value*>{});
}

void CodeGenerator::popScope()
{
    if (scopeStack.empty()) throw std::runtime_error("Scope underflow");
    scopeStack.pop();
}

void CodeGenerator::setNamedValue(const std::string& name, llvm::Value* value)
{
    if (scopeStack.empty()) throw std::runtime_error("No active scope");
    scopeStack.top()[name] = value;
}

llvm::Value* CodeGenerator::getNamedValue(const std::string& name) const
{
    auto copy = scopeStack;
    while (!copy.empty()) {
        const auto& scope = copy.top();
        auto it = scope.find(name);
        if (it != scope.end()) return it->second;
        copy.pop();
    }
    return nullptr;
}

llvm::Type* CodeGenerator::getLLVMType(TypeValue type)
{
    switch(type) {
        case TypeValue::INT: return llvm::Type::getInt32Ty(context);
        case TypeValue::FLOAT: return llvm::Type::getFloatTy(context);
        case TypeValue::DOUBLE: return llvm::Type::getDoubleTy(context);
        case TypeValue::CHAR: return llvm::Type::getInt8Ty(context);
        case TypeValue::BOOL: return llvm::Type::getInt1Ty(context);
        case TypeValue::VOID: return llvm::Type::getVoidTy(context);
        default: throw std::runtime_error("Unknown type in getLLVMType");
    }
}

llvm::Value* CodeGenerator::castToExpectedIfNeeded(llvm::Value* value, llvm::Type* expectedType)
{
    llvm::Type* srcType = value->getType();
    if (srcType == expectedType) return value;

    if (srcType->isIntegerTy() && expectedType->isIntegerTy())
    {
        unsigned srcBits = srcType->getIntegerBitWidth();
        unsigned dstBits = expectedType->getIntegerBitWidth();
        if (srcBits < dstBits) return builder.CreateSExt(value, expectedType, "sexttmp");
    }

    if (srcType->isFloatTy() && expectedType->isDoubleTy()) return builder.CreateFPExt(value, expectedType, "fpexttmp");

    if (srcType->isIntegerTy() && (expectedType->isFloatTy() || expectedType->isDoubleTy())) return builder.CreateSIToFP(value, expectedType, "sitofptmp");

    if ((srcType->isFloatTy() || srcType->isDoubleTy()) && expectedType->isIntegerTy()) return builder.CreateFPToSI(value, expectedType, "fptositmp");

    return value;
}

void CodeGenerator::generate(const std::vector<AST::StmtPtr>& stmts)
{
    for (const auto& stmt : stmts) generateStmt(*stmt);
}

void CodeGenerator::generateStmt(const AST::Stmt& stmt)
{
    if (auto vds = dynamic_cast<const AST::VarDeclStmt*>(&stmt)) generateVarDeclStmt(*vds);
    else if (auto vas = dynamic_cast<const AST::VarAsgnStmt*>(&stmt)) generateVarAsgnStmt(*vas);
    else if (auto fds = dynamic_cast<const AST::FuncDeclStmt*>(&stmt)) generateFuncDeclStmt(*fds);
    else if (auto fcs = dynamic_cast<const AST::FuncCallStmt*>(&stmt)) generateFuncCallStmt(*fcs);
    else if (auto rs = dynamic_cast<const AST::ReturnStmt*>(&stmt)) generateReturnStmt(*rs);
    else if (auto es = dynamic_cast<const AST::EchoStmt*>(&stmt)) generateEchoStmt(*es);
}

void CodeGenerator::generateVarDeclStmt(const AST::VarDeclStmt& stmt)
{
    llvm::Value* initValue = generateExpr(*stmt.expr);
    
    if (builder.GetInsertBlock() == nullptr)
    {
        // Попробуем сконвертировать вычисленное значение в константу, если это выражение из литералов
        llvm::Constant* constantInit = llvm::dyn_cast<llvm::Constant>(initValue);
        if (!constantInit)
        {
            // Для глобалов разрешены только константы; здесь семантика уже проверила константность.
            // Но некоторые значения могли вернуться как инструкции. Попробуем привести тип: если это ConstantExpr
            if (auto* ce = llvm::dyn_cast<llvm::ConstantExpr>(initValue)) constantInit = ce;
        }
        if (!constantInit) throw std::runtime_error("Global variable initializer must be a constant");

        llvm::GlobalVariable* gv = new llvm::GlobalVariable(*module, getLLVMType(stmt.type), false, llvm::GlobalValue::ExternalLinkage, constantInit, stmt.name);
        setNamedValue(stmt.name, gv);
        return;
    }

    llvm::AllocaInst* alloca = builder.CreateAlloca(getLLVMType(stmt.type), nullptr, stmt.name + ".addr");
    std::cout << (alloca == nullptr) << std::endl;

    builder.CreateStore(initValue, alloca);
    setNamedValue(stmt.name, alloca);
}

void CodeGenerator::generateVarAsgnStmt(const AST::VarAsgnStmt& vas)
{
    llvm::Value* address = getNamedValue(vas.name);
    if (address == nullptr) throw std::runtime_error("Variable '" + vas.name + "' does not exists");

    llvm::Type* valueType = nullptr;
    if (auto* allocaInst = llvm::dyn_cast<llvm::AllocaInst>(address)) valueType = allocaInst->getAllocatedType();
    else if (auto* globalVar = llvm::dyn_cast<llvm::GlobalVariable>(address)) valueType = globalVar->getValueType();
    else throw std::runtime_error("Unsupported address kind for variable: " + vas.name);

    llvm::Value* expr = generateExpr(*vas.expr);
    expr = castToExpectedIfNeeded(expr, valueType);

    builder.CreateStore(expr, address);
}

void CodeGenerator::generateFuncDeclStmt(const AST::FuncDeclStmt& stmt)
{
    std::vector<llvm::Type*> argTypes;
    for (const AST::Argument& arg : stmt.args) argTypes.push_back(getLLVMType(arg.type));
    
    llvm::FunctionType* funcType = llvm::FunctionType::get(getLLVMType(stmt.retType), argTypes, false);

    llvm::Function* func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, stmt.name, *module);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);
    
    pushScope();

    for (llvm::Argument& arg : func->args())
    {
        const std::string& argName = stmt.args[arg.getArgNo()].name;
        arg.setName(argName);
        
        llvm::AllocaInst* alloca = builder.CreateAlloca(arg.getType(), nullptr, argName + ".addr");
        
        builder.CreateStore(&arg, alloca);
        
        setNamedValue(argName, alloca);
    }

    for (const auto& stmt : stmt.block) generateStmt(*stmt);

    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
    if (currentBlock != nullptr && currentBlock->getTerminator() == nullptr)
    {
        if (funcType->getReturnType()->isVoidTy()) builder.CreateRetVoid();
        else builder.CreateRet(llvm::Constant::getNullValue(funcType->getReturnType()));
    }

    popScope();

    builder.ClearInsertionPoint();
}

void CodeGenerator::generateFuncCallStmt(const AST::FuncCallStmt& stmt)
{
    llvm::Function* callee = module->getFunction(stmt.name);
    if (callee == nullptr) throw std::runtime_error("Function '" + stmt.name + "' does not exists");

    std::vector<llvm::Value*> argValues;
    argValues.reserve(stmt.args.size());
    for (size_t i = 0; i < stmt.args.size(); ++i)
    {
        llvm::Value* val = generateExpr(*stmt.args[i]);
        llvm::Type* expected = callee->getFunctionType()->getParamType(i);
        argValues.push_back(castToExpectedIfNeeded(val, expected));
    }

    builder.CreateCall(callee, argValues, callee->getReturnType()->isVoidTy() ? "" : "calltmp");
}

void CodeGenerator::generateReturnStmt(const AST::ReturnStmt& stmt)
{
    if (!stmt.expr) builder.CreateRetVoid();
    else builder.CreateRet(generateExpr(*stmt.expr));
}

llvm::Value* CodeGenerator::generateExpr(const AST::Expr& expr)
{
    if (auto lit = dynamic_cast<const AST::Literal*>(&expr)) return generateLiteral(*lit);
    else if (auto binary = dynamic_cast<const AST::BinaryExpr*>(&expr)) return generateBinaryExpr(*binary);
    else if (auto unary = dynamic_cast<const AST::UnaryExpr*>(&expr)) return generateUnaryExpr(*unary);
    else if (auto var = dynamic_cast<const AST::VarExpr*>(&expr)) return generateVarExpr(*var);
    else if (auto func = dynamic_cast<const AST::FuncCallExpr*>(&expr)) return generateFuncCallExpr(*func);

    throw std::runtime_error("Unknown expression type in generateExpr");
}

llvm::Value* CodeGenerator::generateLiteral(const AST::Literal& literal)
{
    const auto& value = literal.value.value;
    
    switch(literal.type) {
        case TypeValue::INT: return llvm::ConstantInt::get(context, llvm::APInt(32, std::get<int>(value), true));
        case TypeValue::FLOAT: return llvm::ConstantFP::get(context, llvm::APFloat(std::get<float>(value)));
        case TypeValue::DOUBLE: return llvm::ConstantFP::get(context, llvm::APFloat(std::get<double>(value)));
        case TypeValue::CHAR: return llvm::ConstantInt::get(context, llvm::APInt(8, std::get<char>(value), false));
        case TypeValue::BOOL: return llvm::ConstantInt::get(context, llvm::APInt(1, std::get<bool>(value), false));
        case TypeValue::STRING:
        {
            const auto& str = std::get<std::string>(value);
            return builder.CreateGlobalStringPtr(str);
        }
        default: throw std::runtime_error("Unknown literal type in generateLiteral");
    }
}

llvm::Value* CodeGenerator::generateBinaryExpr(const AST::BinaryExpr& binaryExpr)
{
    llvm::Value* left = generateExpr(*binaryExpr.left);
    llvm::Value* right = generateExpr(*binaryExpr.right);
    
    switch(binaryExpr.op) {
        case TokenType::PLUS: 
            if (left->getType()->isIntegerTy()) return builder.CreateAdd(left, right, "addtmp");
            else return builder.CreateFAdd(left, right, "addtmp");
        case TokenType::MINUS: 
            if (left->getType()->isIntegerTy()) return builder.CreateSub(left, right, "subtmp");
            else return builder.CreateFSub(left, right, "subtmp");
        case TokenType::MULTIPLY:
            if (left->getType()->isIntegerTy()) return builder.CreateMul(left, right, "multmp");
            else return builder.CreateFMul(left, right, "multmp");
        case TokenType::DIVIDE:
            if (left->getType()->isIntegerTy()) return builder.CreateSDiv(left, right, "divtmp");
            else return builder.CreateFDiv(left, right, "divtmp");
        case TokenType::MODULO:
            return builder.CreateSRem(left, right, "modtmp");
        case TokenType::EQUALS:
            if (left->getType()->isIntegerTy()) return builder.CreateICmpEQ(left, right, "eqtmp");
            else return builder.CreateFCmpOEQ(left, right, "eqtmp");
        case TokenType::NOT_EQUALS:
            if (left->getType()->isIntegerTy()) return builder.CreateICmpNE(left, right, "netmp");
            else return builder.CreateFCmpONE(left, right, "netmp");
        case TokenType::GREATER:
            if (left->getType()->isIntegerTy()) return builder.CreateICmpSGT(left, right, "gttmp");
            else return builder.CreateFCmpOGT(left, right, "gttmp");
        case TokenType::GREATER_EQUALS:
            if (left->getType()->isIntegerTy()) return builder.CreateICmpSGE(left, right, "getmp");
            else return builder.CreateFCmpOGE(left, right, "getmp");
        case TokenType::LESS:
            if (left->getType()->isIntegerTy()) return builder.CreateICmpSLT(left, right, "lttmp");
            else return builder.CreateFCmpOLT(left, right, "lttmp");
        case TokenType::LESS_EQUALS:
            if (left->getType()->isIntegerTy()) return builder.CreateICmpSLE(left, right, "letmp");
            else return builder.CreateFCmpOLE(left, right, "letmp");
        case TokenType::AND: return builder.CreateAnd(left, right, "andtmp");
        case TokenType::OR: return builder.CreateOr(left, right, "ortmp");
        default: throw std::runtime_error("Unknown binary operator in generateBinaryExpr");
    }
}

llvm::Value* CodeGenerator::generateUnaryExpr(const AST::UnaryExpr& unaryExpr)
{
    llvm::Value* operand = generateExpr(*unaryExpr.expr);
    
    switch(unaryExpr.op) {
        case TokenType::MINUS:
            if (operand->getType()->isIntegerTy()) return builder.CreateNeg(operand, "negtmp");
            else return builder.CreateFNeg(operand, "negtmp");
        case TokenType::NOT: return builder.CreateNot(operand, "nottmp");
        default: throw std::runtime_error("Unknown unary operator in generateUnaryExpr");
    }
}

llvm::Value* CodeGenerator::generateVarExpr(const AST::VarExpr& varExpr)
{
    llvm::Value* address = getNamedValue(varExpr.name);
    if (address == nullptr) throw std::runtime_error("Variable '" + varExpr.name + "' does not exists");

    llvm::Type* valueType = nullptr;
    if (auto* allocaInst = llvm::dyn_cast<llvm::AllocaInst>(address)) valueType = allocaInst->getAllocatedType();
    else if (auto* globalVar = llvm::dyn_cast<llvm::GlobalVariable>(address)) valueType = globalVar->getValueType();
    else throw std::runtime_error("Unsupported address kind for variable: " + varExpr.name);

    return builder.CreateLoad(valueType, address, varExpr.name);
}

llvm::Value* CodeGenerator::generateFuncCallExpr(const AST::FuncCallExpr& funcExpr)
{
    llvm::Function* callee = module->getFunction(funcExpr.name);
    if (callee == nullptr) throw std::runtime_error("Function '" + funcExpr.name + "' does not exists");

    std::vector<llvm::Value*> argValues;
    argValues.reserve(funcExpr.args.size());
    for (size_t i = 0; i < funcExpr.args.size(); ++i)
    {
        llvm::Value* val = generateExpr(*funcExpr.args[i]);
        llvm::Type* expected = callee->getFunctionType()->getParamType(i);
        argValues.push_back(castToExpectedIfNeeded(val, expected));
    }

    llvm::Value* call = builder.CreateCall(callee, argValues, callee->getReturnType()->isVoidTy() ? "" : "calltmp");
    
    if (callee->getReturnType()->isVoidTy()) return nullptr;
    
    return call;
}

void CodeGenerator::generateEchoStmt(const AST::EchoStmt& echoStmt)
{
    llvm::Value* value = generateExpr(*echoStmt.expr);
    
    std::string formatStr;
    llvm::Value* promoted = value;

    if (value->getType()->isIntegerTy(32)) formatStr = "%d\n";
    else if (value->getType()->isFloatTy())
    {
        formatStr = "%f\n";
        promoted = builder.CreateFPExt(value, llvm::Type::getDoubleTy(context));
    }
    else if (value->getType()->isDoubleTy()) formatStr = "%lf\n";
    else if (value->getType()->isIntegerTy(8))
    {
        formatStr = "%c\n";
        promoted = builder.CreateSExt(value, llvm::Type::getInt32Ty(context));
    }
    else if (value->getType()->isIntegerTy(1))
    {
        formatStr = "%d\n";
        promoted = builder.CreateZExt(value, llvm::Type::getInt32Ty(context));
    }
    else if (value->getType()->isPointerTy()) formatStr = "%s\n";
    else formatStr = "%d\n";
    
    llvm::Value* format = builder.CreateGlobalStringPtr(formatStr, "printf_format");
    
    llvm::Function* printfFunc = module->getFunction("printf");
    if (!printfFunc)
    {
        std::vector<llvm::Type*> printfArgs = {llvm::Type::getInt8Ty(context)};
        llvm::FunctionType* printfType = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), printfArgs, true);
        printfFunc = llvm::Function::Create(printfType, llvm::Function::ExternalLinkage, "printf", *module);
    }
    
    std::vector<llvm::Value*> args = {format, promoted};
    builder.CreateCall(printfFunc, args);
}

void CodeGenerator::printIR() const
{
    module->print(llvm::errs(), nullptr);
}