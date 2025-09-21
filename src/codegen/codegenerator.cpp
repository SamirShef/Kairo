#include "../../include/codegen/codegenerator.hpp"
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

std::string typeToString(Type type);

CodeGenerator::CodeGenerator(const std::string& moduleName) : context(), builder(context), module(std::make_unique<llvm::Module>(moduleName, context))
{
    scopeStack.emplace(std::map<std::string, llvm::Value*>{});
    typesScopeStack.emplace(std::map<std::string, Type>{});

    auto ptrTy = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
    auto sizeTy = llvm::Type::getInt64Ty(context);
    auto mallocTy = llvm::FunctionType::get(ptrTy, {sizeTy}, false);
    module->getOrInsertFunction("malloc", mallocTy);
    
    auto charToStrTy = llvm::FunctionType::get(ptrTy, {llvm::Type::getInt8Ty(context)}, false);
    charToStringFunc = llvm::Function::Create(charToStrTy, llvm::Function::ExternalLinkage, "char_to_string", *module);
    
    currentFunctionReturnType = Type(TypeValue::VOID, "void");
    
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", charToStringFunc);
    llvm::IRBuilder<> funcBuilder(entry);
    
    llvm::Argument* charArg = &charToStringFunc->arg_begin()[0];
    charArg->setName("ch");
    
    llvm::Value* mallocCall = funcBuilder.CreateCall(module->getFunction("malloc"), llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 2), "str_malloc");
    
    llvm::Value* strPtr = funcBuilder.CreateBitCast(mallocCall, ptrTy, "str_ptr");
    
    llvm::Value* charPtr = funcBuilder.CreateInBoundsGEP(llvm::Type::getInt8Ty(context), strPtr, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0), "char_ptr");
    funcBuilder.CreateStore(charArg, charPtr);
    
    llvm::Value* nullPtr = funcBuilder.CreateInBoundsGEP(llvm::Type::getInt8Ty(context), strPtr, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 1), "null_ptr");
    funcBuilder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), 0), nullPtr);
    
    funcBuilder.CreateRet(strPtr);
}

void CodeGenerator::pushScope()
{
    scopeStack.emplace(std::map<std::string, llvm::Value*>{});
    typesScopeStack.emplace(std::map<std::string, Type>{});
}

void CodeGenerator::popScope()
{
    if (scopeStack.empty()) throw std::runtime_error("Scope underflow");

    scopeStack.pop();

    if (typesScopeStack.empty()) throw std::runtime_error("Types scope underflow");

    typesScopeStack.pop();
}

void CodeGenerator::setNamedValue(const std::string& name, llvm::Value* value)
{
    if (scopeStack.empty()) throw std::runtime_error("No active scope");

    scopeStack.top()[name] = value;
}

llvm::Value* CodeGenerator::getNamedValue(const std::string& name) const
{
    auto copy = scopeStack;
    while (!copy.empty())
    {
        const auto& scope = copy.top();
        auto it = scope.find(name);
        if (it != scope.end()) return it->second;

        copy.pop();
    }

    return nullptr;
}

Type CodeGenerator::getNamedType(const std::string& name) const
{
    auto copy = typesScopeStack;
    while (!copy.empty())
    {
        const auto& scope = copy.top();
        auto it = scope.find(name);
        if (it != scope.end()) return it->second;

        copy.pop();
    }

    throw std::runtime_error("Variable type '" + name + "' not found");
}

Type CodeGenerator::getVariableType(const std::string& name) const
{
    auto copy = typesScopeStack;
    while (!copy.empty())
    {
        const auto& scope = copy.top();
        auto it = scope.find(name);
        if (it != scope.end()) return it->second;

        copy.pop();
    }

    auto globalIt = globalVariableTypes.find(name);
    if (globalIt != globalVariableTypes.end()) return globalIt->second;

    throw std::runtime_error("Variable type '" + name + "' not found");
}

std::string CodeGenerator::resolveClassName(const AST::Expr& expr)
{
    if (auto var = dynamic_cast<const AST::VarExpr*>(&expr))
    {
        auto copy = typesScopeStack;
        while (!copy.empty())
        {
            const auto& scope = copy.top();
            auto it = scope.find(var->name);
            if (it != scope.end())
            {
                if (it->second.type == TypeValue::CLASS) return it->second.name;

                return it->second.name;
            }

            copy.pop();
        }
        
        auto globalIt = globalVariableTypes.find(var->name);
        if (globalIt != globalVariableTypes.end())
        {
            if (globalIt->second.type == TypeValue::CLASS) return globalIt->second.name;
            return globalIt->second.name;
        }
    }
    else if (dynamic_cast<const AST::ThisExpr*>(&expr))
    {
        if (classesStack.empty()) throw std::runtime_error("'this' used outside of class method");

        return classesStack.top();
    }
    else if (auto field = dynamic_cast<const AST::FieldAccessExpr*>(&expr)) return resolveClassName(*field->object);
    else if (auto method = dynamic_cast<const AST::MethodCallExpr*>(&expr)) return resolveClassName(*method->object);
    else if (auto array = dynamic_cast<const AST::ArrayExpr*>(&expr))
    {
        auto copy = typesScopeStack;
        while (!copy.empty())
        {
            const auto& scope = copy.top();
            auto it = scope.find(array->name);
            if (it != scope.end())
            {
                if (it->second.type == TypeValue::ARRAY && it->second.elementType)
                {
                    if (it->second.elementType->type == TypeValue::CLASS) return it->second.elementType->name;
                    return it->second.elementType->name;
                }

                throw std::runtime_error("Variable '" + array->name + "' is not an array");
            }

            copy.pop();
        }
        
        auto globalIt = globalVariableTypes.find(array->name);
        if (globalIt != globalVariableTypes.end())
        {
            if (globalIt->second.type == TypeValue::ARRAY && globalIt->second.elementType)
            {
                if (globalIt->second.elementType->type == TypeValue::CLASS) return globalIt->second.elementType->name;
                return globalIt->second.elementType->name;
            }
            
            throw std::runtime_error("Global variable '" + array->name + "' is not an array");
        }
        
        throw std::runtime_error("Array variable '" + array->name + "' not found");
    }
    else if (auto literal = dynamic_cast<const AST::Literal*>(&expr)) return literal->type.name;
    else if (auto funcCall = dynamic_cast<const AST::FuncCallExpr*>(&expr))
    {
        auto it = functions.find(funcCall->name);
        if (it != functions.end())
            for (const auto& fn : it->second)
                if (fn.args.size() == funcCall->args.size())
                    return fn.returnType.name;
        
        if (!classesStack.empty())
        {
            std::string className = classesStack.top();
            auto classIt = classes.find(className);
            if (classIt != classes.end())
            {
                auto methIt = classIt->second.methods.find(funcCall->name);
                if (methIt != classIt->second.methods.end()) return className;
            }
        }
        
        throw std::runtime_error("Function not found: " + funcCall->name);
    }
    else if (auto binary = dynamic_cast<const AST::BinaryExpr*>(&expr))
    {
        std::string leftType = resolveClassName(*binary->left);
        std::string rightType = resolveClassName(*binary->right);
        
        if (leftType == "double" || rightType == "double") return "double";
        if (leftType == "float" || rightType == "float") return "float";
        if (leftType == "int" || rightType == "int") return "int";
        if (leftType == "char" || rightType == "char") return "char";
        if (leftType == "bool" || rightType == "bool") return "bool";
        if (leftType == "string" || rightType == "string") return "string";
        
        if (leftType == rightType) return leftType;
        
        return "int";
    }
    else if (auto unary = dynamic_cast<const AST::UnaryExpr*>(&expr)) return resolveClassName(*unary->expr);

    throw std::runtime_error("Unable to resolve class name for expression");
}

llvm::Type* CodeGenerator::getLLVMType(Type type)
{
    if (type.name.empty() && type.type != TypeValue::INT && type.type != TypeValue::FLOAT && type.type != TypeValue::DOUBLE && type.type != TypeValue::CHAR && type.type != TypeValue::BOOL && type.type != TypeValue::STRING && type.type != TypeValue::VOID)
        throw std::runtime_error("Type with empty name detected in getLLVMType");
    
    switch(type.type)
    {
        case TypeValue::INT: return llvm::Type::getInt32Ty(context);
        case TypeValue::FLOAT: return llvm::Type::getFloatTy(context);
        case TypeValue::DOUBLE: return llvm::Type::getDoubleTy(context);
        case TypeValue::CHAR: return llvm::Type::getInt8Ty(context);
        case TypeValue::BOOL: return llvm::Type::getInt1Ty(context);
        case TypeValue::STRING: return llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
        case TypeValue::VOID: return llvm::Type::getVoidTy(context);
        case TypeValue::ARRAY:
        {
            if (!type.elementType) throw std::runtime_error("Array type without element type");

            llvm::Type* elementLLVMType = getLLVMType(*type.elementType);
            llvm::ArrayType* arrayType = llvm::ArrayType::get(elementLLVMType, type.arraySize);

            return llvm::PointerType::get(arrayType, 0);
        }
        case TypeValue::CLASS:
        {
            auto it = classes.find(type.name);
            if (it == classes.end()) throw std::runtime_error("Class not found: " + type.name);
            if (!it->second.type) throw std::runtime_error("Class type is null for: " + type.name);

            return llvm::PointerType::get(it->second.type, 0);
        }
        case TypeValue::TRAIT: return llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
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
        if (srcBits > dstBits) return builder.CreateTrunc(value, expectedType, "trunctmp");
        
        return value;
    }

    if (srcType->isFloatTy() && expectedType->isDoubleTy()) return builder.CreateFPExt(value, expectedType, "fpexttmp");
    if (srcType->isDoubleTy() && expectedType->isFloatTy()) return builder.CreateFPTrunc(value, expectedType, "fptrunctmp");
    
    if (srcType->isIntegerTy() && (expectedType->isFloatTy() || expectedType->isDoubleTy())) return builder.CreateSIToFP(value, expectedType, "sitofptmp");
    
    if ((srcType->isFloatTy() || srcType->isDoubleTy()) && expectedType->isIntegerTy()) return builder.CreateFPToSI(value, expectedType, "fptositmp");
    
    if (srcType->isIntegerTy(8) && expectedType->isPointerTy())
    {
        llvm::Constant* charConst = llvm::dyn_cast<llvm::Constant>(value);
        if (charConst)
        {
            llvm::ConstantInt* charInt = llvm::cast<llvm::ConstantInt>(charConst);
            char charValue = static_cast<char>(charInt->getZExtValue());
            
            std::string charStr(1, charValue);
            
            llvm::GlobalVariable* strGV = new llvm::GlobalVariable(*module, llvm::ConstantDataArray::getString(context, charStr)->getType(), true, llvm::GlobalValue::PrivateLinkage, llvm::ConstantDataArray::getString(context, charStr), "char_str_" + std::to_string(static_cast<int>(charValue)));
            
            llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
            return builder.CreateInBoundsGEP(strGV->getValueType(), strGV, {zero, zero}, "char_str_ptr");
        }
        else return builder.CreateCall(charToStringFunc, {value}, "char_to_str");
    }
    
    if (srcType->isIntegerTy(32) && expectedType->isPointerTy())
    {
        llvm::Value* charValue = builder.CreateTrunc(value, llvm::Type::getInt8Ty(context), "int_to_char");
        return builder.CreateCall(charToStringFunc, {charValue}, "char_to_str");
    }
    
    return value;
}

void CodeGenerator::generate(const std::vector<AST::StmtPtr>& stmts)
{
    for (const auto& stmt : stmts)
        if (auto fds = dynamic_cast<const AST::FuncDeclStmt*>(stmt.get())) declareFunctionPrototype(*fds);

    for (const auto& stmt : stmts) generateStmt(*stmt);
}

void CodeGenerator::generateStmt(const AST::Stmt& stmt)
{
    if (auto vds = dynamic_cast<const AST::VarDeclStmt*>(&stmt)) generateVarDeclStmt(*vds);
    else if (auto aas = dynamic_cast<const AST::ArrayAsgnStmt*>(&stmt)) generateArrayAsgnStmt(*aas);
    else if (auto vas = dynamic_cast<const AST::VarAsgnStmt*>(&stmt)) generateVarAsgnStmt(*vas);
    else if (auto fas = dynamic_cast<const AST::FieldAsgnStmt*>(&stmt)) generateFieldAsgnStmt(*fas);
    else if (auto fds = dynamic_cast<const AST::FuncDeclStmt*>(&stmt)) generateFuncDeclStmt(*fds);
    else if (auto fcs = dynamic_cast<const AST::FuncCallStmt*>(&stmt)) generateFuncCallStmt(*fcs);
    else if (auto mcs = dynamic_cast<const AST::MethodCallStmt*>(&stmt)) generateMethodCallStmt(*mcs);
    else if (auto rs = dynamic_cast<const AST::ReturnStmt*>(&stmt)) generateReturnStmt(*rs);
    else if (auto ies = dynamic_cast<const AST::IfElseStmt*>(&stmt)) generateIfElseStmt(*ies);
    else if (auto wls = dynamic_cast<const AST::WhileLoopStmt*>(&stmt)) generateWhileLoopStmt(*wls);
    else if (auto dwls = dynamic_cast<const AST::DoWhileLoopStmt*>(&stmt)) generateDoWhileLoopStmt(*dwls);
    else if (auto fls = dynamic_cast<const AST::ForLoopStmt*>(&stmt)) generateForLoopStmt(*fls);
    else if (dynamic_cast<const AST::BreakStmt*>(&stmt)) generateBreakStmt();
    else if (dynamic_cast<const AST::ContinueStmt*>(&stmt)) generateContinueStmt();
    else if (auto es = dynamic_cast<const AST::EchoStmt*>(&stmt)) generateEchoStmt(*es);
    else if (auto cds = dynamic_cast<const AST::ClassDeclStmt*>(&stmt)) generateClassDeclStmt(*cds);
    else if (auto tds = dynamic_cast<const AST::TraitDeclStmt*>(&stmt)) generateTraitDeclStmt(*tds);
    else if (auto tis = dynamic_cast<const AST::TraitImplStmt*>(&stmt)) generateTraitImplStmt(*tis);
}

void CodeGenerator::generateVarDeclStmt(const AST::VarDeclStmt& stmt)
{
    llvm::Value* initValue = nullptr;
    if (stmt.expr) initValue = generateExpr(*stmt.expr);
    else
    {
        llvm::Type* ty = getLLVMType(stmt.type);
        if (stmt.type.type == TypeValue::STRING) 
        {
            llvm::GlobalVariable* emptyStrGV = new llvm::GlobalVariable(*module, llvm::ConstantDataArray::getString(context, "")->getType(), true, llvm::GlobalValue::PrivateLinkage, llvm::ConstantDataArray::getString(context, ""), "empty_str");
            llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
            initValue = builder.CreateInBoundsGEP(emptyStrGV->getValueType(), emptyStrGV, {zero, zero}, "empty_str_ptr");
        }
        else initValue = llvm::Constant::getNullValue(ty);
    }
    
    if (builder.GetInsertBlock() == nullptr)
    {
        llvm::Type* destTy = getLLVMType(stmt.type);
        
        if (stmt.type.type == TypeValue::ARRAY)
        {
            llvm::Constant* constantInit = llvm::dyn_cast<llvm::Constant>(initValue);
            if (!constantInit) if (auto* ce = llvm::dyn_cast<llvm::ConstantExpr>(initValue)) constantInit = ce;
            if (!constantInit) throw std::runtime_error("Global array initializer must be a constant");

            llvm::GlobalVariable* arrayGV = new llvm::GlobalVariable(*module, constantInit->getType(), stmt.isConst, llvm::GlobalValue::InternalLinkage, constantInit, stmt.name + "_array");
            
            llvm::Constant* arrayPtr = llvm::ConstantExpr::getBitCast(arrayGV, destTy);
            
            llvm::GlobalVariable* gv = new llvm::GlobalVariable(*module, destTy, stmt.isConst, llvm::GlobalValue::ExternalLinkage, arrayPtr, stmt.name);
            setNamedValue(stmt.name, gv);
            globalVariableTypes[stmt.name] = stmt.type;
            
            if (auto* arrayType = llvm::dyn_cast<llvm::ArrayType>(constantInit->getType())) arrayTypes[stmt.name] = arrayType;
        }
        else
        {
            llvm::Constant* constantInit = llvm::dyn_cast<llvm::Constant>(initValue);
            if (!constantInit) if (auto* ce = llvm::dyn_cast<llvm::ConstantExpr>(initValue)) constantInit = ce;
            if (!constantInit) throw std::runtime_error("Global variable initializer must be a constant");

            if (constantInit->getType() != destTy)
            {
                if (auto* ci = llvm::dyn_cast<llvm::ConstantInt>(constantInit))
                {
                    long long ival = ci->getSExtValue();
                    if (destTy->isIntegerTy()) constantInit = llvm::ConstantInt::get(destTy, static_cast<uint64_t>(ival), true);
                    else if (destTy->isFloatTy() || destTy->isDoubleTy()) constantInit = llvm::ConstantFP::get(destTy, static_cast<double>(ival));
                }
                else if (auto* cfp = llvm::dyn_cast<llvm::ConstantFP>(constantInit))
                {
                    double dval = cfp->getValueAPF().convertToDouble();
                    if (destTy->isFloatTy() || destTy->isDoubleTy()) constantInit = llvm::ConstantFP::get(destTy, dval);
                    else if (destTy->isIntegerTy()) constantInit = llvm::ConstantInt::get(destTy, static_cast<long long>(dval), true);
                }
            }

            llvm::GlobalVariable* gv = new llvm::GlobalVariable(*module, destTy, stmt.isConst, llvm::GlobalValue::ExternalLinkage, constantInit, stmt.name);
            setNamedValue(stmt.name, gv);
            globalVariableTypes[stmt.name] = stmt.type;
        }

        return;
    }

    llvm::Type* destTy = getLLVMType(stmt.type);
    llvm::AllocaInst* alloca = builder.CreateAlloca(destTy, nullptr, stmt.name + ".addr");
    initValue = castToExpectedIfNeeded(initValue, destTy);
    
    builder.CreateStore(initValue, alloca);
    setNamedValue(stmt.name, alloca);
    typesScopeStack.top()[stmt.name] = stmt.type;
    
    if (stmt.type.type == TypeValue::ARRAY)
    {
        auto it = arrayTypes.find(stmt.name);
        if (it == arrayTypes.end() || it->second->getNumElements() == 0)
        {
            llvm::Type* elementType = getLLVMType(*stmt.type.elementType);
            llvm::ArrayType* arrayType = llvm::ArrayType::get(elementType, stmt.type.arraySize);
            arrayTypes[stmt.name] = arrayType;
        }
        
        auto literalIt = arrayTypes.find("array_literal");
        if (literalIt != arrayTypes.end() && literalIt->second->getNumElements() > 0) arrayTypes[stmt.name] = literalIt->second;
    }
}

void CodeGenerator::generateVarAsgnStmt(const AST::VarAsgnStmt& vas)
{
    llvm::Value* address = getNamedValue(vas.name);
    if (address == nullptr) 
    {
        if (!classesStack.empty())
        {
            std::string className = classesStack.top();
            auto classIt = classes.find(className);
            if (classIt != classes.end())
            {
                auto fieldIt = classIt->second.fieldIndices.find(vas.name);
                if (fieldIt != classIt->second.fieldIndices.end())
                {
                    llvm::Value* thisPtr = getNamedValue("this");
                    if (!thisPtr) throw std::runtime_error("'this' not available in method");
                    
                    int fieldIndex = fieldIt->second;
                    llvm::Value* gep = builder.CreateStructGEP(classIt->second.type, thisPtr, fieldIndex, vas.name + ".addr");
                    
                    llvm::Value* value = generateExpr(*vas.expr);
                    llvm::Type* fieldTy = llvm::cast<llvm::StructType>(classIt->second.type)->getElementType(fieldIndex);
                    value = castToExpectedIfNeeded(value, fieldTy);
                    
                    builder.CreateStore(value, gep);

                    return;
                }
            }
        }
        
        throw std::runtime_error("Variable '" + vas.name + "' does not exists");
    }

    llvm::Type* valueType = nullptr;
    if (auto* allocaInst = llvm::dyn_cast<llvm::AllocaInst>(address)) valueType = allocaInst->getAllocatedType();
    else if (auto* globalVar = llvm::dyn_cast<llvm::GlobalVariable>(address)) valueType = globalVar->getValueType();
    else throw std::runtime_error("Unsupported address kind for variable: " + vas.name);

    llvm::Value* expr = generateExpr(*vas.expr);
    expr = castToExpectedIfNeeded(expr, valueType);

    builder.CreateStore(expr, address);
}

void CodeGenerator::generateFieldAsgnStmt(const AST::FieldAsgnStmt& stmt)
{
    llvm::Value* baseObjectPtr = nullptr;
    if (auto field = dynamic_cast<const AST::FieldAccessExpr*>(stmt.object.get())) baseObjectPtr = generateExpr(*field->object);
    else baseObjectPtr = generateExpr(*stmt.object);

    std::string className = resolveClassName(*stmt.object);
    
    auto classIt = classes.find(className);
    if (classIt == classes.end()) throw std::runtime_error("Class not found: " + className);
    
    auto fieldIt = classIt->second.fieldIndices.find(stmt.name);
    if (fieldIt == classIt->second.fieldIndices.end()) throw std::runtime_error("Field not found: " + stmt.name);
    
    int fieldIndex = fieldIt->second;
    llvm::Value* gep = builder.CreateStructGEP(classIt->second.type, baseObjectPtr, fieldIndex, stmt.name + ".addr");
    
    llvm::Value* value = generateExpr(*stmt.expr);
    llvm::Type* fieldTy = llvm::cast<llvm::StructType>(classIt->second.type)->getElementType(fieldIndex);
    value = castToExpectedIfNeeded(value, fieldTy);

    builder.CreateStore(value, gep);
}

void CodeGenerator::generateFuncDeclStmt(const AST::FuncDeclStmt& stmt)
{
    llvm::Function* func = declareFunctionPrototype(stmt);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);
    
    currentFunctionReturnType = stmt.retType;
    
    pushScope();

    size_t index = 0;
    for (auto& arg : func->args())
    {
        arg.setName(stmt.args[index].name);
        llvm::AllocaInst* alloca = builder.CreateAlloca(arg.getType(), nullptr, stmt.args[index].name + ".addr");
        builder.CreateStore(&arg, alloca);
        setNamedValue(stmt.args[index].name, alloca);
        typesScopeStack.top()[stmt.args[index].name] = stmt.args[index].type;
        index++;
    }

    for (const auto& s : stmt.block) generateStmt(*s);

    if (builder.GetInsertBlock()->getTerminator() == nullptr)
    {
        llvm::Type* retTy = stmt.retType.type == TypeValue::VOID ? llvm::Type::getVoidTy(context) : getLLVMType(stmt.retType);
        if (retTy->isVoidTy()) builder.CreateRetVoid();
        else builder.CreateRet(llvm::UndefValue::get(retTy));
    }

    popScope();
}

void CodeGenerator::generateFuncCallStmt(const AST::FuncCallStmt& stmt)
{
    std::vector<llvm::Value*> argValues;
    argValues.reserve(stmt.args.size());
    for (const auto& arg : stmt.args) argValues.push_back(generateExpr(*arg));

    auto it = functions.find(stmt.name);
    if (it == functions.end())
    {
        if (!classesStack.empty())
        {
            std::string className = classesStack.top();
            auto classIt = classes.find(className);
            if (classIt != classes.end())
            {
                auto methIt = classIt->second.methods.find(stmt.name);
                if (methIt != classIt->second.methods.end())
                {
                    for (llvm::Function* method : methIt->second)
                    {
                        auto* fnty = method->getFunctionType();
                        if (fnty->getNumParams() != argValues.size() + 1) continue;

                        std::vector<llvm::Value*> args;
                        args.reserve(argValues.size() + 1);
                        llvm::Value* thisPtr = getNamedValue("this");
                        if (!thisPtr) throw std::runtime_error("'this' not available in method");
                        args.push_back(thisPtr);

                        bool ok = true;
                        for (size_t i = 0; i < argValues.size(); i++)
                        {
                            llvm::Type* expected = fnty->getParamType(static_cast<unsigned>(i + 1));
                            llvm::Value* value = argValues[i];
                            if (!expected->isPointerTy()) value = castToExpectedIfNeeded(value, expected);
                            
                            if (value->getType() != expected) { ok = false; break; }

                            args.push_back(value);
                        }

                        if (!ok) continue;

                        builder.CreateCall(method, args);
                        
                        return;
                    }
                }
            }
        }

        throw std::runtime_error("Function not declared: " + stmt.name);
    }

    const auto& overloads = it->second;
    
    int bestScore = -1;
    const FunctionInfo* bestOverload = nullptr;
    std::vector<llvm::Value*> bestCasted;
    
    for (const auto& fn : overloads)
    {
        if (fn.args.size() != argValues.size()) continue;

        std::vector<llvm::Value*> casted;
        casted.reserve(argValues.size());
        bool ok = true;
        int score = 0;
        
        for (size_t i = 0; i < argValues.size(); i++)
        {
            llvm::Type* expected = getLLVMType(fn.args[i].type);
            llvm::Value* value = argValues[i];

            if (value->getType() == expected) 
            { 
                casted.push_back(value); 
                continue;
            }
            if (expected->isPointerTy() && value->getType()->isPointerTy()) { ok = false; break; }

            if (value->getType()->isIntegerTy() && expected->isIntegerTy()) score++;
            else if (value->getType()->isFloatTy() && expected->isFloatTy()) score++;
            else if (value->getType()->isDoubleTy() && expected->isDoubleTy()) score++;
            else if (value->getType()->isIntegerTy() && (expected->isFloatTy() || expected->isDoubleTy())) score += 2;
            else if (value->getType()->isFloatTy() && expected->isDoubleTy()) score++;
            else if (value->getType()->isDoubleTy() && expected->isFloatTy()) score += 2;
            else { ok = false; break; }

            value = castToExpectedIfNeeded(value, expected);

            if (value->getType() != expected) { ok = false; break; }

            casted.push_back(value);
        }

        if (!ok) continue;
        
        if (bestScore == -1 || score < bestScore)
        {
            bestScore = score;
            bestOverload = &fn;
            bestCasted = std::move(casted);
        }
    }
    
    if (bestOverload)
    {
        llvm::Function* callee = module->getFunction(bestOverload->mangledName);
        if (!callee) throw std::runtime_error("Function not declared: " + bestOverload->mangledName);
        builder.CreateCall(callee, bestCasted);

        return;
    }

    throw std::runtime_error("No matching overload found for function: " + stmt.name);
}

void CodeGenerator::generateMethodCallStmt(const AST::MethodCallStmt& stmt)
{
    AST::MethodCallExpr expr(std::unique_ptr<AST::Expr>(stmt.object->clone()), stmt.name, {});
    expr.args.reserve(stmt.args.size());
    for (const auto& a : stmt.args) expr.args.push_back(std::unique_ptr<AST::Expr>(a->clone()));
    
    generateMethodCallExpr(expr);
}

void CodeGenerator::generateReturnStmt(const AST::ReturnStmt& stmt)
{
    if (!stmt.expr) builder.CreateRetVoid();
    else 
    {
        llvm::Value* returnValue = generateExpr(*stmt.expr);
        
        if (currentFunctionReturnType.type != TypeValue::VOID)
        {
            llvm::Type* expectedType = getLLVMType(currentFunctionReturnType);
            returnValue = castToExpectedIfNeeded(returnValue, expectedType);
        }
        
        builder.CreateRet(returnValue);
    }
}

void CodeGenerator::generateIfElseStmt(const AST::IfElseStmt& stmt)
{
    llvm::Value* cond = generateExpr(*stmt.condExpr);
    
    if (!cond->getType()->isIntegerTy(1))
    {
        if (cond->getType()->isIntegerTy()) cond = builder.CreateICmpNE(cond, llvm::ConstantInt::get(cond->getType(), 0), "ifcond");
        else if (cond->getType()->isFloatTy() || cond->getType()->isDoubleTy()) cond = builder.CreateFCmpONE(cond, llvm::ConstantFP::get(cond->getType(), 0.0), "ifcond");
        else throw std::runtime_error("Invalid condition type in if statement");
    }
    
    llvm::Function* function = builder.GetInsertBlock()->getParent();
    
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context, "then", function);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(context, "else", function);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context, "ifcont", function);
    
    builder.CreateCondBr(cond, thenBB, elseBB);
    
    builder.SetInsertPoint(thenBB);
    pushScope();
    
    for (const auto& stmt : stmt.thenBranch) generateStmt(*stmt);
    
    if (builder.GetInsertBlock()->getTerminator() == nullptr) builder.CreateBr(mergeBB);
    
    popScope();
    
    builder.SetInsertPoint(elseBB);
    pushScope();
    
    for (const auto& stmt : stmt.elseBranch) generateStmt(*stmt);
    
    if (builder.GetInsertBlock()->getTerminator() == nullptr) builder.CreateBr(mergeBB);
    
    popScope();
    
    builder.SetInsertPoint(mergeBB);
}

void CodeGenerator::generateWhileLoopStmt(const AST::WhileLoopStmt& stmt)
{
    llvm::Function* function = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* condBlock = llvm::BasicBlock::Create(context, "while.cond", function);
    llvm::BasicBlock* bodyBlock = llvm::BasicBlock::Create(context, "while.body", function);
    llvm::BasicBlock* exitBlock = llvm::BasicBlock::Create(context, "while.end", function);

    builder.CreateBr(condBlock);

    builder.SetInsertPoint(condBlock);
    llvm::Value* cond = generateExpr(*stmt.condExpr);
    if (!cond->getType()->isIntegerTy(1))
    {
        if (cond->getType()->isIntegerTy()) cond = builder.CreateICmpNE(cond, llvm::ConstantInt::get(cond->getType(), 0), "whilecond");
        else if (cond->getType()->isFloatTy() || cond->getType()->isDoubleTy()) cond = builder.CreateFCmpONE(cond, llvm::ConstantFP::get(cond->getType(), 0.0), "whilecond");
        else throw std::runtime_error("Invalid condition type in while loop");
    }
    builder.CreateCondBr(cond, bodyBlock, exitBlock);

    builder.SetInsertPoint(bodyBlock);
    loopBlocks.emplace(exitBlock, condBlock);
    pushScope();

    for (const auto& s : stmt.block) generateStmt(*s);
    if (builder.GetInsertBlock()->getTerminator() == nullptr) builder.CreateBr(condBlock);

    popScope();
    loopBlocks.pop();

    builder.SetInsertPoint(exitBlock);
}

void CodeGenerator::generateDoWhileLoopStmt(const AST::DoWhileLoopStmt& stmt)
{
    llvm::Function* function = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* bodyBlock = llvm::BasicBlock::Create(context, "dowhile.body", function);
    llvm::BasicBlock* condBlock = llvm::BasicBlock::Create(context, "dowhile.cond", function);
    llvm::BasicBlock* exitBlock = llvm::BasicBlock::Create(context, "dowhile.end", function);

    builder.CreateBr(bodyBlock);

    builder.SetInsertPoint(bodyBlock);
    loopBlocks.emplace(exitBlock, condBlock);
    pushScope();

    for (const auto& s : stmt.block) generateStmt(*s);
    if (builder.GetInsertBlock()->getTerminator() == nullptr) builder.CreateBr(condBlock);

    popScope();
    loopBlocks.pop();

    builder.SetInsertPoint(condBlock);
    llvm::Value* cond = generateExpr(*stmt.condExpr);
    if (!cond->getType()->isIntegerTy(1))
    {
        if (cond->getType()->isIntegerTy()) cond = builder.CreateICmpNE(cond, llvm::ConstantInt::get(cond->getType(), 0), "dowhilecond");
        else if (cond->getType()->isFloatTy() || cond->getType()->isDoubleTy()) cond = builder.CreateFCmpONE(cond, llvm::ConstantFP::get(cond->getType(), 0.0), "dowhilecond");
        else throw std::runtime_error("Invalid condition type in do-while loop");
    }
    builder.CreateCondBr(cond, bodyBlock, exitBlock);

    builder.SetInsertPoint(exitBlock);
}

void CodeGenerator::generateForLoopStmt(const AST::ForLoopStmt& stmt)
{
    llvm::Function* function = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* iteratorBlock = llvm::BasicBlock::Create(context, "for.init", function);
    llvm::BasicBlock* condBlock = llvm::BasicBlock::Create(context, "for.cond", function);
    llvm::BasicBlock* iterationBlock = llvm::BasicBlock::Create(context, "for.incr", function);
    llvm::BasicBlock* bodyBlock = llvm::BasicBlock::Create(context, "for.body", function);
    llvm::BasicBlock* exitBlock = llvm::BasicBlock::Create(context, "for.end", function);

    builder.CreateBr(iteratorBlock);

    builder.SetInsertPoint(iteratorBlock);
    if (stmt.iterator) generateStmt(*stmt.iterator);
    builder.CreateBr(condBlock);

    builder.SetInsertPoint(condBlock);
    llvm::Value* cond = nullptr;
    if (stmt.condExpr)
    {
        cond = generateExpr(*stmt.condExpr);
        if (!cond->getType()->isIntegerTy(1))
        {
            if (cond->getType()->isIntegerTy()) cond = builder.CreateICmpNE(cond, llvm::ConstantInt::get(cond->getType(), 0), "forcond");
            else if (cond->getType()->isFloatTy() || cond->getType()->isDoubleTy()) cond = builder.CreateFCmpONE(cond, llvm::ConstantFP::get(cond->getType(), 0.0), "forcond");
            else throw std::runtime_error("Invalid condition type in for loop");
        }
    }
    else cond = llvm::ConstantInt::getTrue(context);

    builder.CreateCondBr(cond, bodyBlock, exitBlock);

    builder.SetInsertPoint(bodyBlock);
    loopBlocks.emplace(exitBlock, iterationBlock);
    pushScope();

    for (const auto& s : stmt.block) generateStmt(*s);
    if (builder.GetInsertBlock()->getTerminator() == nullptr) builder.CreateBr(iterationBlock);

    popScope();
    loopBlocks.pop();

    builder.SetInsertPoint(iterationBlock);
    if (stmt.iterationStmt) generateStmt(*stmt.iterationStmt);
    builder.CreateBr(condBlock);

    builder.SetInsertPoint(exitBlock);
}

void CodeGenerator::generateBreakStmt()
{
    if (loopBlocks.empty()) throw std::runtime_error("break not within loop");
    
    llvm::BasicBlock* exitBlock = loopBlocks.top().first;
    builder.CreateBr(exitBlock);
}

void CodeGenerator::generateContinueStmt()
{
    if (loopBlocks.empty()) throw std::runtime_error("continue not within loop");

    llvm::BasicBlock* condBlock = loopBlocks.top().second;
    builder.CreateBr(condBlock);
}

void CodeGenerator::generateEchoStmt(const AST::EchoStmt& stmt)
{
    llvm::Value* value = generateExpr(*stmt.expr);
    
    std::string formatStr;
    llvm::Value* promoted = value;

    if (value->getType()->isIntegerTy(32)) formatStr = "%d";
    else if (value->getType()->isFloatTy())
    {
        formatStr = "%.6f";
        promoted = builder.CreateFPExt(value, llvm::Type::getDoubleTy(context));
    }
    else if (value->getType()->isDoubleTy()) formatStr = "%.15lf";
    else if (value->getType()->isIntegerTy(8))
    {
        formatStr = "%c";
        promoted = builder.CreateSExt(value, llvm::Type::getInt8Ty(context));
    }
    else if (value->getType()->isIntegerTy(1))
    {
        formatStr = "%s";

        llvm::GlobalVariable* trueGV = new llvm::GlobalVariable(*module, llvm::ConstantDataArray::getString(context, "true")->getType(), true, llvm::GlobalValue::PrivateLinkage, llvm::ConstantDataArray::getString(context, "true"), "true_str");
        llvm::GlobalVariable* falseGV = new llvm::GlobalVariable(*module, llvm::ConstantDataArray::getString(context, "false")->getType(), true, llvm::GlobalValue::PrivateLinkage, llvm::ConstantDataArray::getString(context, "false"), "false_str");

        llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
        llvm::Value* truePtr = builder.CreateInBoundsGEP(trueGV->getValueType(), trueGV, {zero, zero}, "true_ptr");
        llvm::Value* falsePtr = builder.CreateInBoundsGEP(falseGV->getValueType(), falseGV, {zero, zero}, "false_ptr");

        promoted = builder.CreateSelect(value, truePtr, falsePtr, "bool_str");
    }
    else if (value->getType()->isPointerTy())
    {
        formatStr = "%s";
        llvm::GlobalVariable* emptyGV = new llvm::GlobalVariable(*module, llvm::ConstantDataArray::getString(context, "")->getType(), true, llvm::GlobalValue::PrivateLinkage, llvm::ConstantDataArray::getString(context, ""), "empty_str");
        llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
        llvm::Value* emptyPtr = builder.CreateInBoundsGEP(emptyGV->getValueType(), emptyGV, {zero, zero}, "empty_ptr");
        promoted = builder.CreateSelect(builder.CreateICmpNE(value, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(value->getType()))), value, emptyPtr);
    }
    else formatStr = "%d";
    
    llvm::GlobalVariable* formatGV = new llvm::GlobalVariable(*module, llvm::ConstantDataArray::getString(context, formatStr)->getType(), true, llvm::GlobalValue::PrivateLinkage, llvm::ConstantDataArray::getString(context, formatStr), "printf_format");
    llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
    llvm::Value* format = builder.CreateInBoundsGEP(formatGV->getValueType(), formatGV, {zero, zero}, "printf_format_ptr");
    
    llvm::Function* printfFunc = module->getFunction("printf");
    if (!printfFunc)
    {
        std::vector<llvm::Type*> printfArgs = {llvm::Type::getInt8Ty(context)};
        llvm::FunctionType* printfType = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), printfArgs, true);
        printfFunc = llvm::Function::Create(printfType, llvm::Function::ExternalLinkage, "printf", *module);
    }
    
    builder.CreateCall(printfFunc, { format, promoted });
}

void CodeGenerator::generateClassDeclStmt(const AST::ClassDeclStmt& stmt)
{
    classesStack.push(stmt.name);
    
    std::vector<llvm::Type*> fieldTypes;
    ClassInfo classInfo;
    int fieldIndex = 0;

    for (const auto& member : stmt.members)
    {
        if (auto field = dynamic_cast<AST::FieldMember*>(member.get()))
        {
            fieldTypes.push_back(getLLVMType(field->type));
            classInfo.fieldIndices[field->name] = fieldIndex++;
        }
    }

    classInfo.type = llvm::StructType::create(context, fieldTypes, stmt.name);
    classes[stmt.name] = classInfo;

    {
        std::vector<llvm::Type*> constructorArgs = {llvm::PointerType::get(classInfo.type, 0)};
        llvm::FunctionType* constructorType = llvm::FunctionType::get(llvm::Type::getVoidTy(context), constructorArgs, false);
        llvm::Function* constructor = llvm::Function::Create(constructorType, llvm::Function::ExternalLinkage, stmt.name + "_constructor", *module);
        classes[stmt.name].constructors.push_back(constructor);

        llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", constructor);
        builder.SetInsertPoint(entry);

        llvm::Argument* thisArg = &constructor->arg_begin()[0];
        thisArg->setName("this");

        for (const auto& member : stmt.members)
        {
            if (auto field = dynamic_cast<AST::FieldMember*>(member.get()))
            {
                int fieldIndex = classInfo.fieldIndices[field->name];
                llvm::Value* gep = builder.CreateStructGEP(classInfo.type, thisArg, fieldIndex, field->name + ".addr");
                llvm::Type* fieldTy = llvm::cast<llvm::StructType>(classInfo.type)->getElementType(fieldIndex);

                llvm::Value* initValue = nullptr;
                if (field->expr) initValue = generateExpr(*field->expr);
                else
                {
                    if (field->type.type == TypeValue::STRING) 
                    {
                        llvm::GlobalVariable* emptyStrGV = new llvm::GlobalVariable(*module, llvm::ConstantDataArray::getString(context, "")->getType(), true, llvm::GlobalValue::PrivateLinkage, llvm::ConstantDataArray::getString(context, ""), "empty_str");
                        llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
                        initValue = builder.CreateInBoundsGEP(emptyStrGV->getValueType(), emptyStrGV, {zero, zero}, "empty_str_ptr");
                    }
                    else initValue = llvm::Constant::getNullValue(fieldTy);
                }

                if (!fieldTy->isPointerTy()) initValue = castToExpectedIfNeeded(initValue, fieldTy);

                builder.CreateStore(initValue, gep);
            }
        }
        
        builder.CreateRetVoid();
    }

    for (const auto& member : stmt.members)
    {
        if (auto method = dynamic_cast<AST::MethodMember*>(member.get())) generateMethodDeclStmt(*method);
        else if (auto ctor = dynamic_cast<AST::ConstructorMember*>(member.get())) generateConstructorDecl(*ctor);
    }

    builder.ClearInsertionPoint();

    classesStack.pop();
}

void CodeGenerator::generateMethodDeclStmt(const AST::MethodMember& stmt)
{
    std::vector<llvm::Type*> argTypes;
    
    argTypes.push_back(llvm::PointerType::get(classes[classesStack.top()].type, 0));
    
    for (const auto& arg : stmt.args) argTypes.push_back(getLLVMType(arg.type));

    llvm::FunctionType* funcType = llvm::FunctionType::get(getLLVMType(stmt.retType), argTypes, false);

    llvm::Function* func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, classesStack.top() + std::string("_") + stmt.name, *module);

    classes[classesStack.top()].methods[stmt.name].push_back(func);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    pushScope();
    currentFunctionReturnType = stmt.retType;
    
    llvm::Argument* thisArg = &func->arg_begin()[0];
    thisArg->setName("this");
    setNamedValue("this", thisArg);
    typesScopeStack.top()["this"] = Type(TypeValue::CLASS, classesStack.top());

    auto argIt = func->arg_begin();
    std::advance(argIt, 1);

    for (size_t i = 0; i < stmt.args.size() && argIt != func->arg_end(); i++, argIt++)
    {
        llvm::Argument& arg = *argIt;
        arg.setName(stmt.args[i].name);
        
        llvm::AllocaInst* alloca = builder.CreateAlloca(arg.getType(), nullptr, stmt.args[i].name + ".addr");
        
        builder.CreateStore(&arg, alloca);
        setNamedValue(stmt.args[i].name, alloca);
        typesScopeStack.top()[stmt.args[i].name] = stmt.args[i].type;
    }

    for (const auto& stmt : stmt.block) generateStmt(*stmt);

    if (builder.GetInsertBlock()->getTerminator() == nullptr)
    {
        llvm::Type* retTy = stmt.retType.type == TypeValue::VOID ? llvm::Type::getVoidTy(context) : getLLVMType(stmt.retType);
        if (retTy->isVoidTy()) builder.CreateRetVoid();
        else builder.CreateRet(llvm::UndefValue::get(retTy));
    }

    popScope();
}

llvm::Value* CodeGenerator::generateExpr(const AST::Expr& expr)
{
    if (auto lit = dynamic_cast<const AST::Literal*>(&expr)) return generateLiteral(*lit);
    else if (auto arrayLit = dynamic_cast<const AST::ArrayLiteral*>(&expr)) return generateArrayLiteral(*arrayLit);
    else if (auto binary = dynamic_cast<const AST::BinaryExpr*>(&expr)) return generateBinaryExpr(*binary);
    else if (auto unary = dynamic_cast<const AST::UnaryExpr*>(&expr)) return generateUnaryExpr(*unary);
    else if (auto var = dynamic_cast<const AST::VarExpr*>(&expr)) return generateVarExpr(*var);
    else if (auto arrayAccess = dynamic_cast<const AST::ArrayExpr*>(&expr)) return generateArrayExpr(*arrayAccess);
    else if (auto func = dynamic_cast<const AST::FuncCallExpr*>(&expr)) return generateFuncCallExpr(*func);
    else if (auto n = dynamic_cast<const AST::NewExpr*>(&expr)) return generateNewExpr(*n);
    else if (auto f = dynamic_cast<const AST::FieldAccessExpr*>(&expr)) return generateFieldAccessExpr(*f);
    else if (auto m = dynamic_cast<const AST::MethodCallExpr*>(&expr)) return generateMethodCallExpr(*m);
    else if (auto t = dynamic_cast<const AST::ThisExpr*>(&expr)) return generateThisExpr(*t);
    else if (auto s = dynamic_cast<const AST::SizeofExpr*>(&expr)) return generateSizeofExpr(*s);

    throw std::runtime_error("Unknown expression type in generateExpr");
}

llvm::Value* CodeGenerator::generateLiteral(const AST::Literal& literal)
{
    const auto& value = literal.value.value;
    
    switch (literal.type.type)
    {
        case TypeValue::INT: return llvm::ConstantInt::get(context, llvm::APInt(32, std::get<int>(value), true));
        case TypeValue::FLOAT: return llvm::ConstantFP::get(context, llvm::APFloat(std::get<float>(value)));
        case TypeValue::DOUBLE: return llvm::ConstantFP::get(context, llvm::APFloat(std::get<double>(value)));
        case TypeValue::CHAR: 
        {
            char charValue = std::get<char>(value);
            return llvm::ConstantInt::get(context, llvm::APInt(8, charValue, false));
        }
        case TypeValue::BOOL: return llvm::ConstantInt::get(context, llvm::APInt(1, std::get<bool>(value), false));
        case TypeValue::STRING:
        {
            const auto& str = std::get<std::string>(value);
            
            llvm::GlobalVariable* strGV = new llvm::GlobalVariable(*module, llvm::ConstantDataArray::getString(context, str)->getType(), true, llvm::GlobalValue::PrivateLinkage, llvm::ConstantDataArray::getString(context, str), "str_" + std::to_string(reinterpret_cast<uintptr_t>(&str)));
            
            llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
            return builder.CreateInBoundsGEP(strGV->getValueType(), strGV, {zero, zero}, "str_ptr");
        }
        case TypeValue::ARRAY:
        {
            if (auto arrayLit = dynamic_cast<const AST::ArrayLiteral*>(&literal)) return generateArrayLiteral(*arrayLit);
            else throw std::runtime_error("Array literal must be of type ArrayLiteral");
        }
        default: throw std::runtime_error("Unknown literal type in generateLiteral");
    }
}

llvm::Value* CodeGenerator::generateBinaryExpr(const AST::BinaryExpr& binaryExpr)
{
    llvm::Value* left = generateExpr(*binaryExpr.left);
    llvm::Value* right = generateExpr(*binaryExpr.right);
    
    auto getCommonType = [&](llvm::Value* a, llvm::Value* b) -> llvm::Type*
    {
        llvm::Type* ta = a->getType();
        llvm::Type* tb = b->getType();

        if (ta->isPointerTy() && tb->isPointerTy()) return nullptr;
        
        if (ta->isPointerTy() || tb->isPointerTy()) return nullptr;

        if (ta->isDoubleTy() || tb->isDoubleTy()) return llvm::Type::getDoubleTy(context);
        if (ta->isFloatTy() || tb->isFloatTy()) return llvm::Type::getFloatTy(context);
        
        if (ta->isIntegerTy(8) || tb->isIntegerTy(8)) 
        {
            if (!classesStack.empty() && classesStack.top() == "char") return llvm::Type::getInt8Ty(context);
            
            return llvm::Type::getInt32Ty(context);
        }
        
        if ((ta->isIntegerTy(8) && tb->isIntegerTy(32)) || (ta->isIntegerTy(32) && tb->isIntegerTy(8)))
            if (!classesStack.empty() && classesStack.top() == "char")
                return llvm::Type::getInt8Ty(context);
        
        return llvm::Type::getInt32Ty(context);
    };

    auto castBothIfNeeded = [&](llvm::Value*& a, llvm::Value*& b, llvm::Type* target)
    {
        if (!target) return;
        if (a->getType() != target) a = castToExpectedIfNeeded(a, target);
        if (b->getType() != target) b = castToExpectedIfNeeded(b, target);
    };
    
    switch (binaryExpr.op)
    {
        case TokenType::PLUS: 
            if (left->getType()->isPointerTy() && right->getType()->isPointerTy())
            {
                llvm::Function* strcatFunc = module->getFunction("strcat");
                if (!strcatFunc)
                {
                    llvm::FunctionType* strcatType = llvm::FunctionType::get(
                        llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0),
                        {llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), 
                         llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)},
                        false);
                    strcatFunc = llvm::Function::Create(strcatType, llvm::Function::ExternalLinkage, "strcat", *module);
                }
                
                llvm::Function* mallocFunc = module->getFunction("malloc");
                if (!mallocFunc) throw std::runtime_error("malloc not available");
                
                llvm::Value* size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 256);
                llvm::Value* result = builder.CreateCall(mallocFunc, size, "concat_result");
                
                llvm::Function* strcpyFunc = module->getFunction("strcpy");
                if (!strcpyFunc)
                {
                    llvm::FunctionType* strcpyType = llvm::FunctionType::get(
                        llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0),
                        {llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), 
                         llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)},
                        false);
                    strcpyFunc = llvm::Function::Create(strcpyType, llvm::Function::ExternalLinkage, "strcpy", *module);
                }
                
                builder.CreateCall(strcpyFunc, {result, left});
                builder.CreateCall(strcatFunc, {result, right});
                
                return result;
            }
            else if (left->getType()->isPointerTy() && right->getType()->isIntegerTy(8))
            {
                llvm::Value* charStr = builder.CreateCall(charToStringFunc, {right}, "char_to_str");
                
                llvm::Function* strcatFunc = module->getFunction("strcat");
                if (!strcatFunc)
                {
                    llvm::FunctionType* strcatType = llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), {llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)}, false);
                    strcatFunc = llvm::Function::Create(strcatType, llvm::Function::ExternalLinkage, "strcat", *module);
                }
                
                llvm::Function* mallocFunc = module->getFunction("malloc");
                if (!mallocFunc) throw std::runtime_error("malloc not available");
                
                llvm::Value* size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 256);
                llvm::Value* result = builder.CreateCall(mallocFunc, size, "concat_result");
                
                llvm::Function* strcpyFunc = module->getFunction("strcpy");
                if (!strcpyFunc)
                {
                    llvm::FunctionType* strcpyType = llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), {llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)}, false);
                    strcpyFunc = llvm::Function::Create(strcpyType, llvm::Function::ExternalLinkage, "strcpy", *module);
                }
                
                builder.CreateCall(strcpyFunc, {result, left});
                builder.CreateCall(strcatFunc, {result, charStr});
                
                return result;
            }
            else if (left->getType()->isIntegerTy(8) && right->getType()->isPointerTy())
            {
                llvm::Value* charStr = builder.CreateCall(charToStringFunc, {left}, "char_to_str");
                
                llvm::Function* strcatFunc = module->getFunction("strcat");
                if (!strcatFunc)
                {
                    llvm::FunctionType* strcatType = llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), {llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)}, false);
                    strcatFunc = llvm::Function::Create(strcatType, llvm::Function::ExternalLinkage, "strcat", *module);
                }
                
                llvm::Function* mallocFunc = module->getFunction("malloc");
                if (!mallocFunc) throw std::runtime_error("malloc not available");
                
                llvm::Value* size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 256);
                llvm::Value* result = builder.CreateCall(mallocFunc, size, "concat_result");
                
                llvm::Function* strcpyFunc = module->getFunction("strcpy");
                if (!strcpyFunc)
                {
                    llvm::FunctionType* strcpyType = llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), {llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)}, false);
                    strcpyFunc = llvm::Function::Create(strcpyType, llvm::Function::ExternalLinkage, "strcpy", *module);
                }
                
                builder.CreateCall(strcpyFunc, {result, charStr});
                builder.CreateCall(strcatFunc, {result, right});
                
                return result;
            }
            else if (left->getType()->isPointerTy() && right->getType()->isIntegerTy())
            {
                llvm::Value* ch = right->getType()->isIntegerTy(8) ? right : builder.CreateTrunc(right, llvm::Type::getInt8Ty(context), "int_to_char");
                llvm::Value* rightStr = builder.CreateCall(charToStringFunc, {ch}, "char_to_str");

                llvm::Function* strcatFunc = module->getFunction("strcat");
                if (!strcatFunc)
                {
                    llvm::FunctionType* strcatType = llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), {llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)}, false);
                    strcatFunc = llvm::Function::Create(strcatType, llvm::Function::ExternalLinkage, "strcat", *module);
                }

                llvm::Function* mallocFunc = module->getFunction("malloc");
                if (!mallocFunc) throw std::runtime_error("malloc not available");

                llvm::Value* size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 256);
                llvm::Value* result = builder.CreateCall(mallocFunc, size, "concat_result");

                llvm::Function* strcpyFunc = module->getFunction("strcpy");
                if (!strcpyFunc)
                {
                    llvm::FunctionType* strcpyType = llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), {llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)}, false);
                    strcpyFunc = llvm::Function::Create(strcpyType, llvm::Function::ExternalLinkage, "strcpy", *module);
                }

                builder.CreateCall(strcpyFunc, {result, left});
                builder.CreateCall(strcatFunc, {result, rightStr});

                return result;
            }
            else if (left->getType()->isIntegerTy() && right->getType()->isPointerTy())
            {
                llvm::Value* ch = left->getType()->isIntegerTy(8) ? left : builder.CreateTrunc(left, llvm::Type::getInt8Ty(context), "int_to_char");
                llvm::Value* leftStr = builder.CreateCall(charToStringFunc, {ch}, "char_to_str");

                llvm::Function* strcatFunc = module->getFunction("strcat");
                if (!strcatFunc)
                {
                    llvm::FunctionType* strcatType = llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), {llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)}, false);
                    strcatFunc = llvm::Function::Create(strcatType, llvm::Function::ExternalLinkage, "strcat", *module);
                }

                llvm::Function* mallocFunc = module->getFunction("malloc");
                if (!mallocFunc) throw std::runtime_error("malloc not available");

                llvm::Value* size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 256);
                llvm::Value* result = builder.CreateCall(mallocFunc, size, "concat_result");

                llvm::Function* strcpyFunc = module->getFunction("strcpy");
                if (!strcpyFunc)
                {
                    llvm::FunctionType* strcpyType = llvm::FunctionType::get(llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), {llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0)}, false);
                    strcpyFunc = llvm::Function::Create(strcpyType, llvm::Function::ExternalLinkage, "strcpy", *module);
                }

                builder.CreateCall(strcpyFunc, {result, leftStr});
                builder.CreateCall(strcatFunc, {result, right});
                
                return result;
            }
            else
            {
                llvm::Type* common = getCommonType(left, right);
                castBothIfNeeded(left, right, common);
                if (common && common->isIntegerTy()) return builder.CreateAdd(left, right, "addtmp");
                if (common && common->isFloatingPointTy()) return builder.CreateFAdd(left, right, "addtmp");
                
                throw std::runtime_error("Unsupported '+' operand types");
            }
        case TokenType::MINUS: 
        {
            llvm::Type* common = getCommonType(left, right);
            castBothIfNeeded(left, right, common);
            if (common && common->isIntegerTy()) return builder.CreateSub(left, right, "subtmp");

            return builder.CreateFSub(left, right, "subtmp");
        }
        case TokenType::MULTIPLY:
        {
            llvm::Type* common = getCommonType(left, right);
            castBothIfNeeded(left, right, common);
            if (common && common->isIntegerTy()) return builder.CreateMul(left, right, "multmp");

            return builder.CreateFMul(left, right, "multmp");
        }
        case TokenType::DIVIDE:
        {
            llvm::Type* common = getCommonType(left, right);
            castBothIfNeeded(left, right, common);
            if (common && common->isIntegerTy()) return builder.CreateSDiv(left, right, "divtmp");

            return builder.CreateFDiv(left, right, "divtmp");
        }
        case TokenType::MODULO:
        {
            llvm::Type* common = getCommonType(left, right);
            castBothIfNeeded(left, right, common);
            if (common && common->isIntegerTy()) return builder.CreateSRem(left, right, "modtmp");
            
            return builder.CreateFRem(left, right, "modtmp");
        }
        case TokenType::EQUALS:
            if (left->getType()->isIntegerTy() && right->getType()->isIntegerTy()) return builder.CreateICmpEQ(left, right, "eqtmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpOEQ(left, right, "eqtmp");
            else if (left->getType()->isIntegerTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpOEQ(builder.CreateSIToFP(left, right->getType()), right, "eqtmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isIntegerTy()) return builder.CreateFCmpOEQ(left, builder.CreateSIToFP(right, left->getType()), "eqtmp");
            else return builder.CreateICmpEQ(left, right, "eqtmp");
        case TokenType::NOT_EQUALS:
            if (left->getType()->isIntegerTy() && right->getType()->isIntegerTy()) return builder.CreateICmpNE(left, right, "netmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpONE(left, right, "netmp");
            else if (left->getType()->isIntegerTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpONE(builder.CreateSIToFP(left, right->getType()), right, "netmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isIntegerTy()) return builder.CreateFCmpONE(left, builder.CreateSIToFP(right, left->getType()), "netmp");
            else return builder.CreateICmpNE(left, right, "netmp");
        case TokenType::GREATER:
            if (left->getType()->isIntegerTy() && right->getType()->isIntegerTy()) return builder.CreateICmpSGT(left, right, "gttmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpOGT(left, right, "gttmp");
            else if (left->getType()->isIntegerTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpOGT(builder.CreateSIToFP(left, right->getType()), right, "gttmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isIntegerTy()) return builder.CreateFCmpOGT(left, builder.CreateSIToFP(right, left->getType()), "gttmp");
            else return builder.CreateICmpSGT(left, right, "gttmp");
        case TokenType::GREATER_EQUALS:
            if (left->getType()->isIntegerTy() && right->getType()->isIntegerTy()) return builder.CreateICmpSGE(left, right, "getmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpOGE(left, right, "getmp");
            else if (left->getType()->isIntegerTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpOGE(builder.CreateSIToFP(left, right->getType()), right, "getmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isIntegerTy()) return builder.CreateFCmpOGE(left, builder.CreateSIToFP(right, left->getType()), "getmp");
            else return builder.CreateICmpSGE(left, right, "getmp");
        case TokenType::LESS:
            if (left->getType()->isIntegerTy() && right->getType()->isIntegerTy()) return builder.CreateICmpSLT(left, right, "lttmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpOLT(left, right, "lttmp");
            else if (left->getType()->isIntegerTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpOLT(builder.CreateSIToFP(left, right->getType()), right, "lttmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isIntegerTy()) return builder.CreateFCmpOLT(left, builder.CreateSIToFP(right, left->getType()), "lttmp");
            else return builder.CreateICmpSLT(left, right, "lttmp");
        case TokenType::LESS_EQUALS:
            if (left->getType()->isIntegerTy() && right->getType()->isIntegerTy()) return builder.CreateICmpSLE(left, right, "letmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpOLE(left, right, "letmp");
            else if (left->getType()->isIntegerTy() && right->getType()->isFloatingPointTy()) return builder.CreateFCmpOLE(builder.CreateSIToFP(left, right->getType()), right, "letmp");
            else if (left->getType()->isFloatingPointTy() && right->getType()->isIntegerTy()) return builder.CreateFCmpOLE(left, builder.CreateSIToFP(right, left->getType()), "letmp");
            else return builder.CreateICmpSLE(left, right, "letmp");
        case TokenType::AND: return builder.CreateAnd(left, right, "andtmp");
        case TokenType::OR: return builder.CreateOr(left, right, "ortmp");
        default: throw std::runtime_error("Unknown binary operator in generateBinaryExpr");
    }
}

llvm::Value* CodeGenerator::generateUnaryExpr(const AST::UnaryExpr& unaryExpr)
{
    llvm::Value* operand = generateExpr(*unaryExpr.expr);
    
    switch (unaryExpr.op)
    {
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
    if (address == nullptr)
    {
        if (!classesStack.empty())
        {
            std::string className = classesStack.top();
            auto classIt = classes.find(className);
            if (classIt != classes.end())
            {
                auto fieldIt = classIt->second.fieldIndices.find(varExpr.name);
                if (fieldIt != classIt->second.fieldIndices.end())
                {
                    llvm::Value* thisPtr = getNamedValue("this");
                    if (!thisPtr) throw std::runtime_error("'this' not available in method");
                    
                    int fieldIndex = fieldIt->second;
                    llvm::Value* gep = builder.CreateStructGEP(classIt->second.type, thisPtr, fieldIndex, varExpr.name + ".addr");
                    llvm::Type* fieldTy = llvm::cast<llvm::StructType>(classIt->second.type)->getElementType(fieldIndex);

                    return builder.CreateLoad(fieldTy, gep, varExpr.name);
                }
            }
        }

        throw std::runtime_error("Variable '" + varExpr.name + "' does not exists");
    }

    llvm::Type* valueType = nullptr;
    if (auto* allocaInst = llvm::dyn_cast<llvm::AllocaInst>(address)) valueType = allocaInst->getAllocatedType();
    else if (auto* globalVar = llvm::dyn_cast<llvm::GlobalVariable>(address)) valueType = globalVar->getValueType();
    else throw std::runtime_error("Unsupported address kind for variable: " + varExpr.name);

    return builder.CreateLoad(valueType, address, varExpr.name);
}

llvm::Value* CodeGenerator::generateFuncCallExpr(const AST::FuncCallExpr& funcExpr)
{
    std::vector<llvm::Value*> argValues;
    argValues.reserve(funcExpr.args.size());
    for (const auto& arg : funcExpr.args) argValues.push_back(generateExpr(*arg));

    auto it = functions.find(funcExpr.name);
    if (it == functions.end())
    {
        if (!classesStack.empty())
        {
            std::string className = classesStack.top();
            auto classIt = classes.find(className);
            if (classIt != classes.end())
            {
                auto methIt = classIt->second.methods.find(funcExpr.name);
                if (methIt != classIt->second.methods.end())
                {
                    for (llvm::Function* method : methIt->second)
                    {
                        auto* fnty = method->getFunctionType();
                        if (fnty->getNumParams() != argValues.size() + 1) continue;

                        std::vector<llvm::Value*> args;
                        args.reserve(argValues.size() + 1);
                        llvm::Value* thisPtr = getNamedValue("this");
                        if (!thisPtr) throw std::runtime_error("'this' not available in method");
                        args.push_back(thisPtr);

                        bool ok = true;
                        for (size_t i = 0; i < argValues.size(); i++)
                        {
                            llvm::Type* expected = fnty->getParamType(static_cast<unsigned>(i + 1));
                            llvm::Value* value = argValues[i];
                            if (!expected->isPointerTy()) value = castToExpectedIfNeeded(value, expected);

                            if (value->getType() != expected) { ok = false; break; }
                            
                            args.push_back(value);
                        }

                        if (!ok) continue;

                        return builder.CreateCall(method, args);
                    }
                }
            }
        }
        
        throw std::runtime_error("Function not declared: " + funcExpr.name);
    }

    const auto& overloads = it->second;
    
    int bestScore = -1;
    const FunctionInfo* bestOverload = nullptr;
    std::vector<llvm::Value*> bestCasted;
    
    for (const auto& fn : overloads)
    {
        if (fn.args.size() != argValues.size()) continue;

        std::vector<llvm::Value*> casted;
        casted.reserve(argValues.size());
        bool ok = true;
        int score = 0;
        
        for (size_t i = 0; i < argValues.size(); i++)
        {
            llvm::Type* expected = getLLVMType(fn.args[i].type);
            llvm::Value* value = argValues[i];

            if (value->getType() == expected) 
            { 
                casted.push_back(value); 
                continue;
            }
            if (expected->isPointerTy() && value->getType()->isPointerTy()) { ok = false; break; }

            if (value->getType()->isIntegerTy() && expected->isIntegerTy()) score += 1; // int->int conversion
            else if (value->getType()->isFloatTy() && expected->isFloatTy()) score += 1; // float->float conversion
            else if (value->getType()->isDoubleTy() && expected->isDoubleTy()) score += 1; // double->double conversion
            else if (value->getType()->isIntegerTy() && (expected->isFloatTy() || expected->isDoubleTy())) score += 2; // int->float conversion
            else if (value->getType()->isFloatTy() && expected->isDoubleTy()) score += 1; // float->double conversion
            else if (value->getType()->isDoubleTy() && expected->isFloatTy()) score += 2; // double->float conversion
            else { ok = false; break; }

            value = castToExpectedIfNeeded(value, expected);
            
            if (value->getType() != expected) { ok = false; break; }

            casted.push_back(value);
        }
        
        if (!ok) continue;
        
        if (bestScore == -1 || score < bestScore)
        {
            bestScore = score;
            bestOverload = &fn;
            bestCasted = std::move(casted);
        }
    }
    
    if (bestOverload)
    {
        llvm::Function* callee = module->getFunction(bestOverload->mangledName);
        if (!callee) throw std::runtime_error("Function not declared: " + bestOverload->mangledName);
        
        return builder.CreateCall(callee, bestCasted);
    }

    throw std::runtime_error("No matching overload found for function: " + funcExpr.name);
}

std::string CodeGenerator::mangleFunction(const std::string& name, const AST::Arguments& args) const
{
    std::vector<Type> ts; ts.reserve(args.size());
    for (const auto& a : args) ts.push_back(a.type);

    return mangleFunction(name, ts);
}

std::string CodeGenerator::mangleFunction(const std::string& name, const std::vector<Type>& types) const
{
    std::string mangled = name + "__";

    for (size_t i = 0; i < types.size(); i++)
    {
        const Type& t = types[i];
        switch (t.type)
        {
            case TypeValue::INT: mangled += "i"; break;
            case TypeValue::FLOAT: mangled += "f"; break;
            case TypeValue::DOUBLE: mangled += "d"; break;
            case TypeValue::CHAR: mangled += "c"; break;
            case TypeValue::BOOL: mangled += "b"; break;
            case TypeValue::STRING: mangled += "s"; break;
            case TypeValue::VOID: mangled += "v"; break;
            case TypeValue::CLASS: mangled += std::string("C") + t.name; break;
            case TypeValue::TRAIT: mangled += std::string("T") + t.name; break;
            default: mangled += "u"; break;
        }

        if (i + 1 < types.size()) mangled += "_";
    }

    return mangled;
}

llvm::Function* CodeGenerator::declareFunctionPrototype(const AST::FuncDeclStmt& stmt)
{
    std::string funcName = (stmt.name == "main") ? std::string("main") : mangleFunction(stmt.name, stmt.args);
    if (llvm::Function* existing = module->getFunction(funcName)) return existing;

    std::vector<llvm::Type*> argTypes;
    argTypes.reserve(stmt.args.size());
    for (const auto& arg : stmt.args) argTypes.push_back(getLLVMType(arg.type));

    llvm::FunctionType* funcType = llvm::FunctionType::get(getLLVMType(stmt.retType), argTypes, false);
    llvm::Function* func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, funcName, *module);

    FunctionInfo info
    {
        .returnType = stmt.retType,
        .args = stmt.args,
        .mangledName = funcName,
        .function = func,
    };
    functions[stmt.name].push_back(std::move(info));

    return func;
}

llvm::Value* CodeGenerator::generateNewExpr(const AST::NewExpr& expr)
{
    auto it = classes.find(expr.name);    
    ClassInfo& classInfo = it->second;
    if (!classInfo.type) throw std::runtime_error("Class type is null for '" + expr.name + "'");
    
    llvm::Value* size = llvm::ConstantExpr::getSizeOf(classInfo.type);

    llvm::Function* mallocFunc = module->getFunction("malloc");
    if (!mallocFunc) throw std::runtime_error("Failed to declare malloc");
    
    llvm::Value* allocated = builder.CreateCall(mallocFunc, size, "new");
    
    llvm::Type* targetType = llvm::PointerType::get(classInfo.type, 0);
    llvm::Value* object = builder.CreateBitCast(allocated, targetType);
    
    if (llvm::Function* defaultConstrutor = module->getFunction(expr.name + std::string("_constructor"))) builder.CreateCall(defaultConstrutor, { object });

    std::vector<llvm::Value*> argValues;
    argValues.reserve(expr.args.size());
    for (const auto& arg : expr.args) argValues.push_back(generateExpr(*arg));

    for (llvm::Function* constructor : classInfo.constructors)
    {
        if (constructor->getName() == expr.name + std::string("_constructor")) continue;

        auto* fnty = constructor->getFunctionType();
        if (fnty->getNumParams() != argValues.size() + 1) continue;

        std::vector<llvm::Value*> casted;
        casted.reserve(argValues.size() + 1);
        casted.push_back(object);

        bool ok = true;
        for (size_t i = 0; i < argValues.size(); i++)
        {
            llvm::Type* expected = fnty->getParamType(static_cast<unsigned>(i + 1));
            llvm::Value* value = argValues[i];
            if (!expected->isPointerTy()) value = castToExpectedIfNeeded(value, expected);
            
            if (value->getType() != expected) { ok = false; break; }

            casted.push_back(value);
        }

        if (!ok) continue;

        builder.CreateCall(constructor, casted);
        
        break;
    }
    
    return object;
}

llvm::Value* CodeGenerator::generateFieldAccessExpr(const AST::FieldAccessExpr& expr)
{
    llvm::Value* object = generateExpr(*expr.object);

    std::string className = resolveClassName(*expr.object);
    auto it = classes.find(className);
    if (it == classes.end()) throw std::runtime_error("Class not found: " + className);

    auto fldIt = it->second.fieldIndices.find(expr.name);
    if (fldIt == it->second.fieldIndices.end()) throw std::runtime_error("Field not found: " + expr.name);

    int fieldIndex = fldIt->second;
    llvm::Value* gep = builder.CreateStructGEP(it->second.type, object, fieldIndex, expr.name + ".addr");
    llvm::Type* fieldTy = llvm::cast<llvm::StructType>(it->second.type)->getElementType(fieldIndex);

    return builder.CreateLoad(fieldTy, gep, expr.name);
}

llvm::Value* CodeGenerator::generateMethodCallExpr(const AST::MethodCallExpr& expr)
{
    llvm::Value* object = nullptr;
    if (auto field = dynamic_cast<const AST::FieldAccessExpr*>(expr.object.get())) object = generateExpr(*field->object);
    else object = generateExpr(*expr.object);
    
    std::string className = resolveClassName(*expr.object);

    bool isBuiltinType = false;
    TypeValue builtinTypeValue = TypeValue::INT;
    for (auto& typeValue : {TypeValue::INT, TypeValue::FLOAT, TypeValue::DOUBLE, TypeValue::CHAR, TypeValue::BOOL, TypeValue::STRING})
    {
        if (className == typeToString(Type(typeValue, "")))
        {
            isBuiltinType = true;
            builtinTypeValue = typeValue;
            break;
        }
    }
    
    if (isBuiltinType)
    {
        auto traitImplIt = traitImplementations.find(className);
        if (traitImplIt != traitImplementations.end())
        {
            for (const auto& implInfo : traitImplIt->second)
            {
                auto methodIt = implInfo.implementations.find(expr.name);
                if (methodIt != implInfo.implementations.end())
                {
                    llvm::Function* method = methodIt->second;
                    auto* fnty = method->getFunctionType();
                    if (fnty->getNumParams() != expr.args.size() + 1) continue;

                    std::vector<llvm::Value*> args;
                    args.reserve(expr.args.size() + 1);
                    args.push_back(object);

                    bool ok = true;
                    for (size_t i = 0; i < expr.args.size(); i++)
                    {
                        llvm::Value* val = generateExpr(*expr.args[i]);
                        llvm::Type* expected = fnty->getParamType(i + 1);
                        if (!expected->isPointerTy()) val = castToExpectedIfNeeded(val, expected);
                        if (val->getType() != expected) { ok = false; break; }
                        args.push_back(val);
                    }

                    if (!ok) continue;

                    return builder.CreateCall(method, args);
                }
            }
        }

        throw std::runtime_error("Method not found for builtin type: " + expr.name);
    }

    auto traitIt = traits.find(className);
    if (traitIt != traits.end())
    {
        
        for (const auto& implPair : traitImplementations)
        {
            const std::string& implClassName = implPair.first;
            const auto& impls = implPair.second;
            
            for (const auto& implInfo : impls)
            {
                if (implInfo.traitName == className)
                {
                    auto methodIt = implInfo.implementations.find(expr.name);
                    if (methodIt != implInfo.implementations.end())
                    {
                        llvm::Function* method = methodIt->second;
                        auto* fnty = method->getFunctionType();
                        if (fnty->getNumParams() != expr.args.size() + 1) continue;

                        std::vector<llvm::Value*> args;
                        args.reserve(expr.args.size() + 1);
                        args.push_back(object);

                        bool ok = true;
                        for (size_t i = 0; i < expr.args.size(); i++)
                        {
                            llvm::Value* val = generateExpr(*expr.args[i]);
                            llvm::Type* expected = fnty->getParamType(i + 1);
                            if (!expected->isPointerTy()) val = castToExpectedIfNeeded(val, expected);
                            if (val->getType() != expected) { ok = false; break; }
                            args.push_back(val);
                        }

                        if (ok) return builder.CreateCall(method, args);
                    }
                }
            }
        }
        
        throw std::runtime_error("No implementation found for trait method: " + expr.name);
    }

    auto it = classes.find(className);
    if (it == classes.end()) throw std::runtime_error("Class not found: " + className);

    auto methIt = it->second.methods.find(expr.name);
    if (methIt == it->second.methods.end()) throw std::runtime_error("Method not found: " + expr.name);

    for (llvm::Function* method : methIt->second)
    {
        auto* fnty = method->getFunctionType();
        if (fnty->getNumParams() != expr.args.size() + 1) continue;

        std::vector<llvm::Value*> args;
        args.reserve(expr.args.size() + 1);
        args.push_back(object);

        bool ok = true;
        for (size_t i = 0; i < expr.args.size(); i++)
        {
            llvm::Value* val = generateExpr(*expr.args[i]);
            llvm::Type* expected = fnty->getParamType(i + 1);
            if (!expected->isPointerTy()) val = castToExpectedIfNeeded(val, expected);
            if (val->getType() != expected) { ok = false; break; }
            args.push_back(val);
        }

        if (!ok) continue;

        return builder.CreateCall(method, args);
    }

    throw std::runtime_error("No matching overload found for method: " + expr.name);
}

void CodeGenerator::generateConstructorDecl(const AST::ConstructorMember& ctor)
{
    std::vector<llvm::Type*> argTypes;
    argTypes.push_back(llvm::PointerType::get(classes[classesStack.top()].type, 0));
    for (const auto& a : ctor.args) argTypes.push_back(getLLVMType(a.type));

    std::string baseName = classesStack.top() + std::string("_constructor");
    std::string mangled = mangleFunction(baseName, ctor.args);

    llvm::FunctionType* fnty = llvm::FunctionType::get(llvm::Type::getVoidTy(context), argTypes, false);
    llvm::Function* fn = llvm::Function::Create(fnty, llvm::Function::ExternalLinkage, mangled, *module);
    classes[classesStack.top()].constructors.push_back(fn);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", fn);
    builder.SetInsertPoint(entry);

    pushScope();

    llvm::Argument* thisArg = &fn->arg_begin()[0];
    thisArg->setName("this");
    setNamedValue("this", thisArg);
    typesScopeStack.top()["this"] = Type(TypeValue::CLASS, classesStack.top());

    auto it = fn->arg_begin();
    std::advance(it, 1);
    for (size_t i = 0; i < ctor.args.size() && it != fn->arg_end(); i++, it++)
    {
        llvm::Argument& arg = *it; arg.setName(ctor.args[i].name);
        llvm::AllocaInst* alloca = builder.CreateAlloca(arg.getType(), nullptr, ctor.args[i].name + ".addr");
        builder.CreateStore(&arg, alloca);
        setNamedValue(ctor.args[i].name, alloca);
        typesScopeStack.top()[ctor.args[i].name] = ctor.args[i].type;
    }

    for (const auto& s : ctor.block) generateStmt(*s);

    if (builder.GetInsertBlock()->getTerminator() == nullptr) builder.CreateRetVoid();

    popScope();
}

void CodeGenerator::generateTraitDeclStmt(const AST::TraitDeclStmt& stmt)
{
    TraitInfo traitInfo;
    
    for (const auto& method : stmt.methods)
    {
        if (auto traitMethod = dynamic_cast<const AST::TraitMethodMember*>(method.get()))
        {
            std::vector<llvm::Type*> paramTypes;
            paramTypes.push_back(llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0));
            
            for (const auto& arg : traitMethod->args) paramTypes.push_back(getLLVMType(arg.type));
            
            llvm::Type* retType = getLLVMType(traitMethod->retType);
            llvm::FunctionType* funcType = llvm::FunctionType::get(retType, paramTypes, false);
            
            std::string mangledName = mangleFunction(traitMethod->name, traitMethod->args);
            llvm::Function* func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, mangledName, *module);
            
            traitInfo.methods[traitMethod->name].push_back(func);
        }
    }
    
    traits[stmt.name] = std::move(traitInfo);
}

void CodeGenerator::generateTraitImplStmt(const AST::TraitImplStmt& stmt)
{
    TraitImplInfo implInfo;
    implInfo.traitName = stmt.traitName;
    implInfo.className = stmt.targetType.name;
    
    bool isBuiltinType = false;
    TypeValue builtinTypeValue;
    for (auto& typeValue : {TypeValue::INT, TypeValue::FLOAT, TypeValue::DOUBLE, TypeValue::CHAR, TypeValue::BOOL, TypeValue::STRING})
    {
        if (stmt.targetType.type == typeValue)
        {
            isBuiltinType = true;
            builtinTypeValue = typeValue;
            break;
        }
    }
    
    for (const auto& impl : stmt.implementations)
    {
        if (auto method = dynamic_cast<const AST::MethodMember*>(impl.get()))
        {
            std::vector<llvm::Type*> paramTypes;
            
            if (isBuiltinType) paramTypes.push_back(getLLVMType(stmt.targetType));
            else paramTypes.push_back(llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0));
            
            for (const auto& arg : method->args) paramTypes.push_back(getLLVMType(arg.type));
            
            llvm::Type* retType = getLLVMType(method->retType);
            llvm::FunctionType* funcType = llvm::FunctionType::get(retType, paramTypes, false);
            
            std::string mangledName = stmt.targetType.name + "_" + mangleFunction(method->name, method->args);
            llvm::Function* func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, mangledName, *module);
            
            llvm::BasicBlock* entryBlock = llvm::BasicBlock::Create(context, "entry", func);
            builder.SetInsertPoint(entryBlock);
            
            classesStack.push(stmt.targetType.name);
            
            if (isBuiltinType)
            {
                llvm::AllocaInst* thisAlloca = builder.CreateAlloca(func->getArg(0)->getType(), nullptr, "this.addr");
                builder.CreateStore(func->getArg(0), thisAlloca);
                setNamedValue("this", thisAlloca);
            }
            else setNamedValue("this", func->getArg(0));
            
            pushScope();
            currentFunctionReturnType = method->retType;
            
            auto argIt = func->arg_begin();
            std::advance(argIt, 1);
            
            for (size_t i = 0; i < method->args.size() && argIt != func->arg_end(); i++, argIt++)
            {
                llvm::Argument& arg = *argIt;
                arg.setName(method->args[i].name);
                
                llvm::AllocaInst* alloca = builder.CreateAlloca(arg.getType(), nullptr, method->args[i].name + ".addr");
                builder.CreateStore(&arg, alloca);
                setNamedValue(method->args[i].name, alloca);
                typesScopeStack.top()[method->args[i].name] = method->args[i].type;
            }
            
            for (const auto& stmtPtr : method->block) generateStmt(*stmtPtr);
            popScope();
            
            classesStack.pop();
            
            if (method->retType.type == TypeValue::VOID) builder.CreateRetVoid();
            
            implInfo.implementations[method->name] = func;
        }
    }
    
    if (!isBuiltinType)
    {
        auto classIt = classes.find(stmt.targetType.name);
        if (classIt != classes.end())
        {
            for (const auto& impl : stmt.implementations)
            {
                if (auto method = dynamic_cast<const AST::MethodMember*>(impl.get()))
                {
                    bool exists = false;
                    for (const auto& existingMethod : classIt->second.methods[method->name])
                    {
                        if (existingMethod != nullptr)
                        {
                            exists = true;
                            break;
                        }
                    }
                    
                    if (!exists)
                    {
                        auto implIt = implInfo.implementations.find(method->name);
                        if (implIt != implInfo.implementations.end()) classIt->second.methods[method->name].push_back(implIt->second);
                    }
                }
            }
        }
    }
    
    traitImplementations[stmt.targetType.name].push_back(std::move(implInfo));
    
    builder.ClearInsertionPoint();
}

llvm::Value* CodeGenerator::generateThisExpr(const AST::ThisExpr&)
{
    llvm::Value* thisValue = getNamedValue("this");
    if (!thisValue) return nullptr;
    
    if (auto* allocaInst = llvm::dyn_cast<llvm::AllocaInst>(thisValue))
    {
        llvm::Type* allocatedType = allocaInst->getAllocatedType();
        return builder.CreateLoad(allocatedType, allocaInst, "this");
    }

    return thisValue;
}

llvm::Value* CodeGenerator::generateSizeofExpr(const AST::SizeofExpr& expr)
{
    llvm::Type* targetType = nullptr;
    
    if (expr.isTypeExpression)
    {
        if (expr.type.type == TypeValue::CLASS)
        {
            auto it = classes.find(expr.type.name);
            if (it == classes.end()) throw std::runtime_error("Class not found: " + expr.type.name);
            if (!it->second.type) throw std::runtime_error("Class type is null for: " + expr.type.name);
            
            targetType = it->second.type;
        }
        else if (expr.type.type == TypeValue::TRAIT) targetType = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
        else targetType = getLLVMType(expr.type);
    }
    else if (expr.expr)
    {
        if (auto varExpr = dynamic_cast<const AST::VarExpr*>(expr.expr.get()))
        {
            Type varType = getVariableType(varExpr->name);
            if (varType.type == TypeValue::ARRAY)
            {
                auto it = arrayTypes.find(varExpr->name);
                if (it != arrayTypes.end()) targetType = it->second;
                else
                {
                    targetType = getLLVMType(varType);
                    if (auto ptrType = llvm::dyn_cast<llvm::PointerType>(targetType))
                        if (auto arrayType = llvm::dyn_cast<llvm::ArrayType>(ptrType->getArrayElementType()))
                            targetType = arrayType;
                }
            }
            else
            {
                llvm::Value* exprValue = generateExpr(*expr.expr);
                if (!exprValue) throw std::runtime_error("Cannot generate code for expression in sizeof");
                
                targetType = exprValue->getType();
            }
        }
        else if (expr.type.type == TypeValue::ARRAY)
        {
            targetType = getLLVMType(expr.type);
            if (auto ptrType = llvm::dyn_cast<llvm::PointerType>(targetType))
                if (auto arrayType = llvm::dyn_cast<llvm::ArrayType>(ptrType->getArrayElementType()))
                    targetType = arrayType;
        }
        else
        {
            llvm::Value* exprValue = generateExpr(*expr.expr);
            if (!exprValue) throw std::runtime_error("Cannot generate code for expression in sizeof");
            
            targetType = exprValue->getType();
        }
    }
    else targetType = getLLVMType(expr.type);
    
    if (!targetType) throw std::runtime_error("Cannot determine type for sizeof");
    
    const llvm::DataLayout& dataLayout = module->getDataLayout();
    uint64_t size = dataLayout.getTypeAllocSize(targetType);
    
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), size);
}

void CodeGenerator::generateArrayAsgnStmt(const AST::ArrayAsgnStmt& stmt)
{
    llvm::Value* arrayPtr = getNamedValue(stmt.name);
    if (!arrayPtr) throw std::runtime_error("Array variable '" + stmt.name + "' not found");

    llvm::Value* index = generateExpr(*stmt.index);
    
    llvm::Value* value = generateExpr(*stmt.expr);
    
    llvm::PointerType* ptrType = llvm::cast<llvm::PointerType>(arrayPtr->getType());
    if (!ptrType) throw std::runtime_error("Variable '" + stmt.name + "' is not a pointer type");
    
    llvm::ArrayType* arrayType = nullptr;
    auto it = arrayTypes.find(stmt.name);
    if (it != arrayTypes.end()) arrayType = it->second;
    else throw std::runtime_error("Array type not found for variable: " + stmt.name);
    
    std::vector<llvm::Value*> indices =
    {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
        index
    };
    
    llvm::Value* targetArray;
    if (auto* allocaInst = llvm::dyn_cast<llvm::AllocaInst>(arrayPtr)) targetArray = builder.CreateLoad(arrayPtr->getType(), arrayPtr, stmt.name + ".deref");
    else targetArray = builder.CreateLoad(arrayPtr->getType(), arrayPtr, stmt.name + ".load");
    
    llvm::Value* elementPtr = builder.CreateGEP(arrayType, targetArray, indices, stmt.name + ".element");
    
    builder.CreateStore(value, elementPtr);
}

llvm::Value* CodeGenerator::generateArrayLiteral(const AST::ArrayLiteral& arrayLit)
{
    llvm::Type* elementType = getLLVMType(arrayLit.elementType);
    
    size_t arraySize = arrayLit.elements.size();
    
    llvm::ArrayType* arrayType = llvm::ArrayType::get(elementType, arraySize);
    
    if (builder.GetInsertBlock() == nullptr)
    {
        std::vector<llvm::Constant*> elements;
        elements.reserve(arraySize);
        
        for (size_t i = 0; i < arraySize; ++i)
        {
            llvm::Value* element = generateExpr(*arrayLit.elements[i]);
            llvm::Constant* constElement = llvm::dyn_cast<llvm::Constant>(element);
            if (!constElement) throw std::runtime_error("Global array literal element " + std::to_string(i) + " is not a constant expression");
            
            elements.push_back(constElement);
        }
        
        llvm::Constant* constantArray = llvm::ConstantArray::get(arrayType, elements);
        arrayTypes["array_literal"] = arrayType;
        
        return constantArray;
    }
    else
    {
        llvm::AllocaInst* arrayAlloca = builder.CreateAlloca(arrayType, nullptr, "array_literal");
        
        for (size_t i = 0; i < arraySize; ++i)
        {
            llvm::Value* element = generateExpr(*arrayLit.elements[i]);
            
            std::vector<llvm::Value*> indices =
            {
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), i)
            };
            llvm::Value* elementPtr = builder.CreateGEP(arrayType, arrayAlloca, indices, "element_ptr");
            
            builder.CreateStore(element, elementPtr);
        }
        
        arrayTypes["array_literal"] = arrayType;
        
        return arrayAlloca;
    }
}

llvm::Value* CodeGenerator::generateArrayExpr(const AST::ArrayExpr& expr)
{
    llvm::Value* arrayPtr = getNamedValue(expr.name);
    if (!arrayPtr) throw std::runtime_error("Array variable '" + expr.name + "' not found");

    llvm::Value* index = generateExpr(*expr.index);
    
    llvm::PointerType* ptrType = llvm::cast<llvm::PointerType>(arrayPtr->getType());
    if (!ptrType) throw std::runtime_error("Variable '" + expr.name + "' is not a pointer type");
    
    Type varType = getNamedType(expr.name);
    
    if (varType.type == TypeValue::STRING)
    {
        llvm::Value* strPtr = builder.CreateLoad(arrayPtr->getType(), arrayPtr, expr.name + ".load");
        llvm::Value* charPtr = builder.CreateGEP(llvm::Type::getInt8Ty(context), strPtr, index, expr.name + ".char");
        
        return builder.CreateLoad(llvm::Type::getInt8Ty(context), charPtr, expr.name + ".char_value");
    }
    
    llvm::ArrayType* arrayType = nullptr;
    auto it = arrayTypes.find(expr.name);
    if (it != arrayTypes.end()) arrayType = it->second;
    else throw std::runtime_error("Array type not found for variable: " + expr.name);
    
    std::vector<llvm::Value*> indices =
    {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
        index
    };
    
    llvm::Value* targetArray;
    if (auto* allocaInst = llvm::dyn_cast<llvm::AllocaInst>(arrayPtr)) targetArray = builder.CreateLoad(arrayPtr->getType(), arrayPtr, expr.name + ".deref");
    else targetArray = builder.CreateLoad(arrayPtr->getType(), arrayPtr, expr.name + ".load");
    
    llvm::Value* elementPtr = builder.CreateGEP(arrayType, targetArray, indices, expr.name + ".element");
    
    return builder.CreateLoad(arrayType->getElementType(), elementPtr, expr.name + ".value");
}

void CodeGenerator::printIR() const
{
    module->print(llvm::errs(), nullptr);
}


