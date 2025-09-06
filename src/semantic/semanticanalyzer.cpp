#include "../../include/semantic/semanticanalyzer.hpp"
#include <llvm/IR/Type.h>

std::string typeToString(Type);

void SemanticAnalyzer::analyze(const std::vector<AST::StmtPtr>& stmts)
{
    variables.emplace(std::map<std::string, Type>{});
    for (const AST::StmtPtr& stmt : stmts) analyzeStmt(*stmt);
}

void SemanticAnalyzer::analyzeStmt(AST::Stmt& stmt)
{
    if (auto vds = dynamic_cast<AST::VarDeclStmt*>(&stmt)) analyzeVarDeclStmt(*vds);
    else if (auto aas = dynamic_cast<AST::ArrayAsgnStmt*>(&stmt)) analyzeArrayAsgnStmt(*aas);
    else if (auto vas = dynamic_cast<AST::VarAsgnStmt*>(&stmt)) analyzeVarAsgnStmt(*vas);
    else if (auto fas = dynamic_cast<AST::FieldAsgnStmt*>(&stmt)) analyzeFieldAsgnStmt(*fas);
    else if (auto fds = dynamic_cast<AST::FuncDeclStmt*>(&stmt)) analyzeFuncDeclStmt(*fds);
    else if (auto fcs = dynamic_cast<AST::FuncCallStmt*>(&stmt)) analyzeFuncCallStmt(*fcs);
    else if (auto mcs = dynamic_cast<AST::MethodCallStmt*>(&stmt)) analyzeMethodCallStmt(*mcs);
    else if (auto rs = dynamic_cast<AST::ReturnStmt*>(&stmt)) analyzeReturnStmt(*rs);
    else if (auto ies = dynamic_cast<AST::IfElseStmt*>(&stmt)) analyzeIfElseStmt(*ies);
    else if (auto wls = dynamic_cast<AST::WhileLoopStmt*>(&stmt)) analyzeWhileLoopStmt(*wls);
    else if (auto dwls = dynamic_cast<AST::DoWhileLoopStmt*>(&stmt)) analyzeDoWhileLoopStmt(*dwls);
    else if (auto fls = dynamic_cast<AST::ForLoopStmt*>(&stmt)) analyzeForLoopStmt(*fls);
    else if (dynamic_cast<AST::BreakStmt*>(&stmt)) analyzeBreakStmt();
    else if (dynamic_cast<AST::ContinueStmt*>(&stmt)) analyzeContinueStmt();
    else if (auto es = dynamic_cast<AST::EchoStmt*>(&stmt)) analyzeEchoStmt(*es);
    else if (auto cds = dynamic_cast<AST::ClassDeclStmt*>(&stmt)) analyzeClassDeclStmt(*cds);
    else if (auto tds = dynamic_cast<AST::TraitDeclStmt*>(&stmt)) analyzeTraitDeclStmt(*tds);
    else if (auto tis = dynamic_cast<AST::TraitImplStmt*>(&stmt)) analyzeTraitImplStmt(*tis);
}

void SemanticAnalyzer::analyzeVarDeclStmt(AST::VarDeclStmt& vds)
{
    if (vds.expr)
        if (variables.size() == 1 && !isConstExpr(*vds.expr)) throw std::runtime_error("Global variable initializer must be a constant expression");

    if (vds.type.type == TypeValue::CLASS)
    {
        if (traits.find(vds.type.name) != traits.end()) vds.type.type = TypeValue::TRAIT;
        else if (classes.find(vds.type.name) == classes.end()) throw std::runtime_error("Type '" + vds.type.name + "' not found");
    }

    if (variables.top().find(vds.name) != variables.top().end()) throw std::runtime_error("Variable '" + typeToString(vds.type) + " " + vds.name + "' already declared");

    if (vds.expr)
    {
        variablesTypes.emplace(vds.type);
        Type exprType = analyzeExpr(*vds.expr);
        if (!canImplicitlyCast(exprType, vds.type)) throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(exprType) + " to " + typeToString(vds.type));

        variablesTypes.pop();
    }

    variables.top().insert({vds.name, vds.type});
}

void SemanticAnalyzer::analyzeArrayAsgnStmt(AST::ArrayAsgnStmt& aas)
{
    auto copy = variables;
    Type arrayType;
    bool found = false;
    
    while (!copy.empty())
    {
        const auto& scope = copy.top();
        auto it = scope.find(aas.name);
        if (it != scope.end())
        {
            arrayType = it->second;
            found = true;
            break;
        }

        copy.pop();
    }

    if (!found)
    {
        if (!classesStack.empty())
        {
            std::string className = classesStack.top();
            auto classIt = classes.find(className);
            if (classIt != classes.end())
            {
                for (const auto& field : classIt->second.fields)
                {
                    if (field->name == aas.name)
                    {
                        arrayType = field->type;
                        found = true;
                        break;
                    }
                }
            }
        }
    }
    
    if (!found) throw std::runtime_error("Array variable '" + aas.name + "' does not exist");

    if (arrayType.type != TypeValue::ARRAY) throw std::runtime_error("Variable '" + aas.name + "' is not an array");

    Type indexType = analyzeExpr(*aas.index);
    if (indexType.type != TypeValue::INT) throw std::runtime_error("Array index must be of type int");

    Type valueType = analyzeExpr(*aas.expr);
    if (!canImplicitlyCast(valueType, *arrayType.elementType)) 
        throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(valueType) + " to " + typeToString(*arrayType.elementType));
}

void SemanticAnalyzer::analyzeVarAsgnStmt(AST::VarAsgnStmt& vas)
{
    auto copy = variables;
    while (!copy.empty())
    {
        const auto& scope = copy.top();
        auto it = scope.find(vas.name);
        if (it != scope.end())
        {
            Type exprType = analyzeExpr(*vas.expr);
            if (!canImplicitlyCast(exprType, it->second)) throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(exprType) + " to " + typeToString(it->second));

            return;
        }

        copy.pop();
    }

    if (!classesStack.empty())
    {
        std::string className = classesStack.top();
        auto classIt = classes.find(className);
        if (classIt != classes.end())
        {
            for (const auto& field : classIt->second.fields)
            {
                if (field->name == vas.name)
                {
                    Type exprType = analyzeExpr(*vas.expr);
                    if (!canImplicitlyCast(exprType, field->type)) throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(exprType) + " to " + typeToString(field->type));

                    return;
                }
            }
        }
    }
    
    throw std::runtime_error("Variable '" + vas.name + "' does not exists");
}

void SemanticAnalyzer::analyzeFieldAsgnStmt(AST::FieldAsgnStmt& stmt)
{
    Type targetType = analyzeExpr(*stmt.target);
    if (targetType.type != TypeValue::CLASS) throw std::runtime_error("Field assignment on non-class type");

    std::string className = targetType.name;
    auto classIt = classes.find(className);
    if (classIt == classes.end()) throw std::runtime_error("Class '" + className + "' not found");

    for (const auto& field : classIt->second.fields)
    {
        if (field->name == stmt.name)
        {
            Type exprType = analyzeExpr(*stmt.expr);
            if (!canImplicitlyCast(exprType, field->type)) throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(exprType) + " to " + typeToString(field->type));
            
            return;
        }
    }
    
    throw std::runtime_error("Field '" + stmt.name + "' does not exist in class '" + className + "'");
}

void SemanticAnalyzer::analyzeFuncDeclStmt(AST::FuncDeclStmt& fds)
{
    if (fds.retType.type == TypeValue::CLASS)
    {
        if (traits.find(fds.retType.name) != traits.end()) fds.retType.type = TypeValue::TRAIT;
        else if (classes.find(fds.retType.name) == classes.end()) throw std::runtime_error("Return type '" + fds.retType.name + "' not found");
    }

    for (auto& arg : fds.args)
    {
        if (arg.type.type == TypeValue::CLASS)
        {
            if (traits.find(arg.type.name) != traits.end()) arg.type.type = TypeValue::TRAIT;
            else if (classes.find(arg.type.name) == classes.end()) throw std::runtime_error("Parameter type '" + arg.type.name + "' not found");
        }
    }

    auto& overloads = functions[fds.name];
    for (const auto& existing : overloads)
    {
        if (existing.args.size() != fds.args.size()) continue;
        bool sameArgs = true;
        for (size_t i = 0; i < fds.args.size(); i++)
        {
            if (existing.args[i].type.type != fds.args[i].type.type || existing.args[i].type.name != fds.args[i].type.name)
            {
                sameArgs = false;
                break;
            }
        }

        if (sameArgs)
        {
            std::string argsStr;
            for (size_t i = 0; i < fds.args.size(); i++)
            {
                argsStr.append(typeToString(fds.args[i].type));
                if (i < fds.args.size() - 1) argsStr.append(", ");
            }

            throw std::runtime_error("Function '" + typeToString(fds.retType) + " " + fds.name + "(" + argsStr + ")' already declared");
        }
    }

    functionReturnTypes.push(fds.retType);
    variables.emplace(std::map<std::string, Type>{});
    
    for (const auto& arg : fds.args) variables.top()[arg.name] = arg.type;
    for (const auto& stmt : fds.block) analyzeStmt(*stmt);

    variables.pop();
    functionReturnTypes.pop();

    FunctionInfo info
    {
        .returnType = fds.retType,
        .args = fds.args,
        .mangledName = mangleFunction(fds.name, fds.args)
    };
    overloads.push_back(std::move(info));
}

void SemanticAnalyzer::analyzeFuncCallStmt(AST::FuncCallStmt& fcs)
{
    std::vector<Type> argTypes;
    argTypes.reserve(fcs.args.size());
    for (const auto& a : fcs.args) argTypes.push_back(analyzeExpr(*a));

    auto it = functions.find(fcs.name);
    if (it == functions.end())
    {
        if (!classesStack.empty())
        {
            std::string className = classesStack.top();
            auto classIt = classes.find(className);
            if (classIt != classes.end())
            {
                for (const auto& method : classIt->second.methods)
                {
                    if (method->name != fcs.name) continue;
                    if (method->args.size() != argTypes.size()) continue;

                    bool ok = true;
                    for (size_t i = 0; i < argTypes.size(); i++)
                        if (!canImplicitlyCast(argTypes[i], method->args[i].type)) { ok = false; break; }

                    if (ok) return;
                }
            }
        }

        throw std::runtime_error("Function '" + fcs.name + "' does not exists");
    }

    const auto& overloads = it->second;
    bool matched = false;
    for (const auto& fn : overloads)
    {
        if (fn.args.size() != argTypes.size()) continue;

        bool ok = true;
        for (size_t i = 0; i < argTypes.size(); i++)
            if (!canImplicitlyCast(argTypes[i], fn.args[i].type)) { ok = false; break; }

        if (ok) { matched = true; break; }
    }

    if (!matched)
    {
        std::string argsStr;
        for (size_t i = 0; i < argTypes.size(); i++)
        {
            argsStr.append(typeToString(argTypes[i]));
            if (i < argTypes.size() - 1) argsStr.append(", ");
        }

        throw std::runtime_error("No matching function for call '" + fcs.name + "(" + argsStr + ")'");
    }
}

void SemanticAnalyzer::analyzeMethodCallStmt(AST::MethodCallStmt& mcs)
{
    AST::MethodCallExpr expr(std::unique_ptr<AST::Expr>(mcs.object->clone()), mcs.name, {});
    expr.args.reserve(mcs.args.size());
    for (const auto& a : mcs.args) expr.args.push_back(std::unique_ptr<AST::Expr>(a->clone()));
    
    analyzeMethodCallExpr(expr);
}

void SemanticAnalyzer::analyzeReturnStmt(AST::ReturnStmt& rs)
{
    if (functionReturnTypes.empty()) throw std::runtime_error("'return' outside of function");
    
    Type expected = functionReturnTypes.top();

    if (!rs.expr)
    {
        if (expected.type != TypeValue::VOID) throw std::runtime_error("Missing return value for function returning " + typeToString(expected) + "");

        return;
    }

    Type actual = analyzeExpr(*rs.expr);
    if (!canImplicitlyCast(actual, expected)) throw std::runtime_error("Type error: Cannot implicitly cast return value from " + typeToString(actual) + " to " + typeToString(expected));
}

void SemanticAnalyzer::analyzeIfElseStmt(AST::IfElseStmt& ies)
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("If statement cannot be outside function");

    Type condType = analyzeExpr(*ies.condExpr);

    if (condType.type != TypeValue::BOOL) throw std::runtime_error("Conditional expression must be return bool value");

    variables.emplace(std::map<std::string, Type>{});
    for (const auto& stmt : ies.thenBranch) analyzeStmt(*stmt);
    variables.pop();

    if (ies.elseBranch.size() != 0)
    {
        variables.emplace(std::map<std::string, Type>{});
        for (const auto& stmt : ies.elseBranch) analyzeStmt(*stmt);
        variables.pop();
    }
}

void SemanticAnalyzer::analyzeWhileLoopStmt(AST::WhileLoopStmt& wls)
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("While statement cannot be outside function");

    Type condType = analyzeExpr(*wls.condExpr);

    if (condType.type != TypeValue::BOOL) throw std::runtime_error("Conditional expression must be return bool value");

    variables.emplace(std::map<std::string, Type>{});
    loopDepth++;
    for (const auto& stmt : wls.block) analyzeStmt(*stmt);
    loopDepth--;
    variables.pop();
}

void SemanticAnalyzer::analyzeDoWhileLoopStmt(AST::DoWhileLoopStmt& dwls)
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("While statement cannot be outside function");

    Type condType = analyzeExpr(*dwls.condExpr);

    if (condType.type != TypeValue::BOOL) throw std::runtime_error("Conditional expression must be return bool value");

    variables.emplace(std::map<std::string, Type>{});
    loopDepth++;
    for (const auto& stmt : dwls.block) analyzeStmt(*stmt);
    loopDepth--;
    variables.pop();
}

void SemanticAnalyzer::analyzeForLoopStmt(AST::ForLoopStmt& fls)
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("While statement cannot be outside function");
    
    variables.emplace(std::map<std::string, Type>{});

    analyzeStmt(*fls.iterator);

    Type condType = analyzeExpr(*fls.condExpr);

    if (condType.type != TypeValue::BOOL) throw std::runtime_error("Conditional expression must be return bool value");

    analyzeStmt(*fls.iterationStmt);
    
    loopDepth++;
    for (const auto& stmt : fls.block) analyzeStmt(*stmt);
    loopDepth--;
    variables.pop();
}

void SemanticAnalyzer::analyzeBreakStmt()
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("break cannot be outside function");

    if (loopDepth == 0) throw std::runtime_error("break must be inside a loop");
}

void SemanticAnalyzer::analyzeContinueStmt()
{
    if (functionReturnTypes.size() == 0) throw std::runtime_error("continue cannot be outside function");
    
    if (loopDepth == 0) throw std::runtime_error("continue must be inside a loop");
}

void SemanticAnalyzer::analyzeEchoStmt(AST::EchoStmt& es)
{
    analyzeExpr(*es.expr);
}

void SemanticAnalyzer::analyzeClassDeclStmt(AST::ClassDeclStmt& cds)
{
    if (classes.find(cds.name) != classes.end()) throw std::runtime_error("Class '" + cds.name + "' already declared");

    ClassInfo classInfo;
    classesStack.push(cds.name);
    classes[cds.name] = std::move(classInfo);

    std::vector<std::unique_ptr<AST::Member>> membersCopy;
    for (auto& member : cds.members)
    {
        if (auto field = dynamic_cast<AST::FieldMember*>(member.get()))
            membersCopy.push_back(std::make_unique<AST::FieldMember>(field->access, field->name, field->type, field->expr ? std::unique_ptr<AST::Expr>(field->expr->clone()) : nullptr));

        else if (auto method = dynamic_cast<AST::MethodMember*>(member.get()))
        {
            std::vector<std::unique_ptr<AST::Stmt>> blockCopy;
            for (auto& stmt : method->block) blockCopy.push_back(std::unique_ptr<AST::Stmt>(stmt->clone()));

            membersCopy.push_back(std::make_unique<AST::MethodMember>(method->access, method->name, method->retType, method->args, std::move(blockCopy)));
        }
        else if (auto ctor = dynamic_cast<AST::ConstructorMember*>(member.get()))
        {
            std::vector<std::unique_ptr<AST::Stmt>> blockCopy;
            for (auto& stmt : ctor->block) blockCopy.push_back(std::unique_ptr<AST::Stmt>(stmt->clone()));

            membersCopy.push_back(std::make_unique<AST::ConstructorMember>(ctor->access, ctor->args, std::move(blockCopy)));
        }
    }

    for (auto& member : membersCopy)
    {
        if (auto field = dynamic_cast<AST::FieldMember*>(member.get()))
        {
            analyzeFieldMember(classInfo.fields, *field);
            classes[cds.name].fields.emplace_back(static_cast<AST::FieldMember*>(member.release()));
        }
        else if (auto method = dynamic_cast<AST::MethodMember*>(member.get()))
        {
            analyzeMethodMember(cds.name, classInfo.methods, *method);
            classes[cds.name].methods.emplace_back(static_cast<AST::MethodMember*>(member.release()));
        }
        else if (auto ctor = dynamic_cast<AST::ConstructorMember*>(member.get()))
        {
            analyzeConstructorMember(cds.name, classInfo.constructors, *ctor);
            classes[cds.name].constructors.emplace_back(static_cast<AST::ConstructorMember*>(member.release()));
        }
        else throw std::runtime_error("Unsupported class member declaration");
    }

    classesStack.pop();
}

void SemanticAnalyzer::analyzeFieldMember(std::vector<std::unique_ptr<AST::FieldMember>>& fields, AST::FieldMember& field)
{
    for (const auto& existing : fields) if (existing->name == field.name) throw std::runtime_error("Field '" + field.name + "' already declared");
    if (field.expr)
    {
        Type exprType = analyzeExpr(*field.expr);
        if (!canImplicitlyCast(exprType, field.type)) throw std::runtime_error("Type error: Cannot implicitly cast " + typeToString(exprType) + " to " + typeToString(field.type));
    }
}

void SemanticAnalyzer::analyzeMethodMember(std::string className, std::vector<std::unique_ptr<AST::MethodMember>>& methods, AST::MethodMember& mm)
{
    for (const auto& existing : methods)
    {
        if (existing->name == mm.name && existing->args.size() == mm.args.size())
        {
            bool sameArgs = true;
            for (size_t i = 0; i < mm.args.size(); i++)
            {
                if (existing->args[i].type.type != mm.args[i].type.type)
                {
                    sameArgs = false;
                    break;
                }
            }

            if (sameArgs)
            {
                std::string argsStr;
                for (size_t i = 0; i < mm.args.size(); i++)
                {
                    argsStr.append(typeToString(mm.args[i].type));
                    if (i < mm.args.size() - 1) argsStr.append(", ");
                }
                
                throw std::runtime_error("Method '" + typeToString(mm.retType) + " " + mm.name + "(" + argsStr + ")' already declared");
            }
        }
    }

    functionReturnTypes.push(mm.retType);
    variables.emplace(std::map<std::string, Type>{});
    variables.top()["this"] = Type(TypeValue::CLASS, className);

    for (const auto& arg : mm.args) variables.top()[arg.name] = arg.type;
    for (const auto& stmt : mm.block) analyzeStmt(*stmt);

    variables.pop();
    functionReturnTypes.pop();
}

void SemanticAnalyzer::analyzeConstructorMember(std::string className, std::vector<std::unique_ptr<AST::ConstructorMember>>& ctors, AST::ConstructorMember& cm)
{
    for (const auto& existing : ctors)
    {
        if (existing->args.size() != cm.args.size()) continue;
        bool sameArgs = true;
        for (size_t i = 0; i < cm.args.size(); i++)
        {
            if (existing->args[i].type.type != cm.args[i].type.type || existing->args[i].type.name != cm.args[i].type.name)
            {
                sameArgs = false;
                break;
            }
        }
        if (sameArgs) throw std::runtime_error("Constructor with the same signature already declared for class '" + className + "'");
    }

    functionReturnTypes.push(Type(TypeValue::VOID, "void"));
    variables.emplace(std::map<std::string, Type>{});
    variables.top()["this"] = Type(TypeValue::CLASS, className);

    for (const auto& arg : cm.args) variables.top()[arg.name] = arg.type;
    for (const auto& stmt : cm.block) analyzeStmt(*stmt);

    variables.pop();
    functionReturnTypes.pop();
}

void SemanticAnalyzer::analyzeTraitDeclStmt(AST::TraitDeclStmt& tds)
{
    if (traits.find(tds.name) != traits.end()) throw std::runtime_error("Trait '" + tds.name + "' already declared");

    TraitInfo traitInfo;
    
    for (auto& method : tds.methods)
        if (auto traitMethod = dynamic_cast<AST::TraitMethodMember*>(method.get()))
            analyzeTraitMethodMember(tds.name, traitInfo.methods, *traitMethod);
    
    traits[tds.name] = std::move(traitInfo);
}

void SemanticAnalyzer::analyzeTraitImplStmt(AST::TraitImplStmt& tis)
{
    if (traits.find(tis.traitName) == traits.end()) throw std::runtime_error("Trait '" + tis.traitName + "' not found");
    
    bool isBuiltinType = false;
    for (auto& typeValue : {TypeValue::INT, TypeValue::FLOAT, TypeValue::DOUBLE, TypeValue::CHAR, TypeValue::BOOL, TypeValue::STRING})
    {
        if (tis.className == typeToString(Type(typeValue, "")))
        {
            isBuiltinType = true;
            break;
        }
    }
    
    if (!isBuiltinType && classes.find(tis.className) == classes.end()) throw std::runtime_error("Type '" + tis.className + "' not found");
    
    TraitInfo& traitInfo = traits[tis.traitName];
    TraitImplInfo implInfo;
    implInfo.traitName = tis.traitName;
    implInfo.className = tis.className;
    
    for (const auto& traitMethod : traitInfo.methods)
    {
        bool found = false;
        for (auto& impl : tis.implementations)
        {
            if (auto method = dynamic_cast<AST::MethodMember*>(impl.get()))
            {
                if (method->name == traitMethod->name)
                {
                    if (method->retType != traitMethod->retType) throw std::runtime_error("Method '" + method->name + "' return type mismatch in trait implementation");
                    
                    if (method->args.size() != traitMethod->args.size()) throw std::runtime_error("Method '" + method->name + "' argument count mismatch in trait implementation");
                    
                    for (size_t i = 0; i < method->args.size(); i++)
                        if (method->args[i].type != traitMethod->args[i].type)
                            throw std::runtime_error("Method '" + method->name + "' argument type mismatch in trait implementation");
                    
                    AST::Block blockCopy;
                    for (const auto& stmt : method->block) blockCopy.push_back(std::unique_ptr<AST::Stmt>(stmt->clone()));
                    
                    implInfo.implementations.push_back(std::make_unique<AST::MethodMember>(method->access, method->name, method->retType, method->args, std::move(blockCopy)));
                    found = true;

                    break;
                }
            }
        }
        
        if (!found) throw std::runtime_error("Trait method '" + traitMethod->name + "' not implemented for class '" + tis.className + "'");
    }
    
    traitImplementations[tis.className].push_back(std::move(implInfo));
    
    if (!isBuiltinType)
    {
        auto classIt = classes.find(tis.className);
        if (classIt != classes.end())
        {
            for (const auto& impl : tis.implementations)
            {
                if (auto method = dynamic_cast<AST::MethodMember*>(impl.get()))
                {
                    bool exists = false;
                    for (const auto& existingMethod : classIt->second.methods)
                    {
                        if (existingMethod->name == method->name)
                        {
                            exists = true;
                            break;
                        }
                    }
                    
                    if (!exists)
                    {
                        AST::Block blockCopy;
                        for (const auto& stmt : method->block) blockCopy.push_back(std::unique_ptr<AST::Stmt>(stmt->clone()));
                        
                        classIt->second.methods.push_back(std::make_unique<AST::MethodMember>(method->access, method->name, method->retType, method->args, std::move(blockCopy)));
                    }
                }
            }
        }
    }
}

void SemanticAnalyzer::analyzeTraitMethodMember(std::string traitName, std::vector<std::unique_ptr<AST::TraitMethodMember>>& members, AST::TraitMethodMember& method)
{
    for (const auto& existingMethod : members)
        if (existingMethod->name == method.name)
            throw std::runtime_error("Method '" + method.name + "' already declared in trait '" + traitName + "'");
    
    members.push_back(std::make_unique<AST::TraitMethodMember>(method.access, method.name, method.retType, method.args));
}

Type SemanticAnalyzer::analyzeExpr(const AST::Expr& expr)
{
    if (auto lit = dynamic_cast<const AST::Literal*>(&expr)) return analyzeLiteral(*lit);
    else if (auto arrayLit = dynamic_cast<const AST::ArrayLiteral*>(&expr)) return analyzeArrayLiteral(*arrayLit);
    else if (auto binary = dynamic_cast<const AST::BinaryExpr*>(&expr)) return analyzeBinaryExpr(*binary);
    else if (auto unary = dynamic_cast<const AST::UnaryExpr*>(&expr)) return analyzeUnaryExpr(*unary);
    else if (auto var = dynamic_cast<const AST::VarExpr*>(&expr)) return analizeVarExpr(*var);
    else if (auto arrayAccess = dynamic_cast<const AST::ArrayExpr*>(&expr)) return analyzeArrayExpr(*arrayAccess);
    else if (auto func = dynamic_cast<const AST::FuncCallExpr*>(&expr)) return analyzeFuncCallExpr(*func);
    else if (auto newExpr = dynamic_cast<const AST::NewExpr*>(&expr)) return analyzeNewExpr(*newExpr);
    else if (auto field = dynamic_cast<const AST::FieldAccessExpr*>(&expr)) return analyzeFieldAccessExpr(*field);
    else if (auto method = dynamic_cast<const AST::MethodCallExpr*>(&expr)) return analyzeMethodCallExpr(*method);
    else if (auto thisExpr = dynamic_cast<const AST::ThisExpr*>(&expr)) return analyzeThisExpr(*thisExpr);
    
    throw std::runtime_error("Unknown expression type in semantic analysis");
}

Type SemanticAnalyzer::analyzeBinaryExpr(const AST::BinaryExpr& expr)
{
    Type leftType = analyzeExpr(*expr.left);
    Type rightType = analyzeExpr(*expr.right);

    switch (expr.op)
    {
        case TokenType::PLUS:
            if (leftType.type == TypeValue::STRING && rightType.type == TypeValue::STRING)
                return Type(TypeValue::STRING, "string");
            if (!canImplicitlyCast(leftType, rightType) && !canImplicitlyCast(rightType, leftType)) throw std::runtime_error("Type error: " + typeToString(leftType) + " and " + typeToString(rightType));

            return leftType;
        case TokenType::MINUS:
        case TokenType::MULTIPLY:
        case TokenType::DIVIDE:
        case TokenType::MODULO:
            if (!canImplicitlyCast(leftType, rightType) && !canImplicitlyCast(rightType, leftType)) throw std::runtime_error("Type error: " + typeToString(leftType) + " and " + typeToString(rightType));

            return leftType;
        
        case TokenType::EQUALS:
        case TokenType::NOT_EQUALS:
        case TokenType::GREATER:
        case TokenType::GREATER_EQUALS:
        case TokenType::LESS:
        case TokenType::LESS_EQUALS:
            if (!canImplicitlyCast(leftType, rightType) && !canImplicitlyCast(rightType, leftType)) throw std::runtime_error("Type error: " + typeToString(leftType) + " and " + typeToString(rightType));
            
            return Type(TypeValue::BOOL, "bool");

        case TokenType::AND:
        case TokenType::OR:
            if (leftType.type != TypeValue::BOOL || rightType.type != TypeValue::BOOL) throw std::runtime_error("Logical operations require boolean operands");
            
            return Type(TypeValue::BOOL, "bool");
        default: throw std::runtime_error("Unsupported binary operator in semantic analysis");
    }
}

Type SemanticAnalyzer::analyzeUnaryExpr(const AST::UnaryExpr& expr)
{
    Type operandType = analyzeExpr(*expr.expr);
    
    switch (expr.op)
    {
        case TokenType::MINUS:
            if (operandType.type != TypeValue::INT && operandType.type != TypeValue::FLOAT && operandType.type != TypeValue::DOUBLE) throw std::runtime_error("Unary minus requires numeric operand");

            return operandType;
            
        case TokenType::NOT:
            if (operandType.type != TypeValue::BOOL) throw std::runtime_error("Logical NOT requires boolean operand");

            return Type(TypeValue::BOOL, "bool");
            
        default: throw std::runtime_error("Unsupported unary operator in semantic analysis");
    }
}

Type SemanticAnalyzer::analyzeLiteral(const AST::Literal& literal)
{
    return literal.type;
}

Type SemanticAnalyzer::analyzeArrayLiteral(const AST::ArrayLiteral& literal)
{
    if (literal.elements.empty()) throw std::runtime_error("Array literal cannot be empty");
    
    Type firstElementType = analyzeExpr(*literal.elements[0]);
    
    for (size_t i = 1; i < literal.elements.size(); ++i)
    {
        Type elementType = analyzeExpr(*literal.elements[i]);
        if (!canImplicitlyCast(elementType, firstElementType) && !canImplicitlyCast(firstElementType, elementType))
            throw std::runtime_error("All array elements must have the same type");
    }
    
    return Type::createArrayType(std::make_shared<Type>(firstElementType));
}

Type SemanticAnalyzer::analyzeArrayExpr(const AST::ArrayExpr& expr)
{
    auto copy = variables;
    Type arrayType;
    bool found = false;
    
    while (!copy.empty())
    {
        const auto& scope = copy.top();
        auto it = scope.find(expr.name);
        if (it != scope.end())
        {
            arrayType = it->second;
            found = true;
            break;
        }

        copy.pop();
    }

    if (!found)
    {
        if (!classesStack.empty())
        {
            std::string className = classesStack.top();
            auto classIt = classes.find(className);
            if (classIt != classes.end())
            {
                for (const auto& field : classIt->second.fields)
                {
                    if (field->name == expr.name)
                    {
                        arrayType = field->type;
                        found = true;
                        break;
                    }
                }
            }
        }
    }
    
    if (!found) throw std::runtime_error("Array variable '" + expr.name + "' does not exist");

    if (arrayType.type != TypeValue::ARRAY) throw std::runtime_error("Variable '" + expr.name + "' is not an array");

    Type indexType = analyzeExpr(*expr.index);
    if (indexType.type != TypeValue::INT) throw std::runtime_error("Array index must be of type int");

    return *arrayType.elementType;
}

Type SemanticAnalyzer::analizeVarExpr(const AST::VarExpr& expr)
{
    auto copy = variables;
    while (!copy.empty())
    {
        const auto& scope = copy.top();
        auto it = scope.find(expr.name);
        if (it != scope.end()) return it->second;

        copy.pop();
    }

    if (!classesStack.empty())
    {
        std::string className = classesStack.top();
        auto classIt = classes.find(className);

        if (classIt != classes.end())
            for (const auto& field : classIt->second.fields)
                if (field->name == expr.name)
                    return field->type;
    }
    
    throw std::runtime_error("Variable '" + expr.name + "' does not exists");
}

Type SemanticAnalyzer::analyzeFuncCallExpr(const AST::FuncCallExpr& expr)
{
    std::vector<Type> argTypes;
    argTypes.reserve(expr.args.size());
    for (const auto& a : expr.args) argTypes.push_back(analyzeExpr(*a));

    auto it = functions.find(expr.name);
    if (it == functions.end())
    {
        if (!classesStack.empty())
        {
            std::string className = classesStack.top();
            auto classIt = classes.find(className);
            if (classIt != classes.end())
            {
                for (const auto& method : classIt->second.methods)
                {
                    if (method->name != expr.name) continue;
                    if (method->args.size() != argTypes.size()) continue;

                    bool ok = true;
                    for (size_t i = 0; i < argTypes.size(); i++)
                        if (!canImplicitlyCast(argTypes[i], method->args[i].type)) { ok = false; break; }

                    if (ok) return method->retType;
                }
            }
        }

        throw std::runtime_error("Function '" + expr.name + "' does not exists");
    }

    const auto& overloads = it->second;
    for (const auto& fn : overloads)
    {
        if (fn.args.size() != argTypes.size()) continue;
        
        bool ok = true;
        for (size_t i = 0; i < argTypes.size(); i++)
            if (!canImplicitlyCast(argTypes[i], fn.args[i].type)) { ok = false; break; }

        if (ok) return fn.returnType;
    }

    std::string argsStr;
    for (size_t i = 0; i < argTypes.size(); i++)
    {
        argsStr.append(typeToString(argTypes[i]));
        if (i < argTypes.size() - 1) argsStr.append(", ");
    }

    throw std::runtime_error("No matching function for call '" + expr.name + "(" + argsStr + ")'");
}

Type SemanticAnalyzer::analyzeNewExpr(const AST::NewExpr& expr)
{
    auto classIt = classes.find(expr.name);
    if (classIt == classes.end()) throw std::runtime_error("Object '" + expr.name + "' does not exists");

    if (expr.name != variablesTypes.top().name) throw std::runtime_error("Type error: Cannot implicitly cast " + expr.name + " to " + variablesTypes.top().name);

    std::vector<Type> argTypes;
    argTypes.reserve(expr.args.size());
    for (const auto& a : expr.args) argTypes.push_back(analyzeExpr(*a));

    const auto& constructors = classIt->second.constructors;
    bool insideSameClass = !classesStack.empty() && classesStack.top() == expr.name;

    bool found = false;
    bool publicAccess = false;

    if (!constructors.empty())
    {
        for (const auto& constructor : constructors)
        {
            if (constructor->args.size() != argTypes.size()) continue;

            bool ok = true;
            for (size_t i = 0; i < argTypes.size(); i++)
                if (!canImplicitlyCast(argTypes[i], constructor->args[i].type)) { ok = false; break; }

            if (!ok) continue;

            found = true;
            if (constructor->access == AST::AccessModifier::PUBLIC) publicAccess = true;

            break;
        }
        
        if (!found) throw std::runtime_error("No matching constructor for '" + expr.name + "'");
        if (!insideSameClass && !publicAccess) throw std::runtime_error("Constructor for '" + expr.name + "' is not public");
    }
    else if (!argTypes.empty()) throw std::runtime_error("No matching constructor for '" + expr.name + "'");

    return Type(TypeValue::CLASS, expr.name);
}

Type SemanticAnalyzer::analyzeFieldAccessExpr(const AST::FieldAccessExpr& expr)
{
    Type objectType = analyzeExpr(*expr.object);
    if (objectType.type != TypeValue::CLASS) throw std::runtime_error("Field access on non-class type");

    std::string className = objectType.name;
    auto classIt = classes.find(className);
    if (classIt == classes.end()) throw std::runtime_error("Class '" + className + "' not found");

    for (std::unique_ptr<AST::FieldMember>& field : classIt->second.fields)
        if (field->name == expr.name)
            return field->type;
    
    throw std::runtime_error("Field '" + expr.name + "' does not exists in class '" + className + "'");
}

Type SemanticAnalyzer::analyzeMethodCallExpr(const AST::MethodCallExpr& expr)
{
    Type objectType = analyzeExpr(*expr.object);
    
    bool isBuiltinType = false;
    for (auto& typeValue : {TypeValue::INT, TypeValue::FLOAT, TypeValue::DOUBLE, TypeValue::CHAR, TypeValue::BOOL, TypeValue::STRING})
    {
        if (objectType.type == typeValue)
        {
            isBuiltinType = true;
            break;
        }
    }
    
    if (isBuiltinType)
    {
        auto traitImplIt = traitImplementations.find(objectType.name);
        if (traitImplIt != traitImplementations.end())
        {
            std::vector<Type> argTypes;
            argTypes.reserve(expr.args.size());
            for (const auto& a : expr.args) argTypes.push_back(analyzeExpr(*a));
            
            for (const auto& implInfo : traitImplIt->second)
            {
                for (const auto& impl : implInfo.implementations)
                {
                    if (auto method = dynamic_cast<const AST::MethodMember*>(impl.get()))
                    {
                        if (method->name != expr.name) continue;
                        if (method->args.size() != argTypes.size()) continue;

                        bool ok = true;
                        for (size_t i = 0; i < argTypes.size(); i++) if (!canImplicitlyCast(argTypes[i], method->args[i].type)) { ok = false; break; }

                        if (ok) return method->retType;
                    }
                }
            }
        }

        throw std::runtime_error("No matching method '" + expr.name + "' found for builtin type '" + objectType.name + "'");
    }
    
    // Обрабатываем вызовы методов у трейтов
    if (objectType.type == TypeValue::TRAIT)
    {
        std::string traitName = objectType.name;
        auto traitIt = traits.find(traitName);
        if (traitIt == traits.end()) throw std::runtime_error("Trait '" + traitName + "' not found");

        std::vector<Type> argTypes;
        argTypes.reserve(expr.args.size());
        for (const auto& a : expr.args) argTypes.push_back(analyzeExpr(*a));

        for (const auto& method : traitIt->second.methods)
        {
            if (method->name != expr.name) continue;
            if (method->args.size() != argTypes.size()) continue;

            bool ok = true;
            for (size_t i = 0; i < argTypes.size(); i++)
                if (!canImplicitlyCast(argTypes[i], method->args[i].type)) { ok = false; break; }

            if (ok) return method->retType;
        }

        throw std::runtime_error("No matching method '" + expr.name + "' found in trait '" + traitName + "'");
    }
    
    if (objectType.type != TypeValue::CLASS) throw std::runtime_error("Method call on non-class type");

    std::string className = objectType.name;
    auto classIt = classes.find(className);
    if (classIt == classes.end()) throw std::runtime_error("Class '" + className + "' not found");

    std::vector<Type> argTypes;
    argTypes.reserve(expr.args.size());
    for (const auto& a : expr.args) argTypes.push_back(analyzeExpr(*a));

    for (const auto& method : classIt->second.methods)
    {
        if (method->name != expr.name) continue;
        if (method->args.size() != argTypes.size()) continue;

        bool ok = true;
        for (size_t i = 0; i < argTypes.size(); i++)
            if (!canImplicitlyCast(argTypes[i], method->args[i].type)) { ok = false; break; }

        if (ok) return method->retType;
    }

    throw std::runtime_error("No matching method '" + expr.name + "' found in class '" + className + "'");
}

Type SemanticAnalyzer::analyzeThisExpr(const AST::ThisExpr& expr)
{
    if (classesStack.empty()) throw std::runtime_error("'this' used outside of class method");
    
    std::string typeName = classesStack.top();
    
    for (auto& typeValue : {TypeValue::INT, TypeValue::FLOAT, TypeValue::DOUBLE, TypeValue::CHAR, TypeValue::BOOL, TypeValue::STRING})
        if (typeName == typeToString(Type(typeValue, "")))
            return Type(typeValue, typeName);
    
    return Type(TypeValue::CLASS, typeName);
}

bool SemanticAnalyzer::canImplicitlyCast(Type l, Type r)
{
    if (l == r) return true;

    if (l.type == TypeValue::CLASS && r.type == TypeValue::CLASS) return l.name == r.name;
    if (l.type == TypeValue::TRAIT && r.type == TypeValue::TRAIT) return l.name == r.name;
    
    if (l.type == TypeValue::CLASS && r.type == TypeValue::TRAIT)
    {
        auto traitImplIt = traitImplementations.find(l.name);
        if (traitImplIt != traitImplementations.end())
            for (const auto& implInfo : traitImplIt->second)
                if (implInfo.traitName == r.name) return true;

        return false;
    }
    
    if (l.type == TypeValue::TRAIT && r.type == TypeValue::CLASS) return false;

    if (castsTable.find(l.type) != castsTable.end())
        if (std::find(castsTable[l.type].begin(), castsTable[l.type].end(), r.type) != castsTable[l.type].end())
            return true;
    
    if (castsTable.find(r.type) != castsTable.end())
        if (std::find(castsTable[r.type].begin(), castsTable[r.type].end(), l.type) != castsTable[r.type].end())
            return true;

    return false;
}

bool SemanticAnalyzer::isConstExpr(const AST::Expr& expr) const
{
    if (dynamic_cast<const AST::Literal*>(&expr)) return true;
    if (auto bin = dynamic_cast<const AST::BinaryExpr*>(&expr)) return isConstExpr(*bin->left) && isConstExpr(*bin->right);
    if (auto un = dynamic_cast<const AST::UnaryExpr*>(&expr)) return isConstExpr(*un->expr);
    
    return false;
}

std::string typeToString(Type type)
{
    switch (type.type)
    {
        case TypeValue::INT: return "int";
        case TypeValue::FLOAT: return "float";
        case TypeValue::DOUBLE: return "double";
        case TypeValue::CHAR: return "char";
        case TypeValue::BOOL: return "bool";
        case TypeValue::STRING: return "string";
        case TypeValue::VOID: return "void";
        case TypeValue::CLASS: return "class <" + type.name + ">";
        case TypeValue::TRAIT: return "trait <" + type.name + ">";
        case TypeValue::ARRAY: 
            if (type.elementType) return typeToString(*type.elementType) + "[]";
            
            return "array[]";
        default: return "unknown";
    }
}

std::string SemanticAnalyzer::mangleFunction(const std::string& name, const AST::Arguments& args) const
{
    std::vector<Type> types; types.reserve(args.size());
    for (const auto& a : args) types.push_back(a.type);

    return mangleFunction(name, types);
}

std::string SemanticAnalyzer::mangleFunction(const std::string& name, const std::vector<Type>& types) const
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
            case TypeValue::CLASS: mangled += "C" + t.name; break;
            default: mangled += "u"; break;
        }

        if (i + 1 < types.size()) mangled += "_";
    }

    return mangled;
}