#include "../../include/lexer/includemanager.hpp"
#include "../../include/parser/parser.hpp"
#include "../../include/lexer/lexer.hpp"
#include <filesystem>
#include <stdexcept>
#include <fstream>
#include <string>
#include <vector>

std::unordered_set<std::string> includedFiles;
std::unordered_map<std::string, std::vector<AST::StmtPtr>> parsedFilesCache;

std::string resolveIncludePath(const std::string& includePath, const std::string& currentFilePath, const std::string& libsPath)
{
    if (std::filesystem::path(includePath).is_absolute()) return includePath;
    
    if (!libsPath.empty())
    {
        std::filesystem::path libsFullPath = std::filesystem::path(libsPath) / includePath;
        try
        {
            if (std::filesystem::exists(libsFullPath)) return std::filesystem::canonical(libsFullPath).string();
        }
        catch (const std::filesystem::filesystem_error&) {}
    }
    
    if (!currentFilePath.empty())
    {
        std::filesystem::path currentDir = std::filesystem::path(currentFilePath).parent_path();
        std::filesystem::path fullPath = currentDir / includePath;
        
        try
        {
            if (std::filesystem::exists(fullPath)) return std::filesystem::canonical(fullPath).string();
        }
        catch (const std::filesystem::filesystem_error&) {}
    }
    
    return includePath;
}

std::vector<AST::StmtPtr> parseIncludeFile(std::string filePath, std::string currentFilePath, std::string libsPath)
{
    std::string resolvedPath = resolveIncludePath(filePath, currentFilePath, libsPath);
    
    if (includedFiles.find(resolvedPath) != includedFiles.end()) return {};
    
    includedFiles.insert(resolvedPath);
    
    auto cacheIt = parsedFilesCache.find(resolvedPath);
    if (cacheIt != parsedFilesCache.end())
    {
        std::vector<AST::StmtPtr> result;
        for (const auto& stmt : cacheIt->second) result.push_back(std::unique_ptr<AST::Stmt>(stmt->clone()));

        return result;
    }
    
    std::ifstream file(resolvedPath);
    if (!file.is_open()) throw std::runtime_error("Cannot open include file: " + resolvedPath);
    
    std::string content((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    file.close();
    
    Lexer lexer(content);
    std::vector<Token> tokens = lexer.tokenize();
    
    Parser parser(tokens, resolvedPath, libsPath);
    std::vector<AST::StmtPtr> statements = parser.parse();
    
    std::vector<AST::StmtPtr> cacheCopy;
    for (const auto& stmt : statements) cacheCopy.push_back(std::unique_ptr<AST::Stmt>(stmt->clone()));
    parsedFilesCache[resolvedPath] = std::move(cacheCopy);
    
    return statements;
}

void clearIncludeCache()
{
    includedFiles.clear();
    parsedFilesCache.clear();
}