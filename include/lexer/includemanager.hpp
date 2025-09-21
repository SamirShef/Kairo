#include "../parser/ast.hpp"
#include <unordered_set>
#include <unordered_map>
#include <vector>

extern std::unordered_set<std::string> includedFiles;
extern std::unordered_map<std::string, std::vector<AST::StmtPtr>> parsedFilesCache;

std::string resolveIncludePath(const std::string& includePath, const std::string& currentFilePath, const std::string& libsPath = "");
std::vector<AST::StmtPtr> parseIncludeFile(std::string filePath, std::string currentFilePath = "", std::string libsPath = "");
void clearIncludeCache();