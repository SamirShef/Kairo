#include "../include/lexer/lexer.hpp"
#include "../include/parser/parser.hpp"
#include "../include/codegen/codegenerator.hpp"
#include "../include/semantic/semanticanalyzer.hpp"
#include <iostream>
#include <fstream>
#include <map>
#include <string>
#include <vector>
#include <cstdlib>

#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/IR/LegacyPassManager.h"

#include <array>
#include <cstdio>
#include <memory>

std::map<std::string, Value> variables;

int main(int argc, char** argv)
{
    if (argc != 3)
    {
        std::cerr << "Usage: stc <source_file>.st <executable_name>" << std::endl;
        return 1;
    }
    const std::string sourcePath = argv[1];
    std::string executablePath = argv[2];
    
    #if defined(_WIN32)
    const char* objExt = ".obj";
    const char* exeExt = ".exe";
    #else
    const char* objExt = ".o";
    const char* exeExt = "";
    #endif
    
    #if defined(_WIN32)
    if (executablePath.size() < 4 || executablePath.substr(executablePath.size() - 4) != ".exe")
        executablePath += exeExt;
    #endif
    const std::string objectPath = executablePath + objExt;

    std::fstream sourceFile(sourcePath);

    if (!sourceFile.is_open())
    {
        std::cerr << "Error opening file!" << std::endl;
        return 1;
    }

    std::string fileContent( (std::istreambuf_iterator<char>(sourceFile)), (std::istreambuf_iterator<char>()));
    Lexer lexer = Lexer(fileContent);
    std::vector<Token> tokens = lexer.tokenize();
    
    //std::cout << "\n=======TOKENS======\n" << std::endl;

    Parser parser = Parser(tokens);
    std::vector<AST::StmtPtr> stmts = parser.parse();

    SemanticAnalyzer analyzer;
    analyzer.analyze(stmts);

    CodeGenerator codegen("stage_lang_compiler");
    codegen.generate(stmts);
    
    /* std::cout << "\n======LLVM IR======\n" << std::endl;

    codegen.printIR(); */
    std::cerr << std::flush; std::cout << std::flush;

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::unique_ptr<llvm::Module> module = codegen.getModule();
    
    if (module->getFunction("main") == nullptr)
    {
        std::cerr << "Program does not have entry point 'main'" << std::endl;
    }
    auto getTriple = []() -> std::string
    {
        const char* envTriple = std::getenv("STC_TRIPLE");
        if (envTriple && *envTriple) return std::string(envTriple);

        std::array<char, 256> buffer{};
        std::string result;
        #if defined(_WIN32)
        std::unique_ptr<FILE, int(*)(FILE*)> pipe(_popen("clang -dumpmachine", "r"), _pclose);
        #else
        std::unique_ptr<FILE, int(*)(FILE*)> pipe(popen("clang -dumpmachine", "r"), pclose);
        #endif
        if (!pipe) return "";
        while (fgets(buffer.data(), static_cast<int>(buffer.size()), pipe.get()) != nullptr) result += buffer.data();
        while (!result.empty() && (result.back() == '\n' || result.back() == '\r')) result.pop_back();
        return result;
    };
    auto defaultTripleForHost = []() -> std::string
    {
        #if defined(_WIN32)
            #if defined(__aarch64__) || defined(_M_ARM64)
                return "aarch64-pc-windows-msvc";
            #else
                return "x86_64-pc-windows-msvc";
            #endif
        #elif defined(__APPLE__)
            #if defined(__aarch64__)
                return "arm64-apple-darwin";
            #else
                return "x86_64-apple-darwin";
            #endif
        #else
            #if defined(__aarch64__)
                return "aarch64-unknown-linux-gnu";
            #else
                return "x86_64-pc-linux-gnu";
            #endif
        #endif
    };
    std::string targetTriple = getTriple();
    if (targetTriple.empty()) targetTriple = defaultTripleForHost();
    module->setTargetTriple(targetTriple);

    std::string error;
    const llvm::Target* target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
    if (!target)
    {
        std::cerr << error << std::endl;
        return 1;
    }

    std::string CPU = "generic";
    std::string Features = "";
    llvm::TargetOptions opt;
    auto relocModel = std::optional<llvm::Reloc::Model>();
    std::unique_ptr<llvm::TargetMachine> targetMachine(target->createTargetMachine(targetTriple, CPU, Features, opt, relocModel));
    if (!targetMachine)
    {
        std::cerr << "Failed to create TargetMachine for triple '" << targetTriple << "'" << std::endl;
        return 1;
    }

    module->setDataLayout(targetMachine->createDataLayout());

    std::error_code ec;
    llvm::raw_fd_ostream dest(objectPath, ec, llvm::sys::fs::OF_None);
    if (ec)
    {
        std::cerr << "Could not open file '" << objectPath << "': " << ec.message() << std::endl;
        return 1;
    }

    llvm::legacy::PassManager pass;
    
    auto fileType = static_cast<llvm::CodeGenFileType>(1); // 1 = Object file
    if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType))
    {
        std::cerr << "TargetMachine can't emit a file of this type" << std::endl;
        return 1;
    }

    pass.run(*module);
    dest.flush();
    dest.close();

    const char* envLinker = std::getenv("STC_LINKER");
    std::string linker = envLinker ? std::string(envLinker) : std::string("clang");
    #if defined(_WIN32)
    std::string linkCmd = linker + std::string(" ") + std::string("\"") + objectPath + std::string("\"") + " -o " + std::string("\"") + executablePath + std::string("\"") + " -fuse-ld=lld";
    #elif defined(__APPLE__)
    std::string linkCmd = linker + std::string(" ") + std::string("\"") + objectPath + std::string("\"") + " -o " + std::string("\"") + executablePath + std::string("\"");
    #else
    std::string linkCmd = linker + std::string(" ") + std::string("\"") + objectPath + std::string("\"") + " -o " + std::string("\"") + executablePath + std::string("\"") + " -no-pie";
    #endif
    auto runAndCapture = [](const std::string& cmd) -> std::pair<int, std::string>
    {
        std::string output;
        #if defined(_WIN32)
        std::unique_ptr<FILE, int(*)(FILE*)> pipe(_popen(cmd.c_str(), "r"), _pclose);
        #else
        std::unique_ptr<FILE, int(*)(FILE*)> pipe(popen(cmd.c_str(), "r"), pclose);
        #endif
        if (!pipe) return { -1, std::string("Failed to spawn: ") + cmd };
        std::array<char, 512> buf{};
        while (fgets(buf.data(), static_cast<int>(buf.size()), pipe.get()) != nullptr) output += buf.data();
        int code = 0;
        #if defined(_WIN32)
        code = _pclose(pipe.release());
        #else
        code = pclose(pipe.release());
        #endif
        return { code, output };
    };

    auto [linkRes, linkOut] = runAndCapture(linkCmd);
    if (linkRes != 0)
    {
        std::cerr << "Link command: " << linkCmd << std::endl;
        std::cerr << linkOut << std::endl;
        std::cerr << "Linking failed with code " << linkRes << std::endl;
        return 1;
    }

    std::cout << "Built executable: " << executablePath << std::endl;
    
    if (std::remove(objectPath.c_str()) != 0) std::cerr << "Warning: Failed to remove object file: " << objectPath << std::endl;
    
    return 0;
}