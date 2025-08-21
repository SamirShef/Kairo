# StageLang
**Stge** - multi-paradigm programming language

Compiler version: **v0.1** – C++ &amp; LLVM

## Installation (Linux)

### 1) Install dependencies
- LLVM (dev headers and libs), Clang, CMake, C++ compiler, optionally Ninja
- Debian/Ubuntu:
  ```bash
  sudo apt update
  sudo apt install -y llvm-dev clang cmake build-essential ninja-build
  ```
- Arch/Manjaro:
  ```bash
  sudo pacman -S --needed llvm clang cmake ninja
  ```
- Fedora:
  ```bash
  sudo dnf install -y llvm-devel clang cmake gcc-c++ ninja-build
  ```

### 2) Clone the repository
```bash
git clone https://github.com/SamirShef/StageLang.git
cd StageLang
```

### 3) Configure the build
Use Ninja if available (faster), otherwise omit `-G Ninja` to use your default generator.
```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
```
If CMake cannot find LLVM, point it to your LLVM package config:
```bash
# Common paths (pick the one that exists on your system)
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_DIR=/usr/lib/cmake/llvm
# or on Ubuntu (versioned):
# -DLLVM_DIR=/usr/lib/llvm-20/lib/cmake/llvm
```

### 4) Build
```bash
cmake --build build -j"$(nproc)"
```

### 5) Install
- User-local (recommended): installs `stc` into `~/.local/bin`
  ```bash
  cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$HOME/.local"
  cmake --build build -j"$(nproc)"
  cmake --install build
  ```
- System-wide (optional): installs into `/usr/local/bin`
  ```bash
  cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local
  cmake --build build -j"$(nproc)"
  sudo cmake --install build
  ```

### 6) Add to PATH (zsh)
If `~/.local/bin` is not in your PATH:
```bash
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

### 7) Verify
```bash
command -v stc
stc examples/helloworld.st ./hello && ./hello
```

### 8) Uninstall
Keep the `build` directory after installation. To uninstall later:
```bash
# User-local uninstall
xargs -a build/install_manifest.txt -r rm -vf

# System-wide uninstall
sudo xargs -a build/install_manifest.txt -r rm -vf
```

### Troubleshooting
- LLVM not found: pass the correct `-DLLVM_DIR=...` to CMake (see step 3). On Arch it is usually `/usr/lib/cmake/llvm`; on Ubuntu it can be `/usr/lib/llvm-20/lib/cmake/llvm`.
- Linker issues: ensure `clang` is installed and in PATH. You can override the linker used by the compiler with `STC_LINKER=clang`.
- PATH not updated: after editing `~/.zshrc`, run `source ~/.zshrc` or restart the shell.

## Types
List of _primitive_ types:
* int _(32 bits)_
* float _(32 bits)_
* double _(64 bits)_
* char _(8 bits)_
* bool
* void (for return type in functions)

## Literals
* int literal - some integer number (for example `123456`)
* float literal - some floating number with suffix `f` (for example `123.456f`)
* double literal - some floating number with suffix `d` (for example `123.456d`)
* char literal - some character between `'` (for example `'A'`, `'!'`)
* bool literal - `true` or `false`
* string literal - some string text between `"` (for example `"Hello world"`)

## Syntax
List of statements:
* [Global variables definition](#global-variables-definition)
* [Function definition](#function-definition)
* [Local variables definition](#local-variables-definition)
* [Echo](#echo-statement)

> [!NOTE]
> In the end all statements you must be add `;` character.

## Global Variables Definition
For global variable definition you need use the keyword `var`, type, identifier and initializer. For example:
```C++
var int test1 = 10;
var bool test2 = true;
```

> [!WARNING]
> Global variables must be initialized by constant expression.

## Function Definition
For functions definition you need use the keyword `func`, type, identifier, declaration arguments between round brackets and block between braces. For example:
```C++
func void test()
{
    // statements
}
```

In the function you can create `return` statement, for return any value (or without value in void-typed function). For example:
```C++
func int main()
{
    return 0;  // whith expression because function type is not void
}

func void test()
{
    return;    // whithout expression because function type is void
}
```

> [!TIP]
> If your function type is void, you can skip type in definition. For example:
> ```C++
> func test()    // have type void
> {
>     // statements
> }
> ```

> [!NOTE]
> Inside any block creating a new variables scope

> [!WARNING]
> A function named `main` is the entry point to the project. If you forgot to create a definition for this function, the compiler will not compile your code into an executable file. The type of this function can be any.

## Local Variables Definition
Local variables definition like global, but you can initialized but they can be initialized not only by constant expressions. After end block of statements local variables will be deleted.

## Echo Statement
Echo statement can write any value in console. For use echo you need use keyword `echo` and some expression. For example:
```C++
func main()
{
    echo "Hello world!";
    echo 10 + 2 * (2 + 312);
    return 0;
}
```
