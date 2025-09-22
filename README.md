# Kairo
**Kairo** - multi-paradigm programming language

Compiler version: **v0.7.1** – C++ &amp; LLVM

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
git clone https://github.com/SamirShef/Kairo.git
cd Kairo
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

> [!NOTE]
> The build process automatically copies the `libs` directory to the build folder. This means that after building, you can delete the source repository and the compiler will still work with the standard libraries, as they are now embedded in the build directory.

### 5) Install
- User-local (recommended): installs `krc` into `~/.local/bin` and libraries into `~/.local/share/kairo/libs`
  ```bash
  cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$HOME/.local"
  cmake --build build -j"$(nproc)"
  cmake --install build
  ```
- System-wide (optional): installs into `/usr/local/bin` and libraries into `/usr/local/share/kairo/libs`
  ```bash
  cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local
  cmake --build build -j"$(nproc)"
  sudo cmake --install build
  ```

> [!NOTE]
> The installation process installs both the compiler binary and the standard libraries. After installation, you can delete the source repository and the build directory, as the compiler will use the installed libraries.

### 6) Add to PATH (zsh)
If `~/.local/bin` is not in your PATH:
```bash
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

### 7) Verify
```bash
command -v krc
krc examples/hello_world.kr ./hello && ./hello
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
- Linker issues: ensure `clang` is installed and in PATH. You can override the linker used by the compiler with `KRC_LINKER=clang`.
- PATH not updated: after editing `~/.zshrc`, run `source ~/.zshrc` or restart the shell.

## Installation (Windows)

### 1) Install dependencies
- **LLVM**: Download and install LLVM from [llvm.org](https://llvm.org/releases/) or use [winget](https://github.com/microsoft/winget-cli):
  ```cmd
  winget install LLVM.LLVM
  ```
- **CMake**: Download from [cmake.org](https://cmake.org/download/) or use winget:
  ```cmd
  winget install Kitware.CMake
  ```
- **Visual Studio Build Tools**: Install [Visual Studio Build Tools](https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2022) with C++ workload, or use winget:
  ```cmd
  winget install Microsoft.VisualStudio.2022.BuildTools
  ```
- **Ninja** (optional, for faster builds): Download from [ninja-build.org](https://ninja-build.org/) or use winget:
  ```cmd
  winget install Ninja-build.Ninja
  ```

**Note**: The CI system builds LLVM from source using the provided `install.bat` and `build.bat` scripts. This approach ensures compatibility but takes longer than using pre-built packages.

**Additional dependencies for CI approach** (if using Option C in step 3):
- **7-Zip**: For extracting LLVM source archives
- **Perl**: For running patch scripts (usually comes with Git for Windows)
- **Git**: For cloning LLVM sources (if using master branch)
- **Disk space**: ~10GB free space for building LLVM from source

### 2) Clone the repository
```cmd
git clone https://github.com/SamirShef/Kairo.git
cd Kairo
```

### 3) Configure the build
Open **Developer Command Prompt for VS 2022** or **x64 Native Tools Command Prompt** and navigate to the project directory:

**Option A: Using Visual Studio generator (recommended)**
```cmd
cmake -S . -B build -G "Visual Studio 17 2022" -A x64 -DCMAKE_BUILD_TYPE=Release
```

**Option B: Using Ninja (faster builds)**
```cmd
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
```

**Option C: Using CI-compatible approach**
If you want to use the same approach as CI (building LLVM from source):
```cmd
# Set environment variables (similar to CI)
set BUILD_PROJECT=llvm
set LLVM_VERSION=20.1.0
set WORKING_DIR=%CD%\llvm-build

# Run the CI scripts
call set-env.bat msvc17 msvcrt amd64 Release
call install.bat
call build.bat
```

**Note**: If CMake cannot find LLVM, specify the LLVM installation path:
```cmd
cmake -S . -B build -G "Visual Studio 17 2022" -A x64 -DCMAKE_BUILD_TYPE=Release -DLLVM_DIR="C:\Program Files\LLVM\lib\cmake\llvm"
```

### 4) Build
```cmd
cmake --build build --config Release
```

> [!NOTE]
> The build process automatically copies the `libs` directory to the build folder. This means that after building, you can delete the source repository and the compiler will still work with the standard libraries, as they are now embedded in the build directory.

### 5) Install
- **User-local** (recommended): installs `krc.exe` into `%USERPROFILE%\AppData\Local\Programs\Kairo` and libraries into `%USERPROFILE%\AppData\Local\Programs\Kairo\share\kairo\libs`:
  ```cmd
  cmake -S . -B build -G "Visual Studio 17 2022" -A x64 -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="%USERPROFILE%\AppData\Local\Programs\Kairo"
  cmake --build build --config Release
  cmake --install build
  ```
- **System-wide** (optional): installs into `C:\Program Files\Kairo` and libraries into `C:\Program Files\Kairo\share\kairo\libs`:
  ```cmd
  cmake -S . -B build -G "Visual Studio 17 2022" -A x64 -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="C:\Program Files\Kairo"
  cmake --build build --config Release
  cmake --install build
  ```

> [!NOTE]
> The installation process installs both the compiler binary and the standard libraries. After installation, you can delete the source repository and the build directory, as the compiler will use the installed libraries.

**Alternative: CI-compatible installation**
If you used the CI approach (Option C in step 3), the LLVM will be installed locally and you can build Kairo directly:
```cmd
# After running CI scripts, build Kairo
cmake -S . -B build -G "Visual Studio 17 2022" -A x64 -DCMAKE_BUILD_TYPE=Release -DLLVM_DIR="%WORKING_DIR%\llvm\build\lib\cmake\llvm"
cmake --build build --config Release
cmake --install build --prefix="%USERPROFILE%\AppData\Local\Programs\Kairo"
```

### 6) Add to PATH
After installation, add the Kairo directory to your system PATH:

**Option A: User PATH (recommended)**
1. Press `Win + R`, type `sysdm.cpl`, press Enter
2. Go to **Advanced** tab → **Environment Variables**
3. Under **User variables**, find **Path** → **Edit**
4. Click **New** and add: `%USERPROFILE%\AppData\Local\Programs\Kairo`
5. Click **OK** on all dialogs

**Option B: System PATH**
1. Press `Win + R`, type `sysdm.cpl`, press Enter
2. Go to **Advanced** tab → **Environment Variables**
3. Under **System variables**, find **Path** → **Edit**
4. Click **New** and add: `C:\Program Files\Kairo`
5. Click **OK** on all dialogs

**Option C: Command line (temporary)**
```cmd
set PATH=%PATH%;%USERPROFILE%\AppData\Local\Programs\Kairo
```

### 7) Verify installation
Open a **new** Command Prompt or PowerShell and verify:
```cmd
krc --version
krc examples\hello_world.kr hello.exe
hello.exe
```

### 8) Uninstall
Keep the `build` directory after installation. To uninstall later:
```cmd
# User-local uninstall
for /f "tokens=*" %i in (build\install_manifest.txt) do del "%i"

# System-wide uninstall (run as Administrator)
for /f "tokens=*" %i in (build\install_manifest.txt) do del "%i"
```

### Troubleshooting (Windows)
- **LLVM not found**: Ensure LLVM is installed and specify the correct path with `-DLLVM_DIR`. Common paths:
  - `C:\Program Files\LLVM\lib\cmake\llvm`
  - `C:\Program Files (x86)\LLVM\lib\cmake\llvm`
- **Build tools not found**: Install Visual Studio Build Tools with C++ workload
- **PATH not updated**: Restart Command Prompt/PowerShell after adding to PATH
- **Permission denied**: Run Command Prompt as Administrator for system-wide installation
- **CI compatibility**: If you want to use the same approach as CI, ensure you have:
  - 7-Zip installed for extracting LLVM sources
  - Perl installed for running patch scripts
  - Git for cloning LLVM sources (if using master branch)
  - Sufficient disk space (~10GB) for building LLVM from source

## Types
List of _primitive_ types:
* int _(32 bits)_
* float _(32 bits)_
* double _(64 bits)_
* char _(8 bits)_
* string
* bool
* void (for return type in functions)

## Literals
* int literal - some integer number (for example `123456`)
* float literal - some floating number with suffix `f` (for example `123.456f`)
* double literal - some floating number with suffix `d` (for example `123.456d`)
* char literal - some character between `'` (for example `'A'`, `'!'`)
* bool literal - `true` or `false`
* string literal - some string text between `"` (for example `"Hello world"`)

## Operators
The language supports the following operators. All binary operators are left-associative. Operator precedence (from highest to lowest) follows the order listed below within each group (top binds tighter):

- Arithmetic:
  - Unary: `-x`, `x++`, `x--`
  - Multiplicative: `x * y`, `x / y`, `x % y`
  - Additive: `x + y`, `x - y`

- Comparisons (result type: `bool`):
  - `x == y`, `x != y`
  - `x > y`, `x >= y`, `x < y`, `x <= y`

- Logical (operands must be `bool`):
  - Unary: `!x`
  - Binary: `x || y`, `x && y`
  - Note: in this version, `||` binds tighter than `&&` (i.e., `a && b || c` is parsed as `a && (b || c)`). Use parentheses to make intent explicit.

- Assignment:
  - Simple: `x = expr;`
  - Compound: `x += expr;`, `x -= expr;`, `x *= expr;`, `x /= expr;`, `x %= expr;`

- Grouping and calls:
  - Grouping: `(expr)`
  - Function call: `name(arg1, arg2, ...)`

Type rules (summary):
- Arithmetic operators work on string and numeric types; implicit widening casts are allowed (`int -> float -> double`).
- Comparisons allow numeric comparisons and produce `bool`.
- Logical operators require `bool` operands and produce `bool`.
- Assignment (incl. compound) requires the right-hand side to be implicitly castable to the variable type.

## Escape-secuences
* `\\` - character `\`
* `\n` - move to next line
* `\t` - tab character
* `\r` - carriage return
* `\"` - character `"`
* `\'` - character `'`
* `\0` - null character
* `\a` - alert sound
* `\b` - represents the backspace character
* `\f` - form feed character
* `\v` - vertical tab character
* `\x` - hexadecimal number character
* `\u` - unicode character (ASCII)

## Syntax
List of statements:
* [Global variables definition](#global-variables-definition)
* [Function definition](#function-definition)
* [Local variables definition](#local-variables-definition)
* [Constants](#constants)
* [Comments](#comments)
* [If/Else statements](#ifelse-statements)
* [While loop](#while-loop)
* [Do/while loop](#dowhile-loop)
* [For loop](#for-loop)
* [Echo](#echo-statement)
* [Classes](#classes)
* [Arrays](#arrays)
* [Traits](#traits)
* [Include files](#include-files)
* [Sizeof](#sizeof-operator)

> [!NOTE]
> In the end all statements you must be add `;` character.

## Global Variables Definition
For global variable definition you need use the keyword `var`, type, identifier and initializer (if needed). For example:
```C++
var int test1 = 10;
var bool test2 = true;
var int test3;  // has value of 0
```

> [!WARNING]
> Global variables must be initialized by constant expression.

> [!NOTE]
> If you have not initialized the variable, its value will be the default value for its type.

## Function Definition
For functions definition you need use the keyword `func`, type, identifier, declaration arguments between round brackets and block between braces. There is support for function overloads. For example:
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
Local variables definition like global, but they can be initialized not only by constant expressions. After end block of statements local variables will be deleted. For example:
```C++
func int main()
{
    var int a = 10;
    var int b;  // has value of 0

    return 0;
}
```

## Constants
For constant definition you need use keyword `const`, type, name and initializer. For example:
```C++
const char const1 = 'A';

func int main()
{
    const int const2 = 10;
}
```

## Comments
Single-line comment:
```C++
var int a = 10;   // this is variable
```

Multi-line comment:
```C++
///func void test()
{

}///

func int main()
{
    var int a = 10;

    return 0;
}
```

## If/Else Statements
For if statement definition you need use the keyword `if`, conditional expression between round brackets and block of statements. If you need define else statement too, you need use the keyword `else` and block of statements. For example:

```C++
func int main()
{
    var int a = 10;
    var int b = 20;

    if (a > b)
    {
        a *= 2;
        echo a;
        echo '\n';
    }
    else
    {
        b /= a;
        echo b + a;
        echo '\n';
    }
    
    return 0;
}
```

> [!TIP]
> If block of statements in `if` or `else` branch have only 1 statement, you don't have to specify braces. For example:
> ```C++
> func int main()
> {
>     var int a = 10;
>     var int b = 20;
>
>     if (a == b) echo "a equals b\n";
>     else if (a < b) echo "a less then b\n";
>     else echo "a greater then b\n";
> 
>     return 0;
> }
> ```

## While Loop
For `while` loop definition you need use `while`, conditional expression between round brackets and block of statements. For example:
```C++
func int main()
{
    var int i = 0;

    while (i < 10)
    {
        echo i;
        echo '\n';
        i += 1;
    }

    return 0;
}
```

> [!TIP]
> If block of statements in `while` branch have only 1 statement, you don't have to specify braces. For example:
> ```C++
> func int main()
> {
>     while (true) echo "Hello world! This is infinity cycle!\n";
> 
>     return 0;
> }
> ```

## Do/while Loop
For `do/while` loop definition you need use `do`, block of statements, special operator `->` and conditional expression between round brackets. For example:
```C++
func int main()
{
    var int i = 0;

    do 
    {
        echo i;
        echo '\n';
        i += 1;
    } -> (i < 10);

    return 0;
}
```

> [!TIP]
> If block of statements in `do` branch have only 1 statement, you don't have to specify braces. For example:
> ```C++
> func int main()
> {
>     do echo "Hello world! This is infinity cycle!\n"; -> (true);
> 
>     return 0;
> }
> ```

## For Loop
For `for` loop definition you need use `for`, define or assign iterator variable, special operator `;`, conditional expression, special operator `;`, and assignment statement for iterator variable between round brackets and block of statements. For example:
```C++
func int main()
{
    for (int i = 0; i < 10; i += 1)
    {
        echo i;
        echo '\n';
    }

    var int i = 0;
    for (i = 10; i > 0; i /= 2)
    {
        echo i;
        echo '\n';
    }

    return 0;
}
```

> [!TIP]
> If block of statements in `for` branch have only 1 statement, you don't have to specify braces. For example:
> ```C++
> func int main()
> {
>     for (int i = 0; i < 10; i += 1) echo 1;
> 
>     return 0;
> }
> ```

## Echo Statement
Echo statement can write any value in console. For use echo you need use keyword `echo` and some expression (without moving to a new line). For example:
```C++
func int main()
{
    echo "Hello world!\n";
    echo 10 + 2 * (2 + 312);
    echo '\n';
    return 0;
}
```

## Classes
This language support a simple class declaration system. Classes have _members_, each member has its own _access modifier_ (pub/priv) that define a member's access outside the class. There are 3 types of member: fields, methods, constructors.

### Class definition
For class definition you need use keyword `class`, identifier and block of members. For example:
```C++
class Test
{
    // members
}
```

### Field definition
For field definition you need use access modifier, keyword `var`, type, identifier and initializer expression (if needed). For example:
```C++
class Person
{
    pub var string name;
    priv var int age = 15;
}
```

### Method definition
Methods are definition as functions, but with an access modifier at the beginning. For example:
```C++
class Car
{
    priv var int maxSpeed = 200;
   
    pub func void startEngine()
    {
        echo "Vroom vroom...\n";
    }

    pub func int getMaxSpeed()
    {
        return maxSpeed;
    }

    pub func void setMaxSpeed(int speed)
    {
        maxSpeed = speed;
    }
}
```

### Constructor definition
For constructor definition you need use access modifier, keyword `constructor`, declaration arguments between round brackets and block between braces. For example:
```C++
class Box
{
    priv var int number;

    pub constructor(int num)
    {
        number = num;
    }
}
```

> [!NOTE]
> The constructor's access modifier determines whether it will be available outside of the current class or not.

### Creating Instances
For create class instance you need specify the class name as the type and in initialization expression use operator `new`, class name and expressions between round brackets. For example:
```C++
class Box
{
    priv var int number;

    pub constructor(int num)
    {
        number = num;
    }
}

func int main()
{
    var Box box = new Box(123);

    return 0;
}
```

### Chain of calls
If you need use chain of calls you need use operator `->` between objects. For example:
```C++
class Car
{
    priv var int maxSpeed = 200;
   
    pub func void startEngine()
    {
        echo "Vroom vroom...\n";
    }

    pub func int getMaxSpeed()
    {
        return maxSpeed;
    }

    pub func void setMaxSpeed(int speed)
    {
        maxSpeed = speed;
    }
}

func int main()
{
    var Car car = new Car();
    car->startEngine();
    echo car->getMaxSpeed();
    echo '\n';
    
    return 0;
}
```

> [!WARNING]
> Call chains don't work with nested objects yet and can only handle 2 objects (class -> field/method) and code:
> ```C++
> obj1->obj2->obj3
> ```
> does not work on current language version

### This context
To access a member of a class (calling a method/getting a field value) inside this class, you can use the `this` operator. For example:
```C++
class Car
{
    priv var int maxSpeed = 200;

    pub func int getMaxSpeed()
    {
        return this->maxSpeed;
    }

    pub func void setMaxSpeed(int maxSpeed)
    {
        this->maxSpeed = maxSpeed;  // this->maxSpeed and maxSpeed not equals
    }
}
```

> [!TIP]
> If you need to call another method inside a method before defining it, then you need to use `this`. For example:
> ```C++
> class Car
> {
>     priv var int maxSpeed = 200;
> 
>     pub func void init()
>     {
>         this->startEngine();
>     }
>
>     priv func void startEngine()
>     {
>         echo "Vroom vroom...\n";
>     }
> }
> ```

## Arrays
For array definition you need use keyword `var`, type, symbols `[]`, name and if you need an initializer in the form of `[<elements>]`, when `<elements>` are any comma-separated elements for initialization. You can create arrays of classes. For example:
```C++
func int main()
{
    var int[] numbers = [1, 2, 3, 4];
    var string[] strs;  // empty array
    
    return 0;
}
```

For get or assignment element from array you need use array name and index between brackets. For example:
```C++
func int main()
{
    var int[] numbers = [1, 2, 3, 4];
    echo numbers[0];
    echo '\n';

    numbers[0] = 10;

    for (int i; i < 4; i += 1)
    {
        echo numbers[i];
        echo '\n';
    }
    
    return 0;
}
```

You can assign another array to an array. For example:
```C++
func int main()
{
    var int[] numbers = [1, 2, 3, 4];
    var int[] numbers2 = [4, 3, 2, 1];

    numbers = numbers2;
    numbers2 = [5, 6, 7, 8];
    
    return 0;
}
```

You can return arrays in functions. For example:
```C++
fucn int[] genArr()
{
    return [1, 2, 3, 4, 5];
}
```

You can specify an array as an argument to the function. Fox example:
```C++
func int test(int[] arr)
{
    // ...
}
```

> [!WARNING]
> You cannot to specify size of array.

## Traits
For traits definition you need use keyword `trait`, identifier and block of function prototypes between `{}`. For example:
```C++
trait Converter
{
    func string toString();
}
```

For implementation trait for class or type you need use keyword `impl`, trait name, keyword `for`, type name and block of implementations trait's functions. For example:
```C++
trait Converter
{
    func string toString();
}

impl Converter for int
{
    pub func string toString()
    {
        return "123";
    }
}

int main()
{
    var int num = 10;
    echo num->toString() + "\n";
    echo 1->toString() + "\n";
}
```

You can specify trait as a generalizing type of objects (as variable or function's argument).

## Include files
For include files you need use keyword `include` and string literal (path to file with extension). First, a search takes place in the libs folder with the standard language libraries, then a search takes place near the current file. When files are included, their contents are included in the current file. For example:
```C++
include "math.kr";      // std lib
include "convert.kr";   // std lib

func int main()
{
    echo sin(90)->toString() + "\n";
}
```

## Sizeof operator
For use sizeof you need use keyword `sizeof` and expression or type between round brackets. For example:
```C++
func int main()
{
    var int num = 19;
    echo sizeof(int);
    echo '\n';
    echo sizeof(num);
    echo '\n';
}
```

> ![WARNING]
> If you specify `string` in `sizeof`, you will get the value 8, since string is a `char*` and sizeof will return the size of the pointer. To get the length of a string, use the `length()` function from the inclusion `string.kr` (`str->length()`).

More examples you can see in `examples` directory.