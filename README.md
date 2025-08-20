# StageLang
**Stge** - multi-paradigm programming language.

Compiler version: **v0.1** – C++ &amp; LLVM

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
