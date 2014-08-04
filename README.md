Meta C - Quasi-quotation of ANSI C99 syntax
-----

Writing C is a labor, but writing Haskell causes you fever.

Let's generate real world codes by statically typed code generator, using Template Haskell.

## Quasi-quotation

Each of the following quasi-quotations convert the inside string into an abstract syntax tree data type defined in `Language.Meta.C99.AST`.

 * Translation unit `[c| ... |]`

 * Statements `[cs| ... |]`

 * Expression `[ce| ... |]`

Every AST data types is an instance of `Show`.

### Example

```hs
import Language.Meta.C99.Quote

-- Translation unit [c| ... |]
translation_unit :: String
translation_unit = show [c|
int main(int argc, char **argv)
{
    printf("hello world!\n");
    
    return 0;
}
|]

-- Statements [cs| ... |]
statements :: String
statements = show [cs|
    while (1) {
        printf("hello world!\n");
    }
|]

-- Expression [ce| ... |]
expression :: String
expression = show [ce| x * y * z |]
```

## Anti-quotation

 Quoted string can have anti-quotes in the following formats.

 * `${}` takes a `String` variable

 * `${<exp>}` executes arbitrary haskell expression of type `String`

Replacement targets are statements, expressions, external declarations, and string inside identifiers.

### Example

```ghci
> :t [ce| x + ${} |] 
[ce| x + ${} |] :: [Char] -> Language.Meta.C99.AST.Exp
> [ce| x + ${} |] "y"
x + y
> :t  [ce| ${} + (${}) |]
[ce| ${} + (${}) |]
  :: String -> String -> Language.Meta.C99.AST.Exp
> [ce| ${} + ${} |] "x" "y"
x + y
> [ce| x + ${"y"} |]
x + y
> [cs| foo_${}(); |] "bar"
foo_bar();
```

## Limitation

 * Preprocessing is not performed in quoted strings. This means macros for syntax modification could not be parsed.

```c
#define LOOP(i, n) for (i = 0; i < n; ++i)

LOOP(i, 10) {
    // parse error
}
```

 * Comments inside a statement or decalaration causes parse error.

```c
int /* parse error */ x;

struct x {
    int x; // parse error
};
```

 * Context-sensitive portion of grammers will not be parsed appropriately. 

```c
typedef int x;

x * x; // either expression (x * x) or declaration (int *x)

x(y); // either function call (x(y)) or declaration (int (y))
```

  However, those kind of misinterpretations do not really matter when you only concern about their literal representations.

 * Trigraph/Digraph is not supported (who knows).

## Standard

 * C99 (ISO/IEC 9899:TC3)

## LICENSE 

  MIT
