# Mini Yu

Mini Yu is a dependently typed programming language, similar to Agda, Idris, Coq and Lean.
Mini Yu is a prototype language to experiment with dependently typed language
features, and runtime implementation.

Mini Yu is implemented in Haskell, and compiles source files to C code, which is
further compiled to machine code with gcc. Mini Yu uses reference counting
as garbage collection strategy, inspired by [Lean 4](https://github.com/leanprover/lean4).

# Building

In order to build the mini yu source code, you need to be running on
a Unix system, Mac, Linux, FreeBSD, etc.

Before building, make sure you have [stack](https://docs.haskellstack.org/en/stable/README/)
version 2 or higher installed on the system. You can obtain version
of stack with the command
```
stack --version
```
To build mini yu, go to the project root and enter the two commands
```
git submodule update --init
make
```
This will clone [mimalloc](https://github.com/microsoft/mimalloc),
which mini yu uses as memory allocator. The `make` command builds
the Mini Yu compiler.

Once the build has finished, the `yuc` executable in the project root
can be used to compile mini yu source code. The next section describes
how to get started.

# Features

This section describes some of the features of Mini Yu.

## Hello World and effects

To get started, here is the Hello World program in Mini Yu:
```
## Import basic functionality from standard library:
import "yu/prelude.yu"

## Function main is the program entry point:
val main : () ->> {}
let () => "Hello, World!" println
```
Note that line comments start with `##`.

Copy the code and put it in a file named `hello-world.yu`.
Compile it with the command
```
path/to/yuc -c -o hello-world.yu
```
The `-c` command line argument tells mini yu to compile the program,
not just type check, and the `-o` command line argument tells
mini yu to optimize the program. This will produce a binary
file called `hello-world.yu.exe`, which prints `Hello, World!`
when executed.

The type of `main` is effectful function type `() ->> {}`, which is
indicating that the `main` function has no arguments `()`, is
effectful `->>`, and codomain (return type) is unit `{}`.
Unit `{}` is similar to `void` in C and Java.
The effectful arrow type `->>` allows us to apply the `println`
operator and print `Hello, World!` to the standard output device.

The plan is to extend mini yu with a better effect system, and allow
users to define custom (algebraic) effects. Currently, the only
supported effect is `->>`, which allows printing strings to standard out.

The point of having an effect system is to control where observable
side effects may occur. The pure function type `->` is used for functions
which do not have side effects. Hence, we cannot print from inside such a
function. Mini Yu will not accept the following program
```
import "yu/prelude.yu"

val customPrint : Str -> {}
let (s) => s println

val main : () ->> {}
let () => customPrint("Hello, World")
```
Change the type of customPrint to `Str ->> {}`, and then it will work.

## Algebraic data types

Mini Yu uses algebraic data types to represent data.
For example, the natural numbers can be defined with
```
data Nat : Ty
let 0 : Nat
let succ : Nat -> Nat
```
**Note that the examples may not type check if you import files
from the standard library at the same time, because of name
clashes with the standard library.**

The identity type can be defined with
```
data Id[A : Ty] : (A, A) -> Ty
let refl[A : Ty, a : A] : Id(a, a)
```
The square brackets mark implicit arguments, and `(A, A) -> Ty` is a
binary function type.

## Dependent pattern matching

Values are declared with `val` and defined by (dependent) pattern matching.
Addition on natural numbers can be defined by
```
val plus : (Nat, Nat) -> Nat
let (m, 0) => m
let (m, succ(n)) => succ(plus(m, n))
```
And transitivity of identity
```
val trans[A : Ty, x : A, y : A, z : A] : (Id(x, y), Id(y, z)) -> Id(x, z)
let (refl, p) => p
```

## Operator overloading

Mini Yu supports definition and overloading of operators.
There are 3 kinds of operators:
* prefix operators,
* infix operators,
* postfix operators.

Prefix and infix operators start with an operator symbol, such as `+`, `&` or `!`.
But, the symbols `\` and `#` are not allowed in identifiers.
Postfix operators and regular variable identifiers start with an alphanumeric
character, such as `A`, `4` or `a`.
Examples of prefix and infix operator names are `+`, `&&` and `?is-true`.
Examples of postfix operator names and variable identifiers are
`length`, `Size` and `is-even?`.

We can for example define the natural numbers by using a prefix operator `++` for
the successor constructor,
```
data Nat : Ty
let 0 : Nat
let (++_\Nat) : Nat -> Nat
```
The `\Nat` is indicating that this is the operator `++` for `Nat`.

Now define `1` and `2` by
```
val 1 : Nat
let => ++ 0

val 2 : Nat
let => ++ 1
```

Addition on natural numbers as infix operator `+`,
```
val _+_\Nat : (Nat, Nat) -> Nat
let (m, 0) => m
let (m, ++ n) => ++ (m + n)
```

An `is-even?` postfix operator on natural numbers,
```
val _is-even?\Nat : Nat -> Bool
let (0) => true
let (++ 0) => false
let (++ ++ n) => n is-even?
```
where `Bool` is the type
```
data Bool : Ty
let false : Bool
let true : Bool
```

We can overload operators based on type of an argument.
It is possible to define addition of booleans by
```
val _+_\Bool : (Bool, Bool) -> Bool
let (true, b) => true
let (false, b) => b
```
For left associative infix operators, such at `+`,
mini yu uses the type of the first argument to determine which
operator to apply. So `true + x` applies `_+_\Bool` and
`1 + y` applies `_+_\Nat`.

### Details on operator precedence

The associativity and operator precedence of infix operators
is given by the first operator symbol. The infix operator precedence table is:
```
^ @        (right associative)
* / %      (left associative)
$ | &      (right associative)
+ - ? !    (left associative)
< > = : ~  (right associative)
```
The operators in the top has higher precedence than those in
the bottom, so infix `+` is left associative and has lower
precedence than infix `*`.

For right associative operators, mini yu uses the second
argument to determine which operator to apply,
and the type of the first argument of both prefix and postfix
operators is used by mini yu to determine which operator to apply.

Note that these associativity and precedence rules do no apply to
the few built-in operators, such as `=>`, `->` and `:`. For example
function type `->` has lower precedence than infix `<`, and `->` is
right associative.

## Lazy evaluation

By default, mini yu evaluates function arguments strictly,
but it is possible to mark arguments as lazy. For example,
in the definition of addition of booleans, it is often desired
to have the second argument evaluated only when the first
argument is false. This can be achieved with
```
val _+_\Bool : (Bool, [] -> Bool) -> Bool
let (true, b) => true
let (false, b) => b[]
```
The `[] -> Bool` is the lazy `Bool` type. It evaluates
the second argument only when the first argument is `false`.
Syntactic sugar allows us to apply functions in the same way when
arguments are lazy as when they are strict. So we can write
`true + false`, and it will desugar into
```
true + ([]. false)
```

## Dependent types

Let us define vector data type as a postfix operator on `Ty`,
```
data _Vec\Ty : (Ty, Nat) -> Ty
let nil.Vec[A : Ty] : A Vec(0)
let _::_\_Vec\Ty[A : Ty, n : Nat] : (A, A Vec(n)) -> A Vec(++ n)
```
Note that infix `::` is right associative, so it overloads on the
seconds argument, which is `_Vec\Ty` in this case.

As demonstrated earlier, mini yu supports dependent implicit arguments.
For an example of a dependent (explicit) arguments we define a (dependent)
function which returns the zero vector of any length `n`, where the
codomain of the function `Nat Vec(n)` depends on `n`:
```
val 0.Vec : (n : Nat) -> Nat Vec(n)
let (0) => nil.Vec
let (++ n) => 0 :: 0.Vec(n)
```

## Importing and standard library

Importing in mini yu works like copying the file and pasting it in.
There is, however, detection to avoid importing the same file twice.

Mini Yu comes with a standard library with some basic functionality.
For example, to import the list type, one can write
```
import "yu/List.yu"
```

Take a look at the `stdlib/yu/` directory to see what is available
in the standard library. If an import string starts with `yu/`, such
as `"yu/List.yu"`, then mini yu will search for files in the standard
library. Other import strings are relative to the importing file, so
```
import "functionality.yu"
```
will search for a file named `functionality.yu` located in the same directory
as the current/importing file.

## More examples

For more examples, take a look at the `examples/` directory
and the `stdlib/yu/` directory, implementing the Yu standard library.