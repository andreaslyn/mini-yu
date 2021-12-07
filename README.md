# Mini Yu

Mini Yu is a dependently typed programming language, similar to Agda, Idris, Coq, Lean.
It is an experimental language to test dependently typed language
features and runtime implementation.

The mini yu compiler is implemented in Haskell. It compiles mini yu source code to
C code, which is further compiled to machine code with gcc. Mini Yu uses reference counting
as garbage collection strategy, inspired by [Lean 4](https://github.com/leanprover/lean4)
and [Koka](https://github.com/koka-lang/koka).

# Related Work

A list of some work I have relied on while developing mini yu:
* [Sebastian Ullrich and Leonardo de Moura, Counting Immutable Beans: Reference Counting Optimized for Purely Functional Programming](https://arxiv.org/abs/1908.05647)
* [Jesper Cockx, Dependent pattern matching and proof-relevant unification](https://jesper.sikanda.be/files/thesis-final-digital.pdf)
* [Andreas Abel, foetus - Termination Checker for Simple Functional Programs](https://www.semanticscholar.org/paper/foetus-Termination-Checker-for-Simple-Functional-Abel/c216d842401569de47d2472b84d33f4f38bbe670)
* [Matúš Tejiščák and Edwin Brady, Practical Erasure in Dependently Typed Languages](https://eb.host.cs.st-andrews.ac.uk/drafts/dtp-erasure-draft.pdf)
* [Daan Leijen, Benjamin Zorn and Leonardo de Moura, Mimalloc: Free List Sharding in Action](https://www.microsoft.com/en-us/research/publication/mimalloc-free-list-sharding-in-action/)

# Building

In order to build the mini yu source code, you need to be running on
a Unix system, Mac, Linux, FreeBSD, etc.

Before building, make sure you have [stack](https://docs.haskellstack.org/en/stable/README/)
installed on the system. You can obtain version
of stack with the command
```
stack --version
```
If you have an old version of stack, then you may need to
upgrade it. This can commonly be achieved with the command
```
stack upgrade
```
To build mini yu quickly, go to the project root and enter the commands
```
git submodule update --init
make config
make SPLITSTACK=0
make stdlib
```
This will clone the [mimalloc](https://github.com/microsoft/mimalloc)
submodule, which mini yu uses as memory allocator. The `make` commands
will configure and build the project. The `SPLITSTACK=0` option disables
a split stack feature used for automatically growing/shrinking the
runtime stack(s). This feature requires a custom gcc version. The `stdlib`
target builds the Mini Yu standard library.

In order to build mini yu with split stack feature, make sure you have MP 4.2+
and MPFR 3.1.0+ and MPC 0.8.0+ installed, then issue the following commands:
```
git submodule update --init
make config
make -j8 custom-gcc
make
make stdlib
```
This will build mimalloc, the custom version of gcc, and the mini yu
compiler. Beware that building the custom gcc will take some time.

When the build has finished, the `yuc` executable in the project root
can be used to compile mini yu source code. The next section describes
how to get started.

# Features

This section describes some of the features of Mini Yu.

## Hello World and effects

To get started, here is the Hello World program in Mini Yu:
```
## Import basic functionality from standard library:
import yu/prelude
of (...)

## Function main is the program entry point:
val main : {} ->> {}
let () => "Hello, World!" .println
```
Note that line comments start with `##`.

Copy the code and put it in a file named `hello-world.yu`.
Compile it with the command
```
path/to/yuc hello-world.yu -c -o
```
The `-c` command line argument tells mini yu to compile the program,
not just type check. The `-o` command line argument tells
mini yu to optimize the program. This will produce a binary
file called `hello-world.yu.exe`, which prints `Hello, World!`
when executed.

The type of `main` is effectful function type `{} ->> {}`, which is
indicating that the `main` function has one argument of type unit `{}`, is
effectful `->>`, and codomain (return type) is unit `{}`.
The effectful arrow type `->>` allows us to apply the `.println`
postfix operator and print `Hello, World!` to the standard output device.

Currently, the only supported effect is `->>`, which allows printing
strings to standard out. I am working on extending mini yu with a
more powerful effect system.

An effect system is used to control where observable side effects may
occur. The pure function type `->` is used for functions which do not
have side effects. Hence, we cannot print from inside such a function.
Mini Yu will not accept the following program
```
import yu/prelude
of (...)

val customPrint : Str -> {}
let s => s .println

val main : {} ->> {}
let () => customPrint "Hello, World"
```
Change the type of `customPrint` to `Str ->> {}`, and then it will work.

## Algebraic data types

Mini Yu has algebraic data types. For example, the natural numbers can be defined with
```
data Nat : Ty
of 0 : Nat
of succ : Nat -> Nat
```
**Note that the examples may not type check if you import files
from the standard library at the same time, because of name
clashes with the standard library.**

The identity type can be defined with
```
data Id [A] : A & A -> Ty
of refl [A] [a : A] : Id a a
```
The square brackets mark implicit arguments `[A : Ty]` and `[a : A]`,
and `A & A -> Ty` is a binary function type.

## Dependent pattern matching

Values are declared with `val` and defined by (dependent) pattern matching.
Addition on natural numbers can be defined by
```
val plus : Nat & Nat -> Nat
let m 0 => m
let m (succ n) => succ (plus m n)
```
And transitivity of identity
```
val trans [A] [x y z : A] : Id x y & Id y z -> Id x z
let refl p => p
```

## Operator overloading

Mini Yu supports definition and overloading of operators.
There are 3 kinds of operators:
* prefix operators,
* infix operators,
* postfix operators.

Prefix and infix operators start with an operator symbol, such as `+`,
`&`, `!`, `=`, etc. But the symbols `\` and `#` are treated
specially, not regarded as operator symbols. Postfix operators
start with a period `.` for example `.length`, `.is-true?`.
Variable identifiers start with an alphanumeric character,
such as `A`, `1st`, `a*b`. Note that if we insert space `a * b`,
then it will mean the infix operator `*` applied to two arguments,
`a` and `b`, rather than the variable `a*b`.

We can define the natural numbers by using a prefix operator `++` for
the successor constructor,
```
data Nat : Ty
of 0 : Nat
of (++#Nat) : Nat -> Nat
```
The `#Nat` is indicating that this is the operator `++` for `Nat`.

Now define `1` and `2` by
```
val 1 : Nat
let => ++ 0

val 2 : Nat
let => ++ 1
```

Addition on natural numbers as infix operator `+`,
```
val (+#Nat) : Nat & Nat -> Nat
let m 0 => m
let m (++ n) => ++ (m + n)
```

A `.is-even?` postfix operator on natural numbers,
```
val (.is-even?#Nat) : Nat -> Bool
let 0 => true
let (++ 0) => false
let (++ ++ n) => n .is-even?
```
where `Bool` is the type
```
data Bool : Ty
of false : Bool
of true : Bool
```

We can overload operators based on type of an argument.
It is possible to define addition of booleans by
```
val (+#Bool) : Bool & Bool -> Bool
let true b => true
let false b => b
```
For left associative infix operators, such as `+`, mini yu uses the
type of the first argument to determine which operator to apply. So
`true + x` applies `+#Bool` and `1 + y` applies `+#Nat`.

### Details on operator precedence

The associativity and operator precedence of infix operators is given
by the first operator symbol. The infix operator precedence table is:
```
^ @        (right associative)
* / %      (left associative)
$ | &      (right associative)
+ -        (left associative)
= : ? !    (right associative)
< > ~      (left associative)
```
The operators in the top have higher precedence than those in the
bottom, so infix `+` is left associative and has lower precedence
than infix `*`.

For right associative operators, mini yu uses the second argument to
determine which operator to apply, and the type of the first argument
of both prefix and postfix operators is used by mini yu to determine
which operator to apply.

Note that these associativity and precedence rules do no apply to the
few built-in operators, such as `=>`, `->` and `:`. For example
function type `->` has lower precedence than infix `<`, and `->` is
right associative.

## Lazy evaluation

By default, mini yu evaluates function arguments eagerly, but it is
possible to mark arguments as lazy. For example, in the definition of
addition of booleans, it is often desired to have the second argument
evaluated only when the first argument is false. This can be achieved with
```
val (+#Bool) : Bool & ([] -> Bool) -> Bool
let true b => true
let false b => b []
```
The type `[] -> Bool` is the lazy `Bool` type. The operator `+#Bool`
evaluates the second argument only when the first argument is `false`.

## Delayed evaluation

Delayed evaluation is similar to lazy evaluation, but it will
not memoize its result, like lazy does. The below definition of
`Bool` product demonstrates delayed evaluation.
```
val (*#Bool) : Bool & (() -> Bool) -> Bool
let false b => false
let true b => b ()
```

## Dependent types

Define the vector data type by
```
data Vec : Nat & Ty -> Ty
of nilv [A] : Vec 0 A
of (::#Vec) [A n] : A & Vec n A -> Vec (++ n) A
```
Note that infix `::` is right associative, so it overloads on the
second argument, which is `Vec` in this case.

As demonstrated earlier, mini yu supports dependent implicit arguments.
For an example of a dependent (explicit) arguments we define a (dependent)
function which returns the zero vector of any length `n`, where the
codomain of the function `Vec n Nat` depends on `n`:
```
val 0v : (n : Nat) -> Vec n Nat
let 0 => nilv
let (++ n) => 0 :: 0v n
```

## Importing and standard library

Mini Yu comes with a standard library with some basic functionality.
For example, to import the list type, one can write
```
import yu/List
of List
of nil
of (...#List)
```
This puts `List` and `nil` into scope, and `(...#List)` all operators
on `List` from the module `yu/List`. One can specify an aliases for
modules with
```
import L => yu/List
of List
of (...#List)
```
This puts `List` and it's operators into scope, and we can use `nil.L`
to refer to `nil` from module `yu/List`. In general `x.L` will refer
`x` from module `yu/List`.

Alternatively, use `(...)` to import everything from the module
```
import yu/List
of (...)
```

Take a look at the `stdlib/yu/` directory to see what is available
in the standard library. If an import string starts with `yu/`, such
as `yu/List`, then mini yu will search for files in the standard
library.

Users can specify custom packages with the `-p` command line option.
For example if we have a directory `path/to/package`, then we
can invoke `yuc` with `-p path/to/package`, which will bring
modules with `package/` prefix into scope. One can imagine a file
called `path/to/package/mod.yu`, consisting of
```
import yu/Nat
of Nat

import L => yu/List
of List

val empty : List Nat
let => nil.L
```
By running `yuc` with `yuc main.yu -c -p path/to/package`, then
we can refer to the module with `package/mod`. For example, in `main.yu`
containing
```
import yu/prelude
of (...)

import M => package/mod

val main : {} ->> {}
let () => empty.M .len .println
```
This program will print `0` when executed.

## More examples

For more examples, take a look at the `examples/` directory
and the `stdlib/yu/` directory, implementing the standard library.
