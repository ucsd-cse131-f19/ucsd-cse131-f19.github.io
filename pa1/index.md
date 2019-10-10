---
layout: page
title: "PA1 – Simple Compiler with Binary Operations"
doodle: "../doodle.png"
---

# PA1 Anaconda, Due Wednesday October 9, 2019 (Closed Collaboration)

In this assignment you'll implement a compiler for a small language called
Anaconda, that has let bindings and binary operators. The key difference
between this language and what we implemented in class is that there can be
_multiple_ variables defined within a single let.

## Setup

Get the assignment at [Github
Classroom](https://classroom.github.com/a/maFoIzu3). This will make a
private-to-you copy of the repository hosted within the course's
organization. You can also access the public starter code [directly from this
public URL](https://github.com/ucsd-cse131-f19/pa1-student) if you don't have
or prefer not to use a Github account.

## The Anaconda Language

In each of the next several assignments, we'll introduce a language that we'll
implement.  We'll start small, and build up features incrementally.  We're
starting with Anaconda, which has just a few features – defining variables, and
primitive operations on numbers.

There are a few pieces that go into defining a language for us to compile:
* A description of the concrete syntax – the text the programmer writes.
* A description of the abstract syntax – how to express what the
  programmer wrote in a data structure our compiler uses.
* A *description of the behavior* of the abstract
  syntax, so our compiler knows what the code it generates should do.

### Concrete Syntax

The concrete syntax of Anaconda is:

```
<expr> :=
  | <number>
  | <identifier>
  | (let (<bindings>) <expr>)
  | (add1 <expr>)
  | (sub1 <expr>)
  | (+ <expr> <expr>)
  | (- <expr> <expr>)
  | (* <expr> <expr>)

<bindings> :=
  | (<identifier> <expr>)
  | (<identifier> <expr>) <bindings>
```
Here, a `let` expression can have one *or more* bindings.

### Abstract Syntax

The abstract syntax of Anaconda is an OCaml datatype, and corresponds nearly
one-to-one with the concrete syntax.

```
type prim1 =
  | Add1
  | Sub1

type prim2 =
  | Plus
  | Minus
  | Times

type expr =
  | Number of int
  | Id of string
  | Let of (string * expr) list * expr
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
```

### Semantics

A ``semantics'' describes the languages' behavior without giving all of the
assembly code for each instruction.

An Anaconda program always evaluates to a single integer.

- Numbers evaluate to
themselves (so a program just consisting of `Number(5)` should evaluate to the
integer `5`).
- Primitive expressions perform addition or subtraction by one on
their argument.
- Binary operator expressions evaluate their arguments and combine them
based on the operator.
- Let bindings should evaluate all the binding expressions to
values one by one, and after each, store a mapping from the given name to the
corresponding value in both (a) the rest of the bindings, and (b) the body of
the let expression. Identifiers evaluate to whatever their current stored
value is.

There are several examples further down to make this concrete.

The _compiler_ should stop and report an error if:

* There is a binding list containing two or more bindings with the same name. **The error should contain the string `"Duplicate binding"`**
* An identifier is unbound (there is no surrounding let binding for it) **The error should contain the string `"Unbound variable identifier {identifier}"` (where the actual name of the variable is substituted for `{identifier}`)**

Here are some examples of Anaconda programs:

#### Example 1

**Concrete Syntax**

```scheme
5               
```

**Abstract Syntax**

```ocaml
Number(5)
```

**Result**

```
5      
```

#### Example 2

**Concrete Syntax**

```scheme
(sub1 (add1 (sub1 5)))
```

**Abstract Syntax**

```ocaml
Prim1(Sub1, Prim1(Add1, Prim1(Sub1, Number(5))))
```

**Result**

```
4
```

#### Example 3

**Concrete Syntax**

```scheme
(let ((x 5)) (add1 x))
```

**Abstract Syntax**

```ocaml
Let([("x", Number(5))], Prim1(Add1, Id("x")))
```

**Result**

```
6
```

#### More examples
```
(sub1 5)
# as an expr
Prim1(Sub1, Number(5))
# evaluates to
4
```

```
(let ((x 10) (y 7)) (* (- x y) 2))
# as an expr
Let([("x", Number(10)), ("y", Number(7))],
  Prim2(Times, Prim2(Minus, Id("x"), Id("y")), Number(2)))
# evaluates to
6
```

### Implementing a Compiler for Anaconda

You've been given a starter codebase that has several pieces of
infrastructure:

* A stub for a parser for Anaconda which takes a s-expression that represents
  the code, and converts it to an abstract syntax tree (`parser.ml`). You need to
  implement the parser to actually perform the conversion.
* A main program (`main.ml`) that uses the parser and compiler to produce
  assembly code from an input Anaconda text file.  **You don't need to edit this.**
* A `Makefile` that builds `main.ml`, builds a tester for Anaconda
  (`test.ml`), and manipulates assembly programs created by the Anaconda
  compiler.  You don't need to edit the `Makefile` or `test.ml`, but you
  will edit `myTests.ml`.
  Specifically, you will add your own tests by filling in
  `myTestList` following the instructions in the beginning of the file.

  You need to add _at least 10 tests_ to `myTests.ml`. Focus on making these
  interesting and thorough, as you will get credit for showing thoughtful
  testing.
* An OCaml program (`runner.ml`) that works in concert with the `Makefile` to
  allow you to compile and run an Anaconda program from within OCaml, which
  is quite useful for testing. You don't need to edit `runner.ml`.

All of your edits—which will be to write the compiler for Anaconda, and test
it—will happen in `parser.ml`, `compile.ml`, `asm.ml` and `myTests.ml`. You
shouldn't edit `expr.ml`, `test.ml`, `runner.ml`, or `main.ml`, though you
should read and understand `expr.ml`.

### Writing the Parser

The parser will be given a S-expression representing the whole program, and
must build a AST of the `expr` data type (refer to `expr.ml`) from this S-expression.

An S-expression in OCaml (from the Core library) is of the following type:
```
type sexp =
| List of sexp list
| Atom of string
```
For more info about S-expressions in Core, see [here](https://dev.realworldocaml.org/data-serialization.html)

Thus, an example S-expression that could be parsed into a program would be as
follows
```
List([Atom("let"); List([List([Atom("x"); Atom("5")])]); Atom("x")])
```
which corresponds to
```
(let ((x 5)) x)
```
in anaconda or
```
let x = 5 in x
```
in OCaml.

This should then parse to the AST
```
ELet([("x",ENumber(5))],EId("x"))
```
which can then be compiled.

Since most S-expressions are lists, you will need to check the first element
of the list to see if the operation to perform is a `let`, `add1`, `*`, and so
on. If a S-expression is of an invalid form, (i.e. a `let` with no body, a `+`
with three arguments, etc.) report an error using failwith **that contains the string `"Invalid"`**.

You can assume that an id is a valid string of form `[a-zA-z][a-zA-Z0-9]*`.
You will, however, have to check that the string does not match any of
the language's reserved words, such as `let`, `add1`, and `sub1`.

The parsing should be implemented in
```
parse: sexp -> expr
```
There is also an added function parse_binding,
```
parse_binding: sexp -> (string, expr)
```
which may be helpful for handling `let` expressions.

### Writing the Compiler

The primary task of writing the Anaconda compiler is simple to state: take an
instance of the `expr` datatype and turn it into a list of assembly
instructions.  The provided compiler skeleton is set up to do just this,
broken up over a few functions.

The first is
```
compile : expr -> instruction list
```

which takes an `expr` value (abstract syntax) and turns it into a list of
assembly instructions, represented by the `instruction` type.  Use only the
provided instruction types for this assignment; we will be gradually expanding
this as the quarter progresses.  This function has an associated helper that
takes some extra arguments to track the variable environment and stack
offset.  These will be discussed in more detail in lecture. `compile` also
calls some other helper functions that help us seperate out the code,
it is up to you to use these or not.

**Note**: For variable bindings, we use a `(string * int) list`.  
  This is a simple data structure that's often called an association list.  
  There is a provided `find` function that looks up a value (an `int`) by key
  (a `string`).  Adding to an association list is trivial – simply add onto 
  the front with `::`.  You are responsible for understanding how ordering
  in the case of duplicate keys may interact with scope.

The other component you need to implement is:

```
i_to_asm : instruction -> string
```

which is found in `asm.ml`. It renders individual instances of the
instruction datatype into a string representation of the instruction (this is
done for you for `mov` and `ret`). This second step is straightforward, but
forces you to understand the syntax of the assembly code you are generating.
Most of the compiler concepts happen in the first step, that of generating
assembly instructions from abstract syntax. Feel free to ask or refer to
on-line resources if you want more information about a particular assembly
instruction! You should also fill out the rest of `arg_to_asm : arg ->
string` to support the `RegOffset` datatype, which will enable memory
accesses (see stackloc in `compile.ml` and the assembly reference for help).

### Assembly instructions
The assembly instructions that you will have to become familiar with for this
assignment are:

* `IMov of arg * arg` — Copies the right operand (source) into the left operand
  (destination). The source can be an immediate argument, a register or a
  memory location, whereas the destination can be a register or a memory
  location.

  Examples:
  ```
    mov rax, rbx
    mov [rax], 4
  ```

* `IAdd of arg * arg` — Add the two operands, storing the result in its first
  operand.

  Example: `add rax, 10`

* `ISub of arg * arg` — Store in the value of its first operand the result of
  subtracting the value of its second operand from the value of its first
  operand.

  Example: `sub rax, 216`

* `IMul of arg * arg` — Multiply the left argument by the right argument, and
  store in the left argument (typically the left argument is `rax` for us)

  Example: `imul rax, 4`

### Running main

The `main` program built with `make main` takes a single file as its
command-line argument, and outputs the compiled assembly string on standard
out. Note the `.ana` extension.

```
$ make main
$ ./main input/forty_two.ana
section .text
global our_code_starts_here
our_code_starts_here:
  mov rax, 42
  ret
```

To actually evaluate your assembly code, first we must create a `.s` assembly file, and
then link it with `main.c` to create an executable.
```
$ make output/forty_two.s (create the assembly file)
$ make output/forty_two.run (create the executable)
```
Finally you can run the file by executing to see the evaluated output:
```
$ ./output/forty_two.run
```

### Testing the Compiler

The test file has the helper function `t` that will be useful to you:

```
t : string -> string -> string -> OUnit.test
```
The first string given to `t` (test) is a test name, followed by an Anaconda
program (in concrete syntax) to compile and evaluate, followed by a string for
the expected output of the program (this will just be an integer in quotes).
This helper compiles, links, and runs the given program, and if the compiler
ends in error, it will report the error message as a string.  This includes
problems building at the assembler/linker level, as well as any explicit
`failwith` statements in the compiler itself.

If your tests do not have any errors, a `.s` file and `.run` executable is generated
in the `output/` directory, containing the compiled assembly code and executable
for that case.

You can test all the provided tests and the tests you have provided in `myTests.ml`
by running
```
$ make test
$ ./test
```
This should report all tests that fail to compile or diverge from the specified
result.


There is also a function `t_err` that will help with testing for errors:
```
t_err : string -> string -> string -> OUnit.test
```
This will let you check that error messages are correctly printed by your
compiler.

**Note**: You should name your tests, but keep in mind that test
names cannot have spaces; this is due to the way the `Makefile`
relies on test names being used for filenames.

**Debug/Protip**: Check your assembly files as a means of debugging your code.
*If you can work through
the assembly and identify incorrect assembly instructions, you can trace the
problem back to your compiler! You can use `make output/file.s` to build the
assembly for a file in `input/file.ana`. You can use `make output/file.run`
to build the binary for a file in `input/file.ana`. You can just run the
first step (to build the assembly), then manually edit your `.s` to see what
some assembly code may do if you want to experiment.

## Help, Strategies, and Extensions

**Working Incrementally**

If you are struggling to get started, here are a few ideas:

- Try to tackle features one at a time. For example, you might completely
ignore let expressions at first, and just work on addition and numbers to
start. Then you can work into subtraction, multiplication, and so on.
- Some features can be broken down further. For example, the let expressions
in this assignment differ from the ones in class by having multiple variables
allowed per let expression. However, you can first implement let for just a
single variable (which will look quite a bit like what we did in class!) and
then extend it for multiple bindings.
- Use git! Whenver you're in a working state with some working tests, make a
commit and leave a message for yourself. That way you can get back to a good
working state later if you end up stuck.

**Asking For Help**

This assignment is _closed to collaboration_, so TAs won't answer questions
about your code or algorithms, and you should treat it like a take-home test.
In office hours, we _can_ answer any questions you have about the code or
concepts from class (and for this assignment, much of the class code is quite
helpful).

You can also always ask questions _privately_ on the course message board. We
may not answer questions in as much detail as we would for open collaboration
assignments. Rather, we will read all the questions and aggregate frequently
asked questions and advice based on the questions we get.

**Extensions**

These are not required, nor will they give you any extra credit, but they are
interesting to think about!

- There are a lot of extra stores to memory where a value is immediately
re-fetched from memory. How could you change the compiler to avoid these?
- In Scheme and Racket, the `+` operator takes any number of arguments, so
`(+ 1 2 3)` evaluates to 6. Extend your implementation of operators to allow
for these arbitrary-arity cases.

**FAQ F2019**

**How to write tests for parse?**
`t_parse` and `t_parse_error` functions are provided in `test.ml`, which you can use to write your own tests for parser.

An example of a parse test is

```
  let myTestList =
    [ (* Fill in your tests here: *)
      t_parse "example" "1" (ENumber(1));
    ]
  ;;
```

To make this test pass, you would add code to `parser.ml` to handle the `Atom` case, similar to how our parser in class worked.

**What should `(let ((x 5) (z x)) z)` produce?**

From the PA writeup: “Let bindings should evaluate all the binding expressions to values one by one, and after each, store a mapping from the given name to the corresponding value in both (a) the rest of the bindings, and (b) the body of the let expression. Identifiers evaluate to whatever their current stored value is.”

**Are the let bindings from class valid anaconda programs, or do anaconda programs always have the extra parentheses around the bindings?**

In Anaconda, there's always the extra set of parens around the list.

**I get an error that says "Error: Signalled -10 when running output/file"**

That's typically a segmentation fault. See the discussion podcast from 10-04 for some suggestions on debugging.

[https://podcast.ucsd.edu/watch/fa19/cse131_a00/21/screen](https://podcast.ucsd.edu/watch/fa19/cse131_a00/21/screen)

**I get an error like `"ocamlfind: Package sexplib not found"` when I run `make` on my laptop**

Try running `opam install sexplib` to make sure you have the package installed.

**Can we write additional helper functions?**

Yes.


**Do we care about the text return from failwith?**

Absolutely. Any time you write software you should strive to write thoughtful error messages. They will help you while debugging, you when you make a mistake coming back to your code later, and anyone else who uses your code.

As for the autograder, we expect you to catch parsing and compilation errors. For parsing errors you should `failwith` an error message containing the word `Invalid`. For compilation errors, you should catch duplicate binding and unbound variable identifier errors and `failwith` `Duplicate binding` and `Unbound variable identifier {identifier}` respectively. We've also added these instructions to the PA writeup.

**How should we check that identifiers are valid according to the description in the writeup?**

From the PA writeup: “You can **assume** that an id is a valid string of form `[a-zA-z][a-zA-Z0-9]*`. You will, however, ...”

**Assume** means that we're not expecting you to check this for the purposes of the assignment (though you're welcome to if you like).

**What should the program "()" compile to?**

Is `()` an anaconda program (does it match the grammar)? What should the compiler do with a program that doesn't match the grammar?

**What does "and" mean in Ocaml?**

A construction like

```
let rec f ... = ...
and g ... = ....
```

allows `f` and `g` to be mutually recursive (if they were separate `let rec`s, `g` could refer to `f` but not vice versa.)

**What does the writeup mean when it says that duplicate bindings should be an error? Does that mean each variable can only be defined once?**

Consider this Ocaml example, which is directly analogous to the design described for anaconda:

```
# let x = 10 in let x = x + 1 in x;;
- : int = 11
# let x = 10 and x = x + 1 in x;;
Error: Variable x is bound several times in this matching
```

**I wrote out some expected output like `(ELet([("x", ENumber(10)), ("y", ENumber(7))], EPrim2(Times, EPrim2(Minus, EId("x"), EId("y")), ENumber(2))))` and Ocaml is giving me a type error that is hard to make sense of.**

Remember that `;` (semicolon) separates list items and `,` (comma) separates tuple elements.

**What's the best way to test? What is test case <some-test-name-from-autograder> testing?**

A few suggestions:

- First, make sure to test all the different expressions as a baseline
- Then, look at the grammar. There are lots of places where `<expr>` appears. In each of those positions, _any other expression_ could appear. So `let` can appear inside `+` and vice versa, and in the binding position of let, and so on. Make sure you've tested enough _nested expressions_ to be confident that each expression works no matter the context
- Names of variables are interesting – the names can appear in different places and have different meanings depending on the structure of let. Make sure that you've tried different combinations of `let` naming and uses of variables.
