---
layout: page
title: "First Discussion Notes"
doodle: "../doodle.png"
---

## Programming in Ocaml

### Details of `max`

The first example we did (`max`), is well-covered in the [pa0
writeup](../../pa0/) writeup, so we won't address it again in detail here.
The main points we discussed were comparing a C/Java-like signature to the
OCaml one, and noting that they had the same pieces (return type, parameter
names and types, and function name), just in different orders.

```
int max(int n, int m) { // C/Java

let max (n : int) (m : int) : int = (* OCaml *)
```

We also noted that if we used `return n` in the body of the function,
`return` was treated as an identifier and caused an unbound id error – OCaml
doesn't have a keyword called return.

### Defining Datatypes for Expressions

We went on to an example of defining a datatype for simple arithmetic
expressions. First we tried doing it in other languages – we started writing
some Rust code (but didn't finish because Joe rushed through), and then
focused on writing a Java version. With contributions from around the room,
we settled on this approach:

```
interface Expr { ... }
class IntNode implements Expr {
  int value;
  public IntNode(int value) { ... }
  ...
}
class ExprNode implements Expr {
  String op;
  Expr lhs, rhs;
  public ExprNode(String op, Expr lhs, Expr rhs) { ... }
  ...
}
```

Where the `...` sections represent to-be-filled code. Any code that works
with the exprs would probably be written as an interface method on `Expr`,
and then added to each of the implementing classes. We noded that this is a
common pattern in many OO-style compilers, where all the different types of
expressions share a common interface and/or superclass, and then provide
their own implementations of the methods.

The corresponding code in OCaml is:

```
type expr =
  | IntNode of int
  | ExprNode of string * expr * expr
```

It defines the type—`expr`—and two constructors or variants, which are
`IntNode` and `ExprNode`. We noted that there are no names, motivating using
`match` to access the pieces. We also noted that OCaml defined default
constructors for us, so we could directly construct them without writing any
more code:

```
let e1 = ExprNode("+", IntNode(1), IntNode(1))
```

Then we went on to show how `match` could be used to access a single field:

```
let op_of_e1 = match e1 with
  | ExprNode(op, lhs, rhs) -> op
  | IntNode(value) -> "has to be a string"
```

And also that if we changed `"has to be a string"` to some non-string value,
OCaml would report a type error, because _all branches of the match statement
have to be the same_.

Finally, we implemented a function `eval` that takes an arithmetic expression
and produces its arithmetic answer:

```
let rec eval (e : expr) : int =
  match e with
    | IntNode(value) -> value
    | ExprNode("+", lhs, rhs) -> (eval lhs) + (eval rhs)
    | ExprNode("-", lhs, rhs) -> (eval lhs) - (eval rhs)
    | ExprNode(_, lhs, rhs) -> failwith "Unknown operator"
```

We went through a few styles of defining it before ending on this one,
including the following two, which have the same behavior but use `if` and
`match` in different ways:

```
let rec eval (e : expr) : int =
  match e with
    | IntNode(value) -> value
    | ExprNode(op, lhs, rhs) ->
      if op = "+" then (eval lhs) + (eval rhs)
      else if op = "-" then (eval lhs) - (eval rhs)
      else failwith "Unknown operator"
```

```
let rec eval (e : expr) : int =
  match e with
    | IntNode(value) -> value
    | ExprNode("+", lhs, rhs) -> (eval lhs) + (eval rhs)
      match op with
        | "+" -> (eval lhs) + (eval rhs)
        | "-" -> (eval lhs) - (eval rhs)
        | _ -> failwith "Unknown operator"
```

A question came up about the `=` operator (vs `==`). `=` is _structural
equality_, which is more like calling .equals() in Java, and `==` is
_pointer_ or _reference equality_, and similar to `==` in Java. We will talk
more about equality later on in the quarter. When `match` matches on a
particular value, (as with the third case above), it uses `=`, or structural
equality, to check the value against that case.

We ended by mentioning that it would be a good idea to define another
datatype that captured exactly the allowed operators, to avoid the
fall-through `failwith` for an unknown operator. Translating that high-level
idea into code is a good way to check if you understand the ideas from this
discussion!

