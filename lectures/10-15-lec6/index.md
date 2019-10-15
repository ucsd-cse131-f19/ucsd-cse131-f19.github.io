---
layout: page
title: "Calculating Types"
doodle: "../../doodle.png"
---

_This writeup is heavily inspired by the presentation in
[PAPL](https://papl.cs.brown.edu/2018/types.html#%28part._.A_.Calculator_and_.Checker%29),
but adapted for our context._

# Calculating Types

## Static vs. Dynamic Errors

In [Boa](../../pa2/) we make a particular design decision – mismatches in
types, like `(+ 1 true)` report _dynamic_ or _runtime_ errors. This is the
case in many languages, for example in Python programs like `1 + "a"` clearly
report runtime errors rather than static errors. We can gain some confidence
that Python reports it dynamically because of programs like this:

```
def f(b, x):
  if b: return x + "a"
  else: return x + 2

print(f(False, 1))
print(f(True, 1))
```

If we run this Python program, it prints 3 before reporting the error that
`+` isn't supported for `int` and `str`.

Other languages, however, would report these kinds of mismatches _statically_
or at _compile-time_. For example, if we try to define the following method
in Java, we get an immediate error at its _definition_ and the method
is not defined:

```
jshell> int tryAddingIncorrectly(boolean b) { if(b) { return 1 + true; } else { return 1 + 2; } }
|  Error:
|  bad operand types for binary operator '+'
|    first type:  int
|    second type: boolean
|  int tryAddingIncorrectly(boolean b) { if(b) { return 1 + true; } else { return 1 + 2; } }
|                                                       ^------^

jshell> tryAddingIncorrectly(false)
|  Error:
|  cannot find symbol
|    symbol:   method tryAddingIncorrectly()
|  tryAddingIncorrectly()
```

We could have opinions, beliefs, and arguments about which approach is
ultimately _better_, and you can find such arguments on various fora on the
Web with a quick search for “static vs. dynamic typing” and related keywords.
Such arguments are naturally more productive if we are informed about the
inner workings of the two approaches, which represent a number of tradeoffs
between the programmer's experience, available language features, and
required engineering . Tagging values with runtime checks is a natural way to
implement dynamic errors; here we will explore reporting type mismatches
statically. With an understanding of both, we can compare the two approaches
with broader perspective.

## Static Checking in (Mini) Boa

Consider our language from lecture so far:

```
expr := <number> | true | false
     |  (<op> <expr>)
     |  (let (<name> <expr>) <expr>)
     |  (if <expr> <expr> <expr>)
     |  (+ <expr> <expr>)
op   := inc | dec
```

With the abstract syntax represented by:

```
type op =
  | Inc
  | Dec

type expr =
  | ENum of int
  | EBool of bool
  | EOp of op * expr
  | EId of string
  | ELet of string * expr * expr
  | EPlus of expr * expr
  | EIf of expr * expr * expr
```

To give static errors for type mismatches, we'd like to somehow issue an
error from the _compiler_ for programs like

```
(+ 1 true)
(if 3 true false)
(inc false)
```

### Idea #1: Errors By Expression Type

A first approach might be to observe that these errors happen when a `EBool`
appears as part of a `EPlus` or `EOp` expression, or when a `ENum` appears in
the conditional part of an `EIf`. So we might try adding some extra cases to
the compiler to find these errors:

```
let rec e_to_is (e : expr) (si : int) (env : tenv) =
  match e with
    | EIf(cond, thn, els) ->
      begin match cond with
        | ENum(n) -> failwith "Condition position must be boolean"
        (* normal compilation of If here *)
      end
    | EPlus(e1, e2) ->
      begin match e1, e2 with
        | EBool(_), _
        | _, EBool(_) -> failwith "bad operand types for binary operator '+'"
        (* normal compilation of Plus here *)
      end
    | EOp(op, e) ->
      begin match e with
        | EBool(b) -> failwith "Operator not defined on booleans"
        (* normal compilation of EOp here *)
      end
```

This works for the examples above, so we might be tempted to declare victory
and stop here. However, if we try more examples using `let` or nested
expressions we quickly see some issues:

```
(if (+ 1 2) true false)
```

**DO NOW** – take a minute to think through what the behavior of the
expression above would be given the new match cases.

In this case, the full expression is `EIf(EPlus(ENum(1),ENum(2)),
EBool(true), EBool(false))`, and the `EPlus` expression would not match the
`ENum` case in the check for `cond`. This would proceed with the normal
compilation of `EIf` despite the type error!

We might be tempted to augment the `match` cases to handle this:

```
    | EIf(cond, thn, els) ->
      begin match cond with
        | ENum(n) -> failwith "Condition position must be boolean"
        | EPlus(_, _) -> failwith "Condition position must be boolean"
        | EOp(_) -> failwith "Condition position must be boolean"
        (* normal compilation of If here *)
```

An interesting idea that certainly works for this example! But before we take
that step, let's look at another example.

```
(let (x 1) (if x true false))
```

**DO NOW** – take a minute to think through what the behavior of the
expression above would be given the new match cases.

In this case, the body of the let expression is `EIf(EId("x"), EBool(true),
EBool(false))`. The `EId("x")` value would not match any of the cases we just
added to the `match`. We could add this to the match:

```
    | EId(_) -> failwith "Condition position must be boolean"
```

However, that's clearly not a great idea, because this related program should
**not** produce a type error:

```
(let (x true) (if x 1 3))
```

Clearly, we need to do something more sophisticated in the case of `EId`,
because an id could have either a number or a boolean in it, and we need to
know which type _while compiling_.

Note that precisely the same issue comes up in `EPlus`:

```
(let (x true) (+ x 1))
```

Note that you may be thinking “can't we check the tag bits here?” **We
cannot** because we are implementing these checks _in the compiler_, and have
not yet generated any instructions, much less started the OS process that has
these tagged values in memory. Whatever we do to check this can only use the
information available to the compiler, like instances of the `expr` type.

### Idea #2: Type Information in the Environment

We noticed that the `EId` case caused issues because we didn't know the
_type_ of an identifier when checking it in the `EIf` case.

We might next try to augment the environment with type information so that
when we encounter an `EId`, we can look up not just its stack location but
its type as well. We might update the environment to

```
type tenv = (string * int * typ) list
```

That is, now each name stores both a number and its type. This requires
defining the type `typ` (we use `typ` because `type` is a keyword in OCaml).
Since our language only has numbers and booleans so far, this type suffices:

```
type typ = TNum | TBool
```

Now if we revisit the `EIf` case's `EId` match above, we can do an id lookup:

```
    | EIf(cond, thn, els) ->
      begin match cond with
        | ENum(n) -> failwith "Condition position must be boolean"
        | EPlus(_, _) -> failwith "Condition position must be boolean"
        | EOp(_) -> failwith "Condition position must be boolean"
        | EId(_) ->
          begin match find env x with
            | None -> failwith "Unbound variable identifier" (* Now we report this in several places... *)
            | Some(TNum) -> failwith "Condition position must be boolean"
            | Some(TBool) -> (* normal compilation of If here *)
          end
        (* normal compilation of If here *)
```

We would also need to consider how the types get into the environment in the
first place! This happens in `ELet`:

```
    | ELet(x, v, body) ->
      let vis = e_to_is v si env in
      let bis = e_to_is body (si + 1) ((x,si, (* type must go here! *))::env) in
      ...
```

How to determine the type at this position? Based on our examples, it seems
like something along these lines would work:

```
    | ELet(x, v, body) ->
      let type_of_v = match v with
        | ENum(_) -> TNum
        | EPlus(_, _) -> TNum
        | EBool(_) -> TBool
        (* types for other cases *)
      in
      let vis = e_to_is v si env in
      let bis = e_to_is body (si + 1) ((x,si, type_of_v)::env) in
      ...
```

**DO NOW** – Think through what you'd need to do for the remaining cases in
the `match` to calculate the `type_of_v`, like `ELet`, `EIf`, and `EId`.

The above `match`, and strategy for tracking types, works just fine if we
only never use `EIf` or `ELet` in the `v` posiiton of an `ELet`. An example
of such a program is:

```
(let (x (if true 1 2)) (+ x 3))
```

In this program, we can't know what type to give `x` without examining the
`EIf` in more detail to see that it will evaluate to a number. But the `1`
and `2` could _also_ be expressions that could be `EIf` or `ELet` expressions
that contain other expressions, and so on. Clearly, a simple `match` on just
one or two levels of the tree will not suffice. To handle arbitrary
expressions in this position, we need to somehow **traverse the expression
and calculate its type**. Since the expression is a tree, we'll need to think
through a separate recursive helper that calculates an expression's type. (We
**could** try to do this all in `e_to_is`, and have it return a list of
instructions _and_ a calculated type. In this case I'm choosing to separate
these concerns into two separate functions.)

### Idea #3: A Type Calculator (with an Environment)

The progression above suggests that we need a function that takes an
expression and returns its type. This function can also _check for
consistency_ between types and operations as it is calculating them. This is
a useful insight, because there are several cases that will come up where we
simply cannot compute a type for an expression in a meaningful way (at least
not without stepping far outside our current design). We want to write a
function that looks like:

```
type typ_env = (string * typ) list
let rec calc_typ (e : expr) (env : typ_env) : typ =
  match e with
    | ...
```

**DO NOW** – what are the cases for `ENum` and `EBool`?

For constants, calculating the type is straightforward:

```
    | ENum(_) -> TNum
    | EBool(_) -> TBool
```

For `EId`, we can simply look it up in the environment:

```
    | EId(x) -> begin match find env x with
        | None -> failwith "Unbound id"
        | Some(typ) -> typ
      end
```

Then we get to the cases that can potentially fail due to type mismatches.
For example, how should `inc` work?

```
    | EOp(Inc, e) -> ...
```

**DO NOW** – What should this case of `calc_typ` for `EOp` look like?

The `EOp` case needs to do two things: first, make sure that the argument
will evaluate to a number (has type `TNum`), and second, indicate that the
increment itself will evaluate to a number if given a number. In code, that
means:

```
    | EOp(Inc, e) -> begin match calc_typ e env with
        | TNum -> TNum
        | TBool -> failwith "inc must take a number as an argument"
      end
```

**Exercise** – Figure out the `EPlus` case on your own.

We explored the `EId` case above, which relies on names being bound in the
environment. The `ELet` case is responsible for adding them to the
environment. It's useful to consider a few examples. What is the final type
of each of these expressions?

```
(let (x true) x)
(let (x 5) x)
(let (x 6) (let (y 10) (+ x y)))
```

The first is `TBool`, the second `TNum`, and the third `TNum`. In the first
two cases, the result of the let body depends on the environment and isn't
one fixed value – for `EOp` the return was always `TNum` (if it
type-checked). It's interesting to see how this plays out in code:

```
  | ELet(x, bind, body) ->
    let x_typ = calc_typ bind env in
    calc_typ body ((x,x_typ)::env)
```

First, we rely on a recursive call to `calc_typ` to get the type of `x`, and
then use that type for checking the `body`. It's really interesting that
`ELet` doesn't contain any code for `failwith`! A `let` expression doesn't
have type mismatches on its own – there's no way here for `x` to have the
“wrong” type; the case for `let` expressions simply assumes `x` should have
the type of the `bind` expression and tries that for checking the body.

Consider a few programs with `let` that have type errors. In which recursive
call—the one for `bind` or the one for `body`—will each report its error?

```
(let (x true) (+ x 1))
(let (x (inc 5)) (inc (+ x true)))
(let (x (+ 1 true)) (+ x true))
```

**DO NOW** – Setting aside `EPlus` as an exercise, that leaves us with `EIf`.
Think ahead – what do you think will be required for the `EIf`
case? What are some interesting examples?

In `EIf`, as with other expressions, we need to consider both type mismatches
and the eventual calculated type. The type mismatches follow a pattern we've
seen – get the type for the `cond` part and `failwith` if it is `TNum` rather
than `TBool`. But what to do with the then and else branches, and what type
to return?

```
  | EIf(cond, thn, els) ->
    let cond_typ = calc_typ cond env in
    begin match cond_typ with
      | TNum -> failwith "If expects a boolean in conditional position"
      | TBool -> (* ... what happens here? ... *)
    end
```

We ought to type check the then and else branches, because otherwise there
could be a type error in one of them that we don't catch, and _we don't
necessarily know which will run at runtime_. Consider the Java example at the
beginning of these notes – Java's type checker reports an error for the
method because it _might_ be called with `true` at some point, so it refuses
to compile it. This is a crucial distinction between the static and dynamic
approaches. The dynamic approach can wait for runtime information and
potentially never reach the erroneous code, but the static approach always
reports it. So we need to check both. But then we're still stuck deciding
what type to return! But since there are only 2 types, we can just think
through all 4 cases:

```
  | EIf(cond, thn, els) ->
    let cond_typ = calc_typ cond env in
    begin match cond_typ with
      | TNum -> failwith "If expects a boolean in conditional position"
      | TBool ->
        let thn_typ = calc_typ thn env in
        let els_typ = calc_typ els env in
        match thn_typ, els_typ with
          | TNum, TNum ->
          | TNum, TBool ->
          | TBool, TNum ->
          | TBool, TBool ->
    end
```

**DO NOW** – Come up with four examples, each of which reaches a single case
of this `match`.

An example that reaches the first case is `(if x 4 5)`, when `x` is in the
environment as `TBool`. No matter if `x` is `true` or `false`, this
conditional will evaluate to a number, so `TNum` seems like a good calculated
type. Similarly, `(if x true false)` will evaluate to a boolean, so `TBool`
makes sense. But what about `(if x 4 true)`? Our type language has _no way to
express_ that this may evaluate to a number _or_ a boolean. We have two
choices:

- Make this case be an error
- Augment our language of types to accommodate “either-or” types (commonly
called _union_ types)

The former is expedient; the latter is doable with significant engineering
effort. For now, we select the former, so if we have un-equal types in an
`if`, an error is reported. This seems to agree with OCaml, but not Java!

```
❱ jshell
jshell> true ? 7 : false;
$1 ==> 7
```

```
❱ ocaml
# if true then 5 else false;;
Error: This expression has type bool but an expression was expected of type
         int
```

We can explore the consequences of union types (and various flavors of
subtyping) in the future, but for now we end with completed code for `EIf`
and some questions:

```
  | EIf(cond, thn, els) ->
    let cond_typ = calc_typ cond env in
    begin match cond_typ with
      | TNum -> failwith "If expects a boolean in conditional position"
      | TBool ->
        let thn_typ = calc_typ thn env in
        let els_typ = calc_typ els env in
        match thn_typ, els_typ with
          | TNum, TNum -> TNum
          | TBool, TBool -> TBool
          | TBool, TNum -> failwith "If branches must agree on type"
          | TNum, TBool -> failwith "If branches must agree on type"
    end
```

**Exercises and Questions**

- Simplify the `match` on `thn_typ` and `els_typ` using `=` instead of `match`
- Experiment with the Java program using a ternary operator above – say we
wanted to store the result in a variable, what would its type have to be?
- In PA2, there's a variable called `input` that holds a command-line
argument. What challenges would implementing `input` in the type checker
introduce?
- How would you use `calc_typ` in the overall infrastructure of the compiler?