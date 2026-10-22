# Lambda Calculus Interpreter

**Authors:** Nicolás Rivas Rodríguez and Marcos Filgueiras Baamonde

This project implements a typed lambda calculus interpreter with support for multiline expressions, recursion, global definitions, strings, lists, tuples, records, variants, and subtyping.

## Table of Contents

- [User Manual](#user-manual)
  - [Multiline Expressions](#multiline-expressions)
  - [Recursion: `letrec`](#recursion-letrec)
  - [Global Definition Context](#global-definition-context)
  - [`String` Type](#string-type)
  - [Lists](#lists)
  - [Tuples](#tuples)
  - [Records](#records)
  - [Variants and `case of`](#variants-and-case-of)
  - [Subtyping](#subtyping)
- [Technical Manual](#technical-manual)
- [Example Functions](#example-functions)

---

# User Manual

To run the interpreter execute the ```top``` script.

## Multiline Expressions

The interpreter supports expressions that span multiple lines. Expressions are terminated with `;;`, allowing them to be written in a more readable format.

```text
let
    x = 1
in
    succ x
;;
```

## Recursion: `letrec`

Recursive expressions can be defined directly using `letrec`. For example, the following function adds a natural number `n` to `m`:

```text
letrec sum : Nat -> Nat -> Nat =
    lambda n : Nat. lambda m : Nat.
        if iszero n then m else succ (sum (pred n) m)
in
    sum 21 24
;;
```

## Global Definition Context

Global names can be associated with values or terms and reused in subsequent expressions.

```text
x = true;;
x;;
```

Functions can also be stored:

```text
id = lambda x : Bool. x;;
id;;
```

## `String` Type

The interpreter recognizes strings enclosed in double quotation marks. It includes the following functions:

- `concat`: concatenates two strings.
- `length` / `len`: returns the number of characters in a string.

```text
"hola";;
concat "ho" "la";;
len "hola";;
```

## Lists

Lists contain elements of the same type. The following operations are available:

- `head`: returns the first element.
- `tail`: returns the list without its first element.
- `isnil`: indicates whether a list is empty.

```text
x = [1, 2, 3, 4];;
y = ["a", "b", "c", "d"];;

head x;;
head y;;
tail x;;
tail y;;
isnil [];;
isnil x;;
```

To declare a list type in a lambda function, use `List` followed by the type of its elements, for example `List Nat`.

## Tuples

Tuples contain at least two elements, separated by commas and enclosed in curly braces `{}`. Their elements can have different types. To access an element, append a dot followed by its index, starting at `0`.

```text
x = {3, "hola", {1, 2}};;
x.0;;
x.1;;
x.2;;
x.2.0;;
```

## Records

Records contain one or more named fields. Each label begins with a lowercase letter. A field can be accessed by appending a dot followed by its label.

```text
r = {x = 3, y = "hola", z = {x = 1, y = {2, 3}}};;

r.x;;
r.y;;
r.z;;
r.z.x;;
r.z.y.0;;
```

## Variants and `case of`

Variants allow you to declare a type consisting of different labels, each associated with a type. The type name begins with an uppercase letter. The declaration is enclosed in `<` and `>`.

```text
Int = <pos : Nat, zero : Bool, neg : Nat>;;
```

To create an instance, specify a label, its value, and the variant type:

```text
p3 = <pos = 3> as Int;;
f = lambda i : Int. i;;
f p3;;
```

### `case of` Expression

`case of` evaluates a variant using a list of cases separated by `|`. Each case associates a variant with an expression using `=>`. The output expressions must all have the same type. A specific value or a variable can be used to match a label's value.

```text
case <pos = 3> as Int of
    <pos = 2> => 2
    | <pos = 3> => 3
    | <pos = p> => succ p
;;
```

The order of the cases matters: the first matching case is selected.

Example of a function that obtains the absolute value of an `Int` variant:

```text
abs = lambda i : Int.
    case i of
        <pos = p> => (<pos = p> as Int)
        | <zero = z> => (<zero = true> as Int)
        | <neg = n> => (<pos = n> as Int)
;;
abs <neg = 3> as Int;;
```

## Subtyping

The interpreter supports record and variant subtyping.

### Record Subtyping

A record is a subtype of another record if it contains all the labels required by the latter and its values have types equal to or subtypes of the required types. It may include additional labels.

```text
f = lambda i : {x : {y : Nat}}. i.x.y;;
f {x = {x = 1, y = 2}, y = 3};;
```

### Variant Subtyping

For variants, the opposite criterion applies: a variant is a subtype of another if it contains only labels present in the latter, so it may have fewer labels.

```text
Int = <pos : Nat, zero : Bool, neg : Nat>;;
N = <pos : Nat>;;
f = lambda i : Int. i;;
f <pos = 3> as N;;
```

Therefore, a function expecting a given type can accept a subtype of that type.

---

# Technical Manual

## Multiline Expressions

The recursive function `read_line_loop` was added to `main.ml` and is called instead of invoking `read_line` directly. The function reads input until it encounters the `;;` terminator.

## Internal Fixed-Point Combinator

The following changes were made to add direct recursion:

- In `lexer.mll`, the `letrec` token was added.
- In `lambda.ml`, the `TmFix` term was added, along with its typing rule `T-Fix` and evaluation rules `E-Fix` and `E-FixBeta`.
- In `lambda.ml`, `string_of_term`, `free_vars`, and `subst` were also updated to handle the new term.
- In `parser.mly`, handling for the `letrec` token was added.

## Global Definition Context

`main.ml`, `parser.mly`, `lambda.ml`, and `lambda.mli` were modified.

The following types were added to `lambda.ml`:

```ocaml
type vcontext = (string * term) list
type command = Eval of term | Bind of string * term
```

`vcontext` represents the value context, and `command` distinguishes between evaluating a term and binding it to a global name. The functions `emptyvctx`, `addvbinding`, `getvbinding`, and `apply_ctx` were also added to create, extend, query, and apply the value context.

In addition, the context-related functions and the `typeof`, `eval1`, and `eval` functions were adapted to receive the required contexts. The corresponding declarations in `lambda.mli` were updated.

In `parser.mly`, the start symbol was changed to produce a value of type `command`: `IDV EQ term EOF` for a global definition, or `term EOF` for an expression to evaluate. In `main.ml`, `execute` was added and `top_level_loop` was modified to manage the context.

During testing, it was found that a globally stored function was not being correctly applied to its argument. Evaluation treated the two terms separately. This was fixed by taking the context into account when evaluating the first term, so that a global function is applied to the second term.

## `String` Type

`lexer.mll`, `parser.mly`, `lambda.mli`, and `lambda.ml` were modified.

- Recognition of the new expressions was added in `lexer.mll` and `parser.mly`.
- `TyString`, `TmString`, `TmConcat`, and `TmLength` were added to `lambda.mli`.
- In `lambda.ml`, `string_of_ty`, `typeof`, `string_of_term`, `free_vars`, `subst`, and `eval1` were updated.

`typeof` checks that `concat` and `length` receive strings and returns an error if the types are inappropriate. Evaluating `TmConcat` concatenates two strings, while `TmLength` calculates a string's length. The implementation aimed to avoid depending directly on OCaml's `String` module.

## Lists

`lexer.mll`, `parser.mly`, `lambda.ml`, and `lambda.mli` were modified.

In `lexer.mll`, the tokens `[`, `]`, `head`, `tail`, and `isnil` were added. In `parser.mly`, the `list` rule, rules for the new operations, and parsing for lists whose elements are comma-separated were added.

The following were added to `lambda.ml`:

- `TyNil`, to represent empty lists.
- `TyList`, applied to the element type.
- `TmNil`, for an empty list.
- `TmCons`, to construct a list from two terms.
- `TmIsnil`, `TmHead`, and `TmTail`, for the corresponding operations.

`string_of_ty`, `string_of_term`, `free_vars`, `subst`, and `apply_ctx` were also updated, along with `isval` and the typing and evaluation rules.

To allow list types in lambda expressions, `parser.mly` was extended to support declaring a type as `List ty`. Lists must have a type; for example:

```text
lambda l : List Nat. [1]
```

## Tuples

`lexer.mll`, `parser.mly`, `lambda.ml`, and `lambda.mli` were modified.

- The `{` and `}` tokens were added to `lexer.mll`.
- The recursive rules `tupletm` and `tuplety`, tuple parsing, and projection rules were added to `parser.mly`.
- `lambda.ml` and `lambda.mli` were updated with `TyTuple`, which stores a list of types; `TmTuple`, which stores a list of terms; and `TmProjTuple`, which stores a term and an integer.

Finally, `typeof`, `string_of_ty`, `string_of_term`, `free_vars`, `subst`, and `apply_ctx` were updated, and tuple handling was added to `isval`.

## Records

The implementation is similar to that of tuples. `parser.mly`, `lambda.ml`, and `lambda.mli` were modified.

The recursive rules `recordtm` and `recordty` were added to `parser.mly`, along with rules for parsing records and projecting their fields.

The following were added to `lambda.ml` and `lambda.mli`:

- `TyRecord`, which stores a list of `(string * ty)` pairs.
- `TmRecord`, which stores a list of `(string * term)` pairs.
- `TmProjRecord`, which stores a label and a term.

The functions `typeof`, `string_of_ty`, `string_of_term`, `free_vars`, `subst`, and `apply_ctx` were updated, and record handling was added to `isval`. `typeof` also checks that labels are not duplicated.

## Variants

`lexer.mll`, `parser.mly`, `lambda.ml`, `lambda.mli`, and `main.ml` were modified.

In `lexer.mll`, the tokens `<`, `>`, `case`, `of`, `|`, and `=>` were added, along with recognition of identifiers beginning with an uppercase letter.

`TyVariant`, which stores a list of `(string * ty)` pairs, and `TmVariant`, which stores a `(string * term)` pair together with a string identifying the type, were added. `TyVar` was also added to represent type names stored in the global context.

Additional changes included:

- Adding a parser rule to recognize type declarations.
- Adding the `Type` command to `command` and its execution in `main`, storing the type in the type context.
- Modifying `string_of_ty` and `string_of_term` to receive the type context.
- Creating `resolve_tybind` in `lambda.ml` to recursively resolve type bindings; it is used by `typeof` during type comparisons.
- Updating `typeof`, `string_of_ty`, `string_of_term`, `free_vars`, `subst`, and `apply_ctx`, as well as `isval`.

### Implementation of `case of`

The recursive rule `caselist` was added to `parser.mly` to parse variant cases and the terms returned by each case.

The `TmCase` term was added to `lambda.ml` and `lambda.mli`; it stores a term and a list of pairs of terms. The functions that process terms were also updated.

Typing `case of` requires handling the variables that appear on the left-hand side of the cases. To check that all output terms have the same type, the expected type of each variable is derived from the variant label and added to the local context. `resolve_tybind` is reused to resolve type names.

## Subtyping

The subtyping implementation required changes only to `lambda.ml`. The recursive function `subty` was created; it takes two types and checks whether the first is a subtype of the second. The function recursively traverses compound types and applies the specific subtyping rules for records and variants.

`subty` replaces direct type comparison in the `typeof` rules for `TmApp` and `TmFix`.

---

# Example Functions

The following examples show how to define and use recursive functions in the interpreter.

## Addition

```text
letrec sum : Nat -> Nat -> Nat =
    lambda n : Nat. lambda m : Nat.
        if iszero n then m else succ (sum (pred n) m)
in
    sum 21 24
;;
```

## Multiplication

```text
letrec sum : Nat -> Nat -> Nat =
    lambda n : Nat. lambda m : Nat.
        if iszero n then m else succ (sum (pred n) m)
in
letrec aux : Nat -> Nat -> Nat -> Nat =
    lambda c : Nat. lambda p1 : Nat. lambda p2 : Nat.
        if iszero p2 then 0 else
        if iszero (pred p2) then (sum c p1)
        else aux (sum c p1) p1 (pred p2)
in
letrec prod : Nat -> Nat -> Nat =
    aux 0
in
    prod 2 3
;;
```

## Fibonacci

```text
letrec sum : Nat -> Nat -> Nat =
    lambda n : Nat. lambda m : Nat.
        if iszero n then m else succ (sum (pred n) m)
in
letrec fib : Nat -> Nat =
    lambda n : Nat.
        if iszero (pred n) then n
        else sum (fib (pred (pred n))) (fib (pred n))
in
    fib 10
;;
```

## Factorial

```text
letrec sum : Nat -> Nat -> Nat =
    lambda n : Nat. lambda m : Nat.
        if iszero n then m else succ (sum (pred n) m)
in
letrec aux : Nat -> Nat -> Nat -> Nat =
    lambda c : Nat. lambda p1 : Nat. lambda p2 : Nat.
        if iszero p2 then 0 else
        if iszero (pred p2) then (sum c p1)
        else aux (sum c p1) p1 (pred p2)
in
letrec prod : Nat -> Nat -> Nat =
    aux 0
in
letrec fact : Nat -> Nat =
    lambda n : Nat.
        if iszero (pred n) then 1 else prod n (fact (pred n))
in
    fact 5
;;
```

## List Length

```text
letrec length : List Nat -> Nat =
    lambda l : List Nat.
        if isnil l then 0 else sum 1 (length (tail l))
in
    length
;;
```

## List Concatenation

```text
letrec append : List Nat -> List Nat -> List Nat =
    lambda l1 : List Nat. lambda l2 : List Nat.
        if isnil l1 then l2
        else [(head l1)::(append (tail l1) l2)]
in
    append
;;
```

## `map` over Lists

```text
letrec map : (Nat -> Nat) -> List Nat -> List Nat =
    lambda f : Nat -> Nat. lambda l : List Nat.
        if isnil l then l
        else [(f (head l))::(map f (tail l))]
in
    map
;;
```

## Addition of Integers Represented as Variants

```text
letrec add : Int -> Int -> Int =
    lambda n : Int. lambda m : Int.
        case m of
            <pos = p> => add
                (case n of
                    <pos = p1> => <pos = (succ p1)> as Int
                    | <zero = p1> => <pos = (succ 0)> as Int
                    | <neg = 1> => <zero = true> as Int
                    | <neg = p1> => <neg = (pred p1)> as Int)
                <pos = (pred p)> as Int
            | <pos = 0> => n
            | <zero = true> => n
            | <neg = 0> => n
            | <neg = p> => add
                (case n of
                    <pos = p1> => <pos = (pred p1)> as Int
                    | <pos = 1> => <zero = true> as Int
                    | <zero = p1> => <neg = (succ 0)> as Int
                    | <neg = p1> => <neg = (succ p1)> as Int)
                <neg = (pred p)> as Int
in
    add <pos = 3> as Int <pos = 2> as Int
;;
```
