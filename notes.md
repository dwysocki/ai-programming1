---
layout: page
title: "Notes"
author: "Dan Wysocki"
---

# Introduction

An *object* in Lisp is either an *atom*, a *list*, an *instance of a class*, or
other stuff (but really all you need are atoms and lists, what is this, Java?).

An *atom* is an "indecomposable" Lisp object (but just like real atoms, you
actually can decompose them).

- *integer atoms*
    - `-4`
    - `3193`
- *real atoms*
    - `3.14`
    - `000.2`
- *string atoms*
    - `"one two THREE"`
    - `" "`
    - `""`
- *symbolic atoms*
    - `R2D2`
    - `HERE-THERE-EVERYWHERE`
    - `SMALL`

The featured data type in Lisp is the *list*. A *list* is simply a sequence of
Lisp objects.

- `(2 3 5 7)`
- `(SINGLE-DIGIT-PRIMES)`
- `(1 RED 2 YELLOW 3 BLUE)`

The key to understanding Lisp programming lies in a deep understanding of
*forms*.

A *form* is an expression which can be evaluated by the Lisp interpreter.

The read-eval-print loop (REPL)

```ruby
for(ever)
  x = read()
  z = eval(x)
  print(z)
end
```

[Introductory Lisp Session](
    /ai-programming1/sessions/2014/08/29/introductory-session.html)

**Constant forms**

A *constant* is an expression which evaluates to itself.

Numbers, character strings, characters, and booleans are among the most commonly
used constant forms.

- numbers
    - ...
- character strings
    - ...
- characters
    - `#\A`
    - `#\z`
    - `#\4`
- booleans
    - `(NOT NIL) => T`
    - `NIL`

**Variable forms**

A *variable* is a symbol which is denotationally bound to some Lisp object.

The symbol `PI` is pre-bound in Common Lisp.

The symbol `PIE` became a variable in the sample session once it was bound to
`CHERRY`.

Variables evaluate to the values to which they are bound.

**Standard forms**

A *standard form* is a list which represents a function application in the
following sense: the first element of the list represents a function; each
remaining element of the list represents an *unevaluated argument* to that
function.

*Standard form evaluation* is the following two-step process for evaluating a
functional form:

1. the argument forms are evaluated

2. the function is applied to the values obtained in the first step

Examples ...

- `+` and `max` are predefined standard forms

- `double` from the
  [introductory lisp session](
      /ai-programming1/sessions/2014/08/29/introductory-session.html)
  is a standard form defined by the programmer

**Special forms**

A *special form* is like a standard form, except that one or more of the
arguments to the function are not evaluated prior to application of the
function.

Examples ...

- `QUOTE`: `(quote whatever)`, `whatever` is not evaluated

- `SETF`: `(setf symbol object)`, if the first argument is a symbol, it is not
  evaluated; the second argument is evaluated

- `DEFUN`: `(defun lambda-list sequence-of-forms)`, neither of the two arguments
  are evaluated prior to applying `defun`

**Simple Numeric List Processing**

Example numeric operators ...

- `+`

- `-`

- `*`

- `/`

[Simple Numeric Session](
    /ai-programming1/sessions/2014/09/03/simple-numeric-session.html)

# List Processing

**Icons of List Processing in Lisp**

The `CAR` of a nonempty list is the `FIRST` element of the list. Stands for
"contents of `A` register".

The `CDR` of a nonempty list is the `REST` of the elements of the list.
Stands for "contents of `D` register".

The `CONS` of an object `O` and a list `L` is the list whose `CAR` is `0` and
whose `CDR` is `L`.

Abstract Session: featuring `CAR` and `CDR`.

1) What is the `CAR` of `(BLUE RED YELLOW)`?

  - `BLUE`

2) What is the `CDR` of `(BLUE RED YELLOW)`?

  - `(RED YELLOW)`

3) What is the `CAR` of `((1 2) BUCKLE MY SHOE)`?

  - `(1 2)`

4) What is the `CDR` of `((1 2) BUCKLE MY SHOE)`?

  - `(BUCKLE MY SHOE)`

5) What is the `CAR` of `("SUNSHINE")`?

  - `"SUNSHINE"`

6) What is the `CDR` of `("SUNSHINE")`?

  - `()` or `NIL`

Abstract Session: featuring `CONS`.

1) What is the `CONS` of `ESPRESSO` and `(LATTE CAPPUCCINO)`?

  - `(ESPRESSO LATTE CAPPUCCINO)`

2) What is the `CONS` of `(A B C)` and `(1 2 3)`?

  - `((A B C) 1 2 3)`

3) What is the `CONS` of `PETUNIA` and `()`?

  - `(PETUNIA)`

**Referencers and Constructors**

A *referencer* is a form which returns a reference to a part of a given
structure.

A *constructor* is a form which returns a structure constructed from some
number of Lisp objects.

*Note: The "icons of list processing" are all you actually need to do list
       processing in Lisp. The rest are for convenience.*

