---
layout: page
title: "Notes"
author: "Dan Wysocki"
---

# General

[Student Webpages](
  http://www.cs.oswego.edu/~blue/abc/SWS-Repository/F14/SWS-F14.html)

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

{% highlight ruby %}
for(ever)
  x = read()
  z = eval(x)
  print(z)
end
{% endhighlight %}

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

**Summary of some Referencers and Constructors**

`(nth NUMBER LIST)` returns the element in position `NUMBER` of the list `LIST`,
with indexing beginning at `0`.

`(list & SEQUENCE-OF-FORMS)` evaluates each `FORM` in the `SEQUENCE-OF-FORMS`
and then constructs a list from the values of the `FORMS`.

`(append & SEQUENCE-OF-FORMS)` evaluates each `FORM` in the `SEQUENCE-OF-FORMS`
and then constructs a list by concatenating the values of the `FORMS`.

**Evaluators**

An *evaluator* is a function which performs function evaluation!

In Lisp, `eval` is the basic evaluator, but `apply` and `funcall` serve as
alternative evaluators that often provide convenience and conceptual clarity.

`(eval FORM)` evaluates `FORM`.

`(apply FUNCTION LIST-OF-ARGUMENTS)` applies `FUNCTION` to `LIST-OF-ARGUMENTS`.

`(funcall FUNCTION & LIST-OF-ARGUMENTS)` applies `FUNCTION` to
`LIST-OF-ARGUMENTS`.

**Predicates**

A *predicate* is a function which takes some number of arguments and returns a
boolean value.


`COND`

1. Any number of cases
2. Any number of forms in a case
3. Only one case fires -- the first for which the first experssion in the case
   evaluates to non-`NIL`.
4. The value of the `COND` is the value of the last form evaluated in the case
   that is selected -- or `NIL` if no case fires.

{% highlight cl %}
(defun sign (n)
  (cond
    ((> n 0) 'positive)
    ((< n 0) 'negative)
    (T       'zero)))
{% endhighlight %}

**List Processing**

{% highlight cl %}
> (singletonp '(MONDAY))
T
> (singletonp '())
NIL
> (singletonp '(ONE TWO THREE))
NIL
{% endhighlight %}

{% highlight cl %}
;; Graci's definition
(defun singletonp (the-list)
  (cond
    ((null the-list)       NIL)
	((null (cdr the-list)) T)
	;; can just leave this part off to return NIL
	(T                     NIL)))

;; My definition
(defun singletonp (the-list)
  (and (car the-list) (not (cdr the-list))))
{% endhighlight %}

{% highlight cl %}
> (rac '(ONE TWO THREE))
THREE
> (rac '(MONDAY))
MONDAY
{% endhighlight %}

Thoughts:

- `rac` of `(A)` is `A`
- `rac` of `(A B C D)` is the `rac` of `(cdr '(A B C D)) => (B C D)`

{% highlight cl %}
;; Returns the last element of the list
(defun rac (the-list)
  (cond
    ((singletonp the-list) (car the-list))
    (T                     (rac (cdr the-list)))))
{% endhighlight %}

{% highlight cl %}
> (rdc '(A B C D))
(A B C)
> (rdc '(S))
()
{% endhighlight %}

Thoughts:

- `rdc` of `(x)` is `()`
- `rdc` of `(A B C D)` is the `cons` of `A`
  and the `rdc` of `(cdr '(A B C D)) => (B C D)`

{% highlight cl %}
;; Returns all but the rac of the list
;; (pronounced "rudder cdr")
(defun rdc (the-list)
  (cond
    ((singletonp the-list) ())
	(T                     (cons (car the-list) (rdc (cdr the-list))))))
{% endhighlight %}

- `snoc` of `x` and `()` is `(x)`
- `snoc` of `x` and `(a b c)` is the `cons` of the `car` of `(a b c)`
  with the `snoc` of `x` with the `cdr` of `(a b c)`

{% highlight cl %}
(defun snoc (x the-list)
  (cond
    ((null the-list) (cons x the-list))
    (T               (cons (car the-list) (snoc x (cdr the-list))))))
{% endhighlight %}

{% highlight cl %}
(defun sum (x)
  (if (null x)
    0
    (+ (car x) (sum (cdr x)))))
{% endhighlight %}

{% highlight cl %}
> (iota 6)
(1 2 3 4 5 6)
> (iota 0)
()
{% endhighlight %}

{% highlight cl %}
(defun iota (n)
  (cond
    ((= n 0) ())
    (T       (snoc n (iota (- n 1))))))
{% endhighlight %}

{% highlight cl %}
> (evenp 5)
NIL
> (evenp 6)
T
> (numberp 'FOUR)
NIL
> (numberp 4)
T
> (numberp '(4))
NIL
{% endhighlight %}
