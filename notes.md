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

**Keyword Arguments**

Arguments are sometimes given names! This is generally done either to provide
flexibility or to add integrity to the code.

**Rest Arguments**

A "rest" parameter will bind any "left over" items into a list for subsequent
use in the function. (demo next time)

{% highlight cl %}
(defun demorest (f &rest r)
  (format T "F = ~A~%" F)
  (format T "R = ~A~%" R))

> (demorest 1 2 3 4)
F = 1
R = (2 3 4)
NIL
> (demorest 'A 'B 'C 'D 'E 'F)
F = A
R = (B C D E F)
NIL

(defun name (&rest r)
  r)
> (name 'craig 'graci)
(CRAIG GRACI)
> (name 'jose 'aldo 'silva 'da 'costa)
(JOSE ALDO SILVA DA COSTA)
{% endhighlight %}

# Mapping Functions

{% highlight cl %}
;; mapcar which accepts 1 list argument ;;
(defun apply-to-all-1 (fun lst)
  (let ((f (car lst))
        (r (cdr lst)))
    (cond
      ((null lst) ())
      (T          (cons (funcall fun f)
                        (apply-to-all-1 fun r))))))
; demo
> (apply-to-all-1 #'car  '((A B) (C D) (E F)))
(A C E)
> (apply-to-all-1 #'cadr '((A B) (C D) (E F)))
(B D F)
> (apply-to-all-1 #'iota (iota 5))
((1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5))

;; mapcar which accepts 2 list arguments ;;
(defun apply-to-all-2 (fun lst1 lst2))
; demo
> (apply-to-all-2 #'expt '(1 2 3 4 5) '(2 2 2 2 2))
(1 4 9 16 25)
> (apply-to-all-2 #'list '(a b c d) '(1 2 3 4))
((A 1) (B 2) (C 3) (D 4))
> (apply-to-all-2 #'cons '(a b c d) '(1 2 3 4))
((A . 1) (B . 2) (C . 3) (D . 4))
{% endhighlight %}

In general, we use `mapcar`.

`mapcar`:

- takes
    - a function of `N` arguments
    - `N` lists of equal lengths
- applies the function to each of the
    - first elements of the lists
    - second elements of the lists
    - ...
    - last elements of the lists
- returns a list of the values computed

{% highlight cl %}
[1]> (mapcar #'- '(1 2 3))
(-1 -2 -3)
[2]> (mapcar #'- '(1 2 3) '(4 5 6))
(-3 -3 -3)
[3]> (mapcar #'- '(1 2 3) '(4 5 6) '(7 8 9))
(-10 -11 -12)
{% endhighlight %}

**Association Lists**

An association list (`a-list`) is a list of cons-cell objects, each of which
represents a key/datum pair by means of its `car` and its `cdr` -- or by means
of its `cdr` and its `car` ... depending on how you look at it.

Remark: The function `mapcar` provides a nice mechanism for constructing
`a-list`s. The functions `assoc` and `rassoc` are nice functions for
referencing data in `a-lists`.

Notes:

1) The functions `assoc` and `rassoc` make use of the function `eq` by default.

2) In real applications with large data sets you tend to use hash tables
   rather than `a-lists`.

**Property Lists**

A property list or `p-list` is a simply a list of pairs associated with
a symbol.

Functions that tend to be used for `p-list` processing:

- `get`
    - satan's evil transmutation function (`setf`)
- `symbol-plist`
- `remprop`

**State Spaces**

An *object* is a set of properties together with a set of behaviors.

The *state of an object* is a set of bindings for the properties of the object.

A *world* is a collection of objects.

The *state of a world* is a set of bindings for all of the objects in the
world.

The *state space of an object* is the set of all possible sets of bindings for
the properties of the object.

The *state space of a world* is the set of all possible sets of bindings for
all of the all of the objects in the world.

A *state space operator* is an operator that maps one state of the world to
another state.

A *state space program description* is a triple consisting of

1. `I`: a set of initial states
2. `G`: a set of goal states
3. `O`: a set of state spaces operators

All in the context of `S`, some state space that fits the program.

A *state space problem solution* is a triple consisting of

1. `i`: one of the initial states in `I`
2. `g`: one of the goal states in `G`
3. `x`: a finite sequence of state space operators that transforms `i` into `g`

A *state space search* is the problem of finding a sequence of state space
operators that transforms an initial state into a goal state.

Example:
Three coins problem

Statement:
Given three coins arranged `(T T H)`, make them all the same in exactly three
moves, where a move amounts to turning a coin over.

Objects of "the" state space:

The state space will consist of four objects, each with just one property

- coin `L` (leftmost coin):
with one property -- its top face
(which will have either `T` or `H` for its value)
- coin `C` (centermost coin)
- coin `R` (rightmost coin)
- counter `V`, which takes on non-negative values

Representation of a state:

We will represent in the form `<LCR V>`, where each of `L`, `C`, and `R` is
either `T` or `H`, and `V` is a non-negative integer.

- initial state set: `I = { <TTH 0> }`
- state space operators: `O = { OL, OC, OR }`
    - `OL = <LCR V> -> <L'CR V+1>`
    - `OC = <LCR V> -> <LC'R V+1>`
    - `OR = <LCR V> -> <LCR' V+1>`
- goal state set: `G = { <TTT 3>, <HHH 3> }`

![3 coin problem](
  {{ site.baseurl }}/note_data/graphviz/state_space_3coins.svg)

**State Space Search**

Two basic forms

- Breadth-first search (BFS)
- Depth-first search (DFS)

Implementation idea

In either case, the idea is to maintain two lists of noes, an unexplored lists,
which nodes not yet examined are placed and an explored list, on which nodes
that have been examined are placed.

To find a solution `(i, g, x)` to a state space problem defined by `(I, G, O)`
do...

- establish a list of unexplored nodes called `unexplored`, and place all of
  the initial states on it.
- establish a list, called `explored` and bind it to the empty list.
- perform the following iteration:

~~~
repeat
if (UNEXPLORED is empty) then
    report "NO SOLUTION"
    BREAK
end

bind ESTATE to the next UNEXPLORED state

if (ESTATE has been explored) then
    CONTINUE
else if (ESTATE is the goal) then
    report "SOLUTION FOUND"
    return ESTATE
else
    generate the children of ESTATE, and call them KIDS
    place kids on UNEXPLORED
    place ESTATE on EXPLORED
end
~~~

Prior to iterating:

- `e` - `nil`
- `c` - `nil`
- `u` - `(<TTH 0>)`
- `x` - `nil`

After 1 pass:

- `e` - `<TTH 0>`
- `c` - `(<HTH 1>, <TTH 1>, <TTT 1>)`
- `u` - `(<HTH 1>, <TTH 1>, <TTT 1>)`
- `x` - `(<TTH 0>)`

After 2 passes:

- `e` - `<HTH 1>`
- `c` - `(<HHH 2>, <THH 2>, <HTT 2>)`
- `u` - `(<TTH 1>, <TTT 1>, <HHH 2>, <THH 2>, <HTT 2>)`
- `x` - `(<TTH 0>, <HTH 1>)`


# BFS vs DFS (implementation)

*Breadth-first search* is implemented by maintaining *unexplored* as a *queue*
(first in, first out list, or FIFO).

*Depth-first search* is implemented by maintaining *unexplored* as a *stack*
(last in first out list, or LIFO).

# The Water Jug Problem

A simple instance...

You have a sink, with an unlimited supply of water.
You have two unmarked jugs, a 4-gallon jug, and a 3-gallon jug.
How can you get exactly two gallons in the four gallon jug?

Solution:

- fill 3 gallon jug with sink
- empty 3 gallon jug into 4 gallon jug
- fill 3 gallon jug with sink
- fill 4 gallon jug with 3 gallon jug
- empty 4 gallon jug into sink
- empty 3 gallon jug into 4 gallon jug

State space representation of the water jug problem.

Let `x` represent the number of gallons in the 4-gallon jug.
Let `y` represent the number of gallons in the 3-gallon jug.

The state space points:
`{ (x, y) | x ϵ { 0, 1, 2, 3, 4 } and y ϵ { 0, 1, 2, 3 } }`.

Initial state set:
`{ (0, 0) }`

Goal state set:
`{ (2, 0), (2, 1), (2, 2), (2, 3) }`

Operations:

1. Fill jug one:

   `(x, y) | x < 4 -> (4, y)`
2. Fill jug two:

   `(x, y) | y < 3 -> (x, 3)`
3. Empty jug one:

   `(x, y) | x > 0 -> (0, y)`
4. Empty jug two:

   `(x, y) | y > 0 -> (x, 0)`
5. Pour from jug two to fill jug one:

   `(x, y) | x + y >= 4 and x < 4 and y > 0 -> (4, y-(4-x))`
6. Pour from jug one to fill jug two:

   `(x, y) | x + y >= 3 and x > 0 and y < 3 -> (x-(3-y), 3)`
7. Dump from jug two into jug one:

   `(x, y) | x < 4 and y > 0 -> (x+y, 0)`
8. Dump from jug one into jug two:

   `(x, y) | x > 0 and y < 3 -> (0, x+y)`

I like to think of there being 2 general functions:

1. `fillFrom(x, y)`,
   where `x,y` can be either jug, and `y` can also be the sink

2. `emptyInto(x, y)`,
   where `x,y` can be either jug, and `x` can also be the sink

**CLOS**

The Common Lisp Object System.

Modelling a coin.

{% highlight cl %}
(defclass coin ()
  ((face    :accessor coin-face    :initarg :face :initform 'h)
   (history :accessor coin-history                :initform ())))

(defmethod display ((c coin))
  (format t "[~A,~A]"
    (write-to-string (coin-face    c))
    (write-to-string (coin-history c))))

(defmethod to-string ((c coin))
  (format nil "[~A,~A]"
    (write-to-string (coin-face    c))
    (write-to-string (coin-history c))))

(defmethod flip ((c coin))
  (setf (coin-face c)
        (nth (random 2) '(h t)))
  (setf (coin-history c)
        (append (coin-history c) (list (coin-face c))))
  nil)

(defmethod turn ((c coin))
  (if (eq (coin-face c) 't)
    (setf (coin-face c) 'h)
    (setf (coin-face c) 't))
  (setf (coin-history c)
        (append (coin-history c) (list (coin-face c))))

  nil)

(defmethod forget ((c coin))
  (setf (coin-history c) ()))

(defmethod flip-for-h ((c coin))
  (flip c)
  (when (not (eq (coin-face c) 'h))
    (flip-for-h c)))

(defmethod flip-for-hh ((c coin))
  (flip-for-h c)
  (flip c)
  (when (not (eq (coin-face c) 'h))
    (flip-for-hh c)))

(defmethod flip-for-hhh ((c coin))
  (flip-for-hh c)
  (flip c)
  (when (not (eq (coin-face c) 'h))
    (flip-for-hhh c)))

(defmethod flip-for-n-h ((c coin) (n integer))
  (if (= n 1)
    (flip-for-h c)
    (progn
      (flip-for-n-h c (1- n))
      (flip c)
      (when (not (eq (coin-face c) 'h))
        (flip-for-n-h c n)))))

(defun times-to-h ()
  (let ((c (make-instance 'coin)))
    (flip-for-h c)
    (length (coin-history c))))

(defun times-to-h-iter (i acc)
  (if (> i 0)
    (times-to-h-iter (1- i) (+ acc (times-to-h)))
    acc))

(defun times-to-h-avg (N)
  (/ (times-to-h-iter N 0)
     N))
{% endhighlight %}
