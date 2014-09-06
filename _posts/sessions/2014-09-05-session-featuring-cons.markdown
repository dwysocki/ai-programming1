---
layout: post
category: sessions
title: "Session Featuring CONS"
date: 2014-09-05T09:54:41-04:00
---

[Download]({{ site.baseurl }}/sessions/cons-session.txt)

```
[1]> (cons espresso (latte cappuccino))

*** - SYSTEM::READ-EVAL-PRINT: variable ESPRESSO has no value
The following restarts are available:
USE-VALUE      :R1      Input a value to be used instead of ESPRESSO.
STORE-VALUE    :R2      Input a new value for ESPRESSO.
ABORT          :R3      Abort main loop
Break 1 [2]> :a
[3]> (cons 'espresso '(latte cappuccino))
(ESPRESSO LATTE CAPPUCCINO)
[4]> (cons '(a b c) '(1 2 3))
((A B C) 1 2 3)
[5]> (cons 'petunia ())
(PETUNIA)
[6]> (cons 'petunia NIL)
(PETUNIA)
[7]> (cons '(a b c) (1 2 3))

*** - EVAL: 1 is not a function name; try using a symbol instead
The following restarts are available:
USE-VALUE      :R1      Input a value to be used instead.
ABORT          :R2      Abort main loop
Break 1 [8]> :a
[9]> (bye)
```
