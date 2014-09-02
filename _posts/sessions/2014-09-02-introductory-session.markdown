---
layout: post
title: "Introductory Session"
category: sessions
date: 2014-09-02T18:15:52-04:00
---
[Download](/ai-programming1/sessions/introductory-session.txt)

```common-lisp
[1]> 15
15
[2]> "Common Lisp with Objects"
"Common Lisp with Objects"
[3]> pie

*** - SYSTEM::READ-EVAL-PRINT: variable PIE has no value
The following restarts are available:
USE-VALUE      :R1      Input a value to be used instead of PIE.
STORE-VALUE    :R2      Input a new value for PIE.
ABORT          :R3      Abort main loop
Break 1 [4]> :a ; (ABORT)
[5]> pi
3.1415926535897932385L0
[6]> (+ 2 3 5 7)
17
[7]> (3 + 7)

*** - EVAL: 3 is not a function name; try using a symbol instead
The following restarts are available:
USE-VALUE      :R1      Input a value to be used instead.
ABORT          :R2      Abort main loop
Break 1 [8]> :a
[9]> (* (+ 3 6 9) (- 8 5))
54
[10]> (double 5)

*** - EVAL: undefined function DOUBLE
The following restarts are available:
USE-VALUE      :R1      Input a value to be used instead of (FDEFINITION 'DOUBLE).
RETRY          :R2      Retry
STORE-VALUE    :R3      Input a new value for (FDEFINITION 'DOUBLE).
ABORT          :R4      Abort main loop
Break 1 [11]> :a
[12]> (quote pie)
PIE
[13]> (quote (double 5))
(DOUBLE 5)
[14]> 'pie
PIE
[15]> '(double 5)
(DOUBLE 5)
[16]> (setf pie 'cherry)
CHERRY
[17]> pie
CHERRY
[18]> (setf pie apple)

*** - SETQ: variable APPLE has no value
The following restarts are available:
USE-VALUE      :R1      Input a value to be used instead of APPLE.
STORE-VALUE    :R2      Input a new value for APPLE.
ABORT          :R3      Abort main loop
Break 1 [19]> :a
[20]> (setf dozen 12)
12
[21]> dozen
12
[22]> (defun double (x) (* x 2))
DOUBLE
[23]> (double 5)
10
[24]> (double dozen)
24
[25]> (double 'dozen)

*** - *: DOZEN is not a number
The following restarts are available:
USE-VALUE      :R1      Input a value to be used instead.
ABORT          :R2      Abort main loop
Break 1 [26]> :a
[27]> (double pi)
6.283185307179586477L0
[28]> (double pie)

*** - *: CHERRY is not a number
The following restarts are available:
USE-VALUE      :R1      Input a value to be used instead.
ABORT          :R2      Abort main loop
Break 1 [29]> :a
(bye)
```