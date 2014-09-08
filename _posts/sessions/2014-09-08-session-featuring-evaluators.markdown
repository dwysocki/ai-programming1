---
layout: post
category: sessions
title: "Session Featuring Evaluators"
date: 2014-09-08T10:04:30-04:00
---

[Download]({{ site.baseurl }}/sessions/evaluators-session.txt)

```
[1]> (expt 2 5)
32
[2]> (eval '(expt 2 5))
32
[3]> (apply (function expt) '(2 5))
32
[4]> (apply expt (2 5))

*** - SYSTEM::READ-EVAL-PRINT: variable EXPT has no value
The following restarts are available:
USE-VALUE      :R1      Input a value to be used instead of EXPT.
STORE-VALUE    :R2      Input a new value for EXPT.
ABORT          :R3      Abort main loop
Break 1 [5]> :a
[6]> (funcall (function expt) 2 5)
32
[7]> (funcall #'expt 2 5)
32
[8]> (funcall #'* 2 3 5 7)
210
[9]> (apply #'* '(2 3 5 7)
)
210
[10]> (eval '(* 2 3 5 7 11 13 17 19))
9699690
[11]> (bye)
```