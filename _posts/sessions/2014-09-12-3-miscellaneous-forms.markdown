---
layout: post
category: sessions
title: "3 Miscellaneous Forms"
date: 2014-09-12T09:32:07-04:00
---

[Download]({{ site.baseurl }}/sessions/3-misc-forms.txt)

```
[1]> (setf a 4)
4
[2]> a
4
[3]> (setf a 5)
5
[4]> a
5
[5]> (defconstant b 6)
B
[6]> b
6
[7]> (setf b 7)

*** - SETQ: B is a constant, may not be used as a variable
The following restarts are available:
USE-VALUE      :R1      Input a value to be used instead.
ABORT          :R2      Abort main loop
Break 1 [8]> :a
[9]> (defconstant *c* 19)
*C*
[10]> *c*
19
[11]> (random 6)
1
[12]> (random 6)
2
[13]> (random 6)
0
[14]> (random 6)
0
[15]> (random 6)
4
[16]> (random 6.0)
5.1253653
[17]> (random 100.0)
66.32515
[18]> (setf animal 'dog)
DOG
[19]> animal
DOG
[20]> (set animal 'noomi)
NOOMI
[21]> animal
DOG
[22]> dog
NOOMI
[23]> (eval animal)
NOOMI
[24]> (bye)
```