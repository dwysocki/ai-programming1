---
layout: post
category: sessions
title: "Session Featuring Constructors"
date: 2014-09-08T09:35:49-04:00
---

[Download]({{ site.baseurl }}/sessions/constructors-session.txt)

```
[1]> (setf numbers '(1 2 3))
(1 2 3)
[2]> (setf letters '(a b c))
(A B C)
[3]> numbers
(1 2 3)
[4]> letters
(A B C)
[5]> (cons numbers letters)
((1 2 3) A B C)
[6]> (list numbers letters)
((1 2 3) (A B C))
[7]> (append numbers letters)
(1 2 3 A B C)
[8]> (list numbers (cdr numbers) (cddr numbers))
((1 2 3) (2 3) (3))
[9]> (append numbers (cdr numbers) (cddr numbers))
(1 2 3 2 3 3)
[10]> (bye)
```