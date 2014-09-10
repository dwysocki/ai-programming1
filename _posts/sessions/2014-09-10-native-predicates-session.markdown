---
layout: post
category: sessions
title: "Native Predicates Session"
date: 2014-09-10T09:41:33-04:00
---

[Download]({{ site.baseurl }}/sessions/native-predicates-session.txt)

```
[1]> (< 3 5)
T
[2]> (<= 3 3)
T
[3]> (< 3 5 7 8 9)
T
[4]> (< 3 5 5 7 9 11)
NIL
[5]> (eq 'a 'b)
NIL
[6]> (eq 'a 'a)
T
[7]> (eq '(cat) '(dog))
NIL
[8]> (eq '(cat) '(cat))
NIL
[9]> (equal '(cat) '(cat))
T
[10]> (= 4 4)
T
[11]> (equal '(one two) '(one two))
T
[12]> (or t nil)
T
[13]> (or nil nil nil)
NIL
[14]> (or (> 3 5) (= 3 7))
NIL
[15]> (or nil nil nil nil t)
T
[16]> (and t t t t t t)
T
[17]> (and t t t t nil t)
NIL
[18]> (not (= 3 5))
T
[19]> (not (= 5 5))
NIL
[20]> (bye)
```