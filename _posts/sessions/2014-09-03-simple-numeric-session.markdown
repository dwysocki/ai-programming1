---
layout: post
category: sessions
title: "Simple Numeric Session"
date: 2014-09-03T09:56:34-04:00
---

[Download]({{ site.baseurl }}/sessions/simple-numeric-session.txt)

```common-lisp
[1]> (+ 1 2 3 4 5 6 7 8 9 10)
55
[2]> (* 1 2 3 4 5 6 7 8 9 10)
3628800
[3]> (- 2 2 2)
-2
[4]> (/ 3.0 5)
0.6
[5]> (/ 3 5)
3/5
[6]> (sqrt 100)
10
[7]> (expt 7 50)
1798465042647412146620280340569649349251249
[8]> ; circumference of a radius 10 circle
(* 2 pi 10)
62.83185307179586477L0
[9]> ; area of a radius 15 circle
(* pi (expt 15 2))
706.8583470577034787L0
[10]> ; area of a radius 17.2 circle
(* pi (expt 17.2 2))
929.4089
[11]> ; area of a "ring" delimited by concentric circles of radii 15 and 17.2
(- (* pi (expt 17.2 2)) (* pi (expt 15 2)))
222.55052
(bye)
```