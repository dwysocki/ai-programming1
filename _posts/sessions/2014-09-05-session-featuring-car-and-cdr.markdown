---
layout: post
category: sessions
title: "Session Featuring CAR and CDR"
date: 2014-09-05T09:46:15-04:00
---

[Download]({{ site.baseurl }}/sessions/car-cdr-session.txt)

```
[1]> (car (quote (blue red yellow)))
BLUE
[2]> (car '(blue red yellow))
BLUE
[3]> (cdr '(blue red yellow))
(RED YELLOW)
[4]> (car '((1 2) buckle my shoe))
(1 2)
[5]> (cdr '((1 2) buckle my shoe))
(BUCKLE MY SHOE)
[6]> (car '("SUNSHINE"))
"SUNSHINE"
[7]> (cdr '("SUNSHINE"))
NIL
(bye)
```