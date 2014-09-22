---
layout: post
category: sessions
title: "Keyword Arguments Session"
date: 2014-09-22T10:04:08-04:00
---

[Download]({{ site.baseurl }}/sessions/keyword-arguments-session.txt)

{% highlight cl %}
[1]> (setf numbers '(1 2 3 4 5 6 7 8 9 2 4 6 8 3 6 9 4 8 5 6 7 8 9))
(1 2 3 4 5 6 7 8 9 2 4 6 8 3 6 9 4 8 5 6 7 8 9)
[2]> (remove 4 numbers)
(1 2 3 5 6 7 8 9 2 6 8 3 6 9 8 5 6 7 8 9)
[3]> numbers
(1 2 3 4 5 6 7 8 9 2 4 6 8 3 6 9 4 8 5 6 7 8 9)
[4]> (remove 4 numbers :count 1) ; removes 1 occurence of 4
(1 2 3 5 6 7 8 9 2 4 6 8 3 6 9 4 8 5 6 7 8 9)
[5]> (remove 4 numbers :count 2) ; removes 2 occurences of 4
(1 2 3 5 6 7 8 9 2 6 8 3 6 9 4 8 5 6 7 8 9)
[6]> (setf letters '(c r a i g g r a c i))
(C R A I G G R A C I)
[7]> letters
(C R A I G G R A C I)
[8]> (remove 'g letters)
(C R A I R A C I)
[9]> (remove 'g letters :count 1)
(C R A I G R A C I)
[10]> letters
(C R A I G G R A C I)
[11]> (remove 'g letters :test #'equal)
(C R A I R A C I)
[12]> (remove 'g letters :test #'eq)
(C R A I R A C I)
[13]> (remove 'g letters :test #'=)

*** - =: G is not a number
The following restarts are available:
USE-VALUE      :R1      Input a value to be used instead.
ABORT          :R2      Abort main loop
Break 1 [14]> :a
[15]> (bye)
Bye.
{% endhighlight %}