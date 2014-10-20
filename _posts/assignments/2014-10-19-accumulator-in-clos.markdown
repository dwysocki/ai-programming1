---
layout: post
category: assignments
title: "Accumulator in CLOS"
date: 2014-10-19T23:14:20-04:00
---

[Download]({{ site.baseurl }}/assignments/clos-accumulator.l)

{% highlight cl %}
(defclass accumulator ()
  ((value :accessor accumulator-value :initform 0)))

(defmethod inc ((a accumulator) (i number))
  (setf (accumulator-value a)
        (+ (accumulator-value a) i))
  nil)

(defmethod dec ((a accumulator) (i number))
  (setf (accumulator-value a)
        (- (accumulator-value a) i))
  nil)

(defmethod reset ((a accumulator))
  (setf (accumulator-value a) 0)
  nil)
{% endhighlight %}