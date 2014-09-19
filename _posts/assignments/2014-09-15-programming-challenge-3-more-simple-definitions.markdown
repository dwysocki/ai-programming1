---
layout: post
category: assignments
title: "Programming Challenge #3 - \"More Simple Definitions\""
date: 2014-09-15T09:10:00-04:00
---

[`mychoice.l`]({{ site.baseurl }}/assignments/mychoice.l)

{% highlight cl %}
;; (1)

;; Simplified version of built-in function mapcar.
;; Accepts a function, and a list of arguments.
;; Returns a new list in which each element is the result of applying
;; the function to the corresponding element of the input list.
;;
;; This is done by building a list with cons, in which the car of each
;; cons cell is the function applied to the first element of the list,
;; and the cdr of the cons cell is the result of recursing on the cdr of
;; the list.
(defun mapcar* (fn l)
  (when l
    (cons (funcall fn (first l))
    (mapcar* fn (rest l)))))

;; DEMO
;; > (mapcar* #'- (list 1 2 3))
;; (-1 -2 -3)
;; > (mapcar* (lambda (x) (* x 2)) (list 1 2 3))
;; (2 4 6)


;; (2)

;; Newton's method for computing square roots (taken from SICP)
;; Requires some helper functions first

;; Average of two numbers
(defun average (x y)
  (/ (+ x y) 2))

;; Improve the guess by averaging guess and x/guess
(defun improve (guess x)
  (average guess (/ x guess)))

;; Determine whether guess is within a certain threshold of the answer
(defconstant *threshold* 0.001)
(defun good-enough? (guess x)
  (< (abs (- (expt guess 2) x)) *threshold*))

;; Compute the square root of x by applying Newton's method, starting
;; with the given guess, and improving it until it is good enough,
;; using the definitions of improve and good enough from above
(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
	       x)))

;; Compute the square root of x, by applying Newton's method, starting with
;; 1.0 as the guess
(defun sqrt* (x)
  (sqrt-iter 1.0 x))

;; DEMO
;; > (sqrt* 4)
;; 2.0
;; > (sqrt* 2)
;; 1.4142157


;; (3)

;; Compute the absolute value of x (taken from SICP)
;; Uses if for branching, the < predicate for determining if x is negative,
;; and the - function to make x positive when it's negative.
(defun abs* (x)
  (if (< x 0)
    (- x)
    x))

;; DEMO
;; > (abs* 100)
;; 100
;; > (abs* -100)
;; 100
;; > (abs* 0)
;; 0
{% endhighlight %}