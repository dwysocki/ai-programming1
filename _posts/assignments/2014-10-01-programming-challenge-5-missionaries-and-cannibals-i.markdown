---
layout: post
category: assignments
title: "Programming Challenge #5 - \"Missionaries & Cannibals I\""
date: 2014-10-01T09:10:00-04:00
---

# Solution

{% highlight cl %}
[1]> (load "missionaries-cannibals.l")
;; Loading file missionaries-cannibals.l ...
;; Loaded file missionaries-cannibals.l
T
[2]> (mc)
*LEFT-BANK*     (M M M C C C B)
*RIGHT-BANK*    NIL            
(b m c)
*LEFT-BANK*     (M M C C)      
*RIGHT-BANK*    (B M C)        
(b m)
*LEFT-BANK*     (M M C C B M)  
*RIGHT-BANK*    (C)            
(b c c)
*LEFT-BANK*     (M M M)        
*RIGHT-BANK*    (C B C C)      
(b c)
*LEFT-BANK*     (M M M B C)    
*RIGHT-BANK*    (C C)          
(b m m)
*LEFT-BANK*     (M C)          
*RIGHT-BANK*    (C C B M M)    
(b m c)
*LEFT-BANK*     (M C B M C)    
*RIGHT-BANK*    (C M)          
(b m m)
*LEFT-BANK*     (C C)          
*RIGHT-BANK*    (C M B M M)    
(b c)
*LEFT-BANK*     (C C B C)      
*RIGHT-BANK*    (M M M)        
(b c c)
*LEFT-BANK*     (C)            
*RIGHT-BANK*    (M M M B C C)  
(b m)
*LEFT-BANK*     (C B M)        
*RIGHT-BANK*    (M M C C)      
(b m c)
*LEFT-BANK*     NIL            
*RIGHT-BANK*    (M M C C B M C)
good work!
NIL
[3]> (display-solution)
(B M C)
(B M)
(B C C)
(B C)
(B M M)
(B M C)
(B M M)
(B C)
(B C C)
(B M)
(B M C)
NIL
{% endhighlight %}

# Code

[Download]({{ site.baseurl }}/assignments/missionaries-cannibals.l)

{% highlight cl %}
(defun mc ()
  (establish-world)
  (init-move-list)
  (make-moves))

(defun establish-world ()
  "Initializes the states of the left and right banks."
  (setf *left-bank*  '(M M M C C C B)
	*right-bank* ()))

(defun init-move-list ()
  "Initializes *move-list* to the empty-list."
  (setf *move-list* ()))

(defun display-symbol-and-value (symbol)
  "Prints a symbol and the value it is bound to."
  (format t "~15A ~15A~%" symbol (eval symbol)))

(defun display-world ()
  "Displays the current state of the world."
  (mapcar #'display-symbol-and-value
	  '(*left-bank* *right-bank*)))

;;;;;;;;;;;;;;;
;;;; Moves ;;;;
;;;;;;;;;;;;;;;

(defun make-moves ()
  "Enters a REPL of sorts."
  (display-world)
  (cond
   ((goalp)
    (write-line "good work!")
    nil)

   ((feast-state-p)
    (write-line "yummy yummy yummy, I got Good in my tummy!!") ; wut?
    nil)

   (T
    (let ((m (read)))
      (if (applicable-p m)
	(progn
	  (perform-move m)
	  (make-moves))
	(progn
	  (write-line "move inapplicable")
	  nil))))))

(defun perform-move (move)
  "Performs a move based from the current bank to the other bank."
  (setf *move-list* (snoc move *move-list*))
  (if (equal (current-bank) *left-bank*)
    (move-lr move)
    (move-rl move)))

(defun move-lr (ml)
  "Moves a list of pieces from the left to the right bank."
  (when (null ml) (return-from move-lr))
  (move-lr-1 (car ml))
  (move-lr   (cdr ml)))

(defun move-lr-1 (move)
  "Moves a single piece from the left to the right bank."
  (setf *left-bank*  (remove move *left-bank* :count 1))
  (setf *right-bank* (snoc move *right-bank*)))

(defun move-rl (ml)
  "Moves a list of pieces from the right to the left bank."
  (when (null ml) (return-from move-rl))
  (move-rl-1 (car ml))
  (move-rl   (cdr ml)))

(defun move-rl-1 (move)
  "Moves a single piece from the right to the left bank."
  (setf *right-bank* (remove move *right-bank* :count 1))
  (setf *left-bank*  (snoc move *left-bank*)))

;;;;;;;;;;;;;;;;;;;;;;
;;;; Counting Fns ;;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun cannibal-count (l)
  "Counts the number of cannibals in the given list."
  (count 'C l))

(defun missionary-count (l)
  "Counts the number of missionaries in the given list."
  (count 'M l))


;;;;;;;;;;;;;;;;;;;;
;;;; Predicates ;;;;
;;;;;;;;;;;;;;;;;;;;

(defun goalp ()
  "Returns T when the goal state has been reached."
  (null *left-bank*))

(defun feast-state-p ()
  "Returns T when the feast state has been reached."
  (or (and (contains-missionaries-p *left-bank*)
	   (more-cannibals-p *left-bank*))
      (and (contains-missionaries-p *right-bank*)
	   (more-cannibals-p *right-bank*))))

(defun more-cannibals-p (bank)
  "Returns T if cannibals outnumber missionaries on the given bank."
  (> (cannibal-count   bank)
     (missionary-count bank)))

(defun contains-missionaries-p (l)
  "Returns T if the given list contains any missionaries, 'M."
  (member 'M l))

(defun contains-boat-p (l)
  "Returns T if the given list contains the boat 'B."
  (member 'B l))

(defun applicable-p (move)
  "Returns T if the given move is valid."
  (let ((current-bank   (current-bank))
	(number-in-boat (1- (length move))))
    (and (<= 1 number-in-boat 2) ; must have 1 or 2 people in boat
	 (contains-boat-p move)  ; move must include boat
	 ; must not move people who are not on the current bank
	 (>= (cannibal-count current-bank)   (cannibal-count move))
	 (>= (missionary-count current-bank) (missionary-count move)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; State Checkers ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-bank ()
  "Returns the current bank."
  (if (contains-boat-p *left-bank*)
    *left-bank*
    *right-bank*))

(defun display-solution ()
  "Displays the moves used in the last solution (or attempted solution)."
  (dolist (m *move-list*)
    (format T "~A~%" m)))

;;;;;;;;;;;;;;
;;;; Misc ;;;;
;;;;;;;;;;;;;;

(defun snoc (x the-list)
  "Appends x to the-list."
  (if (null the-list)
    (list x)
    (cons (car the-list)
	  (snoc x (cdr the-list)))))
{% endhighlight %}