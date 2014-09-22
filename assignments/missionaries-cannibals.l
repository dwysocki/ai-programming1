(defun mc ()
  (establish-world)
  (init-move-list)
  (make-moves))

(defun establish-world ()
  (setf *left-bank*  ()
	*right-bank* ()
	*move-list*  ()))

(defun make-moves ()
  (display-world)
  (cond
   ((goalp)
    (write-line "good work!")
    nil)

   ((feast-state-p)
    (write-line "yummy yummy yummy, I got Good in my tummy!!")
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
  (setf *move-list* (snoc move *move-list*))
  (if (equal (current-bank) *left-bank*)
    (move-lr move)
    (move-rl move)))

(defun move-lr (ml)
  (when (null ml) (return-from move-lr))
  (move-lr-1 (car m1))
  (move-lr   (cdr m1)))

(defun move-rl (ml)
  (when (null ml) (return-from move-rl))
  (move-rl-1 (car ml))
  (move-rl   (cdr ml)))


;;;;;;;;;;;;;;
;;;; Misc ;;;;
;;;;;;;;;;;;;;

(defun snoc (x the-list)
  (if (null the-list)
    (list x)
    (cons (car the-list)
	  (snoc x (cdr the-list)))))
