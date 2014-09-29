;;;; SIMPLE FUNCTION DEFINITIONS TO OPERATE ON STANDARD PLAYING CARDS THAT
;;;; ARE REPRESENTED AS PAIRS ... (RANK SUIT)
;;;; (2 CLUB) (QUEEN HEART)

(defconstant *suit* '(club diamond heart spade))
(defconstant *rank* '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun take-random (x)
  (let* ((l (length x))
	 (n (random l)))
    (nth n x)))

(defun dc (c)
  (let* ((rank (take-random *rank*))
         (suit (take-random *suit*))
	 (random-card (list rank suit)))
    "I'm sorry, John McCarthy"
    (set c random-card)
    "I'm sorry, Alonzo Church"
    NIL))

(defun rank (c)
  (first c))

(defun suit (c)
  (second c))

(defun red-p (c)
  (let ((rank (rank c)))
    (or (eq rank 'heart)
	(eq rank 'diamond))))

(defun black-p (c)
  (not (red-p c)))

(defun high-card-point (c)
  (let ((suit (suit c)))
    (cond
     ((eq suit 'ace)   4)
     ((eq suit 'king)  3)
     ((eq suit 'queen) 2)
     ((eq suit 'jack)  1)
     (T                0))))

;; second session

(defconstant *suits* '(club diamond heart spade))
(defconstant *ranks* '(10 jack queen king ace))

(defun new-suit (s)
  (mapcar #'list *ranks* (make-list 5 :initial-element s)))

(defun new-deck ()
  (mapcan #'new-suit *suits*))
