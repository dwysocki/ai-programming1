;;;; lp.l - LIST-PROCESSING ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PICK ;;
(defun pick (the-list)
  (when the-list
      (let* ((length (length the-list))
             (index  (random length))
             (choice (nth index the-list)))
        choice)))

;; SINGLETONP - RAC - RDC - SNOC - PALINDROMEP ;;
(defun singletonp (the-list)
  (and the-list
       (not (cdr the-list))))

(defun rac (the-list)
  (cond
    ((null the-list)       NIL)
    ((singletonp the-list) (car the-list))
    (T                     (rac (cdr the-list)))))

(defun rdc (the-list)
  (cond
   ((null the-list)       ())
   ((singletonp the-list) ())
   (T                     (cons (car the-list)
                                 (rdc (cdr the-list))))))

(defun rdcdr (the-list)
  (rdc (cdr the-list)))

(defun snoc (x the-list)
  (cond
   ((null the-list) (list x))
   (T               (cons (car the-list)
                          (snoc x (cdr the-list))))))

(defun palindromep (the-list)
  (cond
    ((null the-list) T)
    (T               (and (equal (car the-list)
                                 (rac the-list))
                          (palindromep (rdcdr the-list))))))

;; SUM - PRODUCT ;;
(defun sum (the-list)
  (cond
   ((null the-list) 0)
   (T               (+ (car the-list)
                       (sum (cdr the-list))))))

(defun product (the-list)
  (cond
   ((null the-list) 1)
   (T               (* (car the-list)
                       (product (cdr the-list))))))

;; IOTA - GENERATE ;;
(defun iota (n)
  (cond
   ((zerop n) ())
   (T         (snoc n (iota (1- n))))))

(defun generate (n lo)
  (cond
   ((zerop n) ())
   (T         (cons lo
                    (generate (1- n) lo)))))

;; FACTORIAL - POWER ;;
(defun factorial (n)
  (product (iota n)))

(defun power (base expt)
  (product (generate expt base)))

;; FILTER-IN - FILTER-OUT ;;
(defun filter-in (pred the-list)
  (let ((element (car the-list)))
    (cond
     ((null the-list)        ())
     ((funcall pred element) (cons element
                                   (filter-in pred
                                              (cdr the-list))))
     (T                      (filter-in pred (cdr the-list))))))

(defun filter-out (pred the-list)
  (let ((element (car the-list)))
    (cond
     ((null the-list)        ())
     ((funcall pred element) (filter-out pred (cdr the-list)))
     (T                      (cons element
                                   (filter-out pred
                                              (cdr the-list)))))))

;; TAKE-FROM ;;
(defun take-from (obj the-list)
  (let ((element (car the-list)))
    (cond
     ((null the-list)     ())
     ((equal obj element) (take-from obj (cdr the-list)))
     (T                   (cons element (take-from obj (cdr the-list)))))))

;; RANDOM-PERMUTATION ;;
(defun drop-first (obj the-list)
  (let ((element (car the-list)))
    (cond
     ((null the-list)     ())
     ((equal obj element) (cdr the-list))
     (T                   (cons element
                                (drop-first obj
                                            (cdr the-list)))))))

(defun random-permutation (the-list)
  (when the-list
    (let* ((element   (pick the-list))
           (remainder (drop-first element the-list)))
      (cons element
            (random-permutation remainder)))))
