(load "util.l")

(defconstant *limit* 25)
(defconstant *population-size* 100)
(defconstant *selection-size* 8)

(defvar *select-demo* nil)

(defmethod rbg ()
  (let ((r (random 3)))
    (case r
      (0 'R)
      (1 'B)
      (2 'G))))

(defmethod rbg-string (&optional (n *limit*))
  (when (not (zerop n))
    (cons (rbg)
          (rbg-string (1- n)))))

(defmethod mutation ((rbg-str list))
  (let* ((p (random (length rbg-str)))
         (q (others '(r b g) (nth p rbg-str))))
    (change rbg-str (pick q) p)))

(defmethod crossover ((mother list) (father list))
  (let ((pos (1+ (random (length mother)))))
    (append (take pos mother)
            (drop pos father))))

(defmacro defitness (name symbol)
  `(defmethod ,name ((rbg-str list))
     (count ,symbol rbg-str)))

(defitness fitness-r 'r)
(defitness fitness-b 'b)
(defitness fitness-g 'g)

;; classes

(defvar *fitness* #'fitness-b)
(defclass individual ()
  ((rbg-string :accessor individual-rbg-string :initarg :rbg-string)
   (fitness    :accessor individual-fitness    :initarg :fitness)
   (number     :accessor individual-number     :initarg :number)))

(defmethod new-individual ((n number) (rbg-str list))
  (make-instance 'individual
      :rbg-string rbg-str
      :fitness    (funcall *fitness* rbg-str)
      :number     n))

(defmethod random-individual ()
  (new-individual 0 (rbg-string)))

(defmethod display ((i individual))
  (format t "~6@<~D~>~A ~2D~%"
            (individual-number     i)
            (individual-rbg-string i)
            (individual-fitness    i)))

(defmacro defitness-individual (name)
  `(defmethod ,name ((i individual))
     (,name (individual-rbg-string i))))

(defitness-individual fitness-r)
(defitness-individual fitness-b)
(defitness-individual fitness-g)

(defclass population ()
  ((individuals :accessor population-individuals :initarg :individuals)
   (generation  :accessor population-generation  :initarg :generation
                                                 :initform 0)))

(defmethod size ((p population))
  (length (population-individuals p)))

(defmethod display ((p population))
  (format t "Generation ~D population...~%" (population-generation p))
  (mapcar #'display (population-individuals p))
  nil)

(defmethod initial-individuals (&optional (n *population-size*))
  (when (> n 0)
    (cons (new-individual n (rbg-string))
          (initial-individuals (1- n)))))

(defmethod initial-population ()
  (make-instance 'population :individuals (reverse (initial-individuals))))

(defmethod average ((numbers list))
  (let ((sum (apply #'+ numbers))
        (num (length numbers)))
    (float
     (/ sum
        num))))

(defmethod average ((p population))
  (average (mapcar #'individual-fitness
                   (population-individuals p))))

(defmethod select-individual ((p population))
  (let* ((candidates (select-individuals p))
         (mfi        (most-fit-individual candidates)))
    (when *select-demo*
      (select-demo-helper candidates mfi))
    mfi))

(defmethod select-random-individual ((individuals list))
  (nth (random (length individuals))
       individuals))

(defmethod select-random-individual ((p population))
  (select-random-individual (population-individuals p)))

(defmethod select-individuals ((p population)
                               &optional (n *selection-size*))
  (when (> n 0)
    (cons (select-random-individual p)
          (select-individuals p (1- n)))))

(defmethod most-fit-pair ((i1 individual) (i2 individual))
  (let ((f1 (funcall *fitness* i1))
        (f2 (funcall *fitness* i2)))
    (if (> f1 f2)
      i1
      i2)))

(defmethod most-fit-individual ((individuals list))
  (reduce #'most-fit-pair individuals))

(defmethod select-demo-helper ((individuals list) (i individual))
  (format t "the sample of individuals ...~%")
  (mapcar #'display individuals)
  (format t "~%")
  (format t "the most fit of the sample ...~%")
  (display i)
  nil)

;; Mutation

(defmethod mutate ((i individual))
  (let ((mutation (mutation (individual-rbg-string i))))
    (make-instance 'individual
       :number     (individual-number i)
       :rbg-string mutation
       :fitness    (funcall *fitness* mutation))))

;; percent mutation
(defconstant *pc-m* 50)

(defmethod maybe-mutate ((i individual))
  (if (< (random 100) *pc-m*)
    (mutate i)
    i))

;; copies
(setf *copy-demo* nil)
(defconstant *pc-c* 40)

(defmethod perform-copies ((current-pop population)
                           (new-pop population))
  (dotimes (_ (nr-copies))
    (perform-one-copy current-pop new-pop)))

(defmethod nr-copies ()
  (* (/ *pc-c* 100)
     *population-size*))

(defmethod perform-one-copy ((current-pop population)
                             (new-pop population))
  (let ((m (select-individual current-pop)))
    (when *copy-demo*
      (format t "Selected individual =~%")
      (display m))
    (let ((mm (maybe-mutate m)))
      (when *copy-demo*
        (format t "Possibly mutated individual =~%")
        (display mm))
      (setf (individual-number mm)
            (1+ (size new-pop)))
      (when *copy-demo*
        (format t "Renumbered individual =~%")
        (display mm))
      (let ((new-i (new-individual (1+ (size new-pop))
                                   (individual-rbg-string mm))))
        (setf (population-individuals new-pop)
              (append (population-individuals new-pop) (list new-i))))))
  nil)

(defmethod empty-population ((current-pop population))
  (make-instance 'population
     :generation  (1+ (population-generation current-pop))
     :individuals ()))

(setf *crossover-demo* nil)
(defconstant *pc-x* 60)

(defmethod perform-crossovers ((current-pop population)
                               (new-pop population))
  (dotimes (_ (nr-crossovers))
    (perform-one-crossover current-pop
                           new-pop)))

(defmethod nr-crossovers ()
  (* (/ *pc-x* 100)
     *population-size*))

(defmethod perform-one-crossover ((current-pop population)
                                  (new-pop population))
  ;; select parents
  (let ((mother (select-individual current-pop))
        (father (select-individual current-pop)))
    ;; verbose output
    (when *crossover-demo*
      (format t "Selected mother =~%")
      (display mother)
      (format t "Selected father =~%")
      (display father))
    ;; make crossover
    (let ((m (crossover mother father)))
      ;; verbose output
      (when *crossover-demo*
        (format t "the crossover = ~%")
        (display m))
      ;; possibly mutate child
      (let ((mm (maybe-mutate m)))
        ;; verbose output
        (when *crossover-demo*
          (format t "the possibly mutated individual =~%")
          (display mm))
        ;; set child's number to a unique number
        (setf (individual-number mm)
              (1+ (size new-pop)))
        ;; verbose output
        (when *crossover-demo*
          (format t "the renumbered individual =~%")
          (display mm))
        ;; add new individual to new population
        (let ((new-i (new-individual (1+ (size new-pop))
                                     (individual-rbg-string mm))))
          (setf (population-individuals new-pop)
                (append (population-individuals new-pop)
                        (list new-i)))))))
  nil)

(defmethod crossover ((mother individual)
                      (father individual))
  (let* ((mi (individual-rbg-string mother))
         (fi (individual-rbg-string father))
         (x  (crossover mi fi))
         (i  (new-individual 0 x)))
    i))

;; next generation

(defconstant *nr-generations* 25)

(defmethod next-generation ((current-pop population))
  (let ((new-pop (empty-population current-pop)))
    (perform-copies     current-pop new-pop)
    (perform-crossovers current-pop new-pop)

    new-pop))

;; progress methods

(defmethod summarize ((p population))
  (display p)
  (check-average p)
  (format t "~%")
  nil)

(defmethod check-average ((p population))
  (format t "average fitness of population ~A = ~A~%"
            (population-generation p)
            (average p))
  nil)
