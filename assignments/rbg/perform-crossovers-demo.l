(load "rbg.l")

(defmethod perform-crossovers-demo ()
  (let* ((cp (initial-population))
         (np (empty-population cp)))
    (display np)
    (format t "~%~%")

    (setf *select-demo*    t)
    (setf *crossover-demo* t)

    (dotimes (_ 10)
      (perform-one-crossover cp np)
      (display np)
      (format t "~%~%"))

    (setf *select-demo*    nil)
    (setf *crossover-demo* nil))
  nil)
