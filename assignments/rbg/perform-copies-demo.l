(load "rbg.l")

(defmethod perform-copies-demo ()
  (let* ((current-pop (initial-population))
         (new-pop     (empty-population current-pop)))
    (display new-pop)
    (format t "~%~%")

    (setf *select-demo* t)
    (setf *copy-demo*   t)

    (dotimes (_ 10)
      (perform-one-copy current-pop new-pop)
      (display new-pop)
      (format t "~%~%"))

    (setf *select-demo* nil)
    (setf *copy-demo*   nil)))
