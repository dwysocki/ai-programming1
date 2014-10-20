(load "die.l")

(defun square (x)
  (expt x 2))

(defun mean (list)
  (/ (apply #'+ list)
     (length list)))

(defun stdev (list)
  (let ((mean (mean list))
        (N    (length list)))
    (sqrt (* (/ N)
             (apply #'+
                    (mapcar #'square
                            (mapcar (lambda (x) (- x mean))
                                    list)))))))

(defun pair-times (iterations a-faces b-faces)
  (let* ((trials (pair-times-trials iterations a-faces b-faces))
         (mean   (mean trials))
         (stdev  (stdev trials)))
    (format nil "~F +/- ~F times" mean stdev)))

(defun pair-times-trials (iterations a-faces b-faces)
  (when (> iterations 0)
    (let* ((a (make-instance 'die :faces a-faces))
           (b (make-instance 'die :faces b-faces)))
      (roll-for-pair a b)
      (cons (length (die-history a))
            (pair-times-trials (1- iterations) a-faces b-faces)))))

(defun sum-times (iterations a-faces b-faces sum)
  (let* ((trials (sum-times-trials iterations a-faces b-faces sum))
         (mean   (mean trials))
         (stdev  (stdev trials)))
    (format nil "~F +/- ~F times" mean stdev)))

(defun sum-times-trials (iterations a-faces b-faces sum)
  (when (> iterations 0)
    (let* ((a (make-instance 'die :faces a-faces))
           (b (make-instance 'die :faces b-faces)))
      (roll-for-sum a b sum)
      (cons (length (die-history a))
            (sum-times-trials (1- iterations) a-faces b-faces sum)))))
