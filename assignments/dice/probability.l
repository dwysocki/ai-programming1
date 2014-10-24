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

(defun sdm (list)
  (/ (stdev list)
     (sqrt (length list))))

(defun trials (fn iterations a-faces b-faces)
  (when (> iterations 0)
    (let* ((a (make-instance 'die :faces a-faces))
           (b (make-instance 'die :faces b-faces)))
      (funcall fn a b)
      (cons (length (die-history a))
            (trials fn (1- iterations) a-faces b-faces)))))

(defun run-trials (fn iterations a-faces b-faces)
  (let* ((trials (trials fn iterations a-faces b-faces))
         (mean   (mean trials))
         (sdm    (sdm trials)))
    (format nil "mean=~F, sdm=~F" mean sdm)))

(defun pair-times (iterations a-faces b-faces)
  (run-trials #'roll-for-pair iterations a-faces b-faces))

(defun sum-times (iterations a-faces b-faces sum)
  (run-trials (lambda (a b) (roll-for-sum a b sum))
              iterations a-faces b-faces))

(defun sums-pair-times (iterations a-faces b-faces sums)
  (run-trials (lambda (a b) (roll-for-sums-or-pair a b sums))
              iterations a-faces b-faces))
