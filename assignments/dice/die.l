(defclass die ()
  ((faces :reader   die-faces
          :initarg  :faces
          :initform 6)
   (top-face :accessor die-top-face
             :initarg  :top-face
             :initform 1)
   (history :accessor die-history
            :initform ())))

(defmethod add-to-history ((d die) (top-face integer))
  (setf (die-history d)
        (append (die-history d) (list top-face)))
  nil)

(defmethod roll ((d die))
  (let* ((faces    (die-faces d))
         (new-face (1+ (random faces))))
    (add-to-history d new-face)
    (setf (die-top-face d) new-face)))

(defmethod roll-for-cond ((a die) (b die) (condition function))
  (let ((a-top-face (roll a))
        (b-top-face (roll b)))
    (if (funcall condition a-top-face b-top-face)
      nil
      (roll-for-cond a b condition))))

(defmethod roll-for-pair ((a die) (b die))
  (roll-for-cond a b #'=))

(defmethod roll-for-sum ((a die) (b die) (sum integer))
  (roll-for-cond a b (lambda (a b) (= sum (+ a b)))))

(defmethod roll-for-sums-or-pair ((a die) (b die) sums)
  (roll-for-cond a b (lambda (a b) (or (= a b)
                                       (member (+ a b) sums)))))

;; (defmethod roll-for-sums-or-pair ((a die) (b die) sums)
;;   (let ((a-top-face (roll a))
;;         (b-top-face (roll b)))
;;     (if (or (member (+ a-top-face b-top-face) sums)
;;             (= a-top-face b-top-face))
;;       nil
;;       (roll-for-sum a b sums))))
