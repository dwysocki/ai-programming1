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

(defmethod roll-for-pair ((a die) (b die))
  (let ((a-top-face (roll a))
        (b-top-face (roll b)))
    (if (= a-top-face b-top-face)
      nil
      (roll-for-pair a b))))

(defmethod roll-for-sum ((a die) (b die) (sum integer))
  (let ((a-top-face (roll a))
        (b-top-face (roll b)))
    (if (= sum (+ a-top-face
                  b-top-face))
      nil
      (roll-for-sum a b sum))))
