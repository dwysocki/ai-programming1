(defmethod pick ((l list))
  (nth (random (length l))
       l))

(defmethod others ((l list) e)
  (remove-if (lambda (x) (eq e x))
             l))

(defmethod change ((l list) e (n integer))
  (if (zerop n)
    (cons e
          (cdr l))
    (cons (car l)
          (change (cdr l)
                  e
                  (1- n)))))

(defmethod take ((n integer) (l list))
  (when (> n 0)
    (cons (car l)
          (take (1- n)
                (cdr l)))))

(defmethod drop ((n integer) (l list))
  (if (zerop n)
    l
    (drop (1- n)
          (cdr l))))
