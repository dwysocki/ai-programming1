(defun square (x) (expt x 2))
(defun euclidean-distance (x y)
  (sqrt (apply '+
               (mapcar 'square
                       (mapcar '- x y)))))
(defun perimeter-of-triangle (x)
  (let ((x1 (first x))
        (x2 (second x))
        (x3 (third x)))
    (apply '+ (mapcar 'abs (list (euclidean-distance x1 x2)
				 (euclidean-distance x2 x3)
				 (euclidean-distance x3 x1))))))
