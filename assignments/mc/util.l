(defun add-n (list n elem)
  (if (<= n 0)
    list
    (cons elem
          (add-n list (1- n) elem))))

(defun remove-n (list n)
  (subseq list n))
