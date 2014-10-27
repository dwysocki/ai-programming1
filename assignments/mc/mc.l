(load "classes.l")

(defun mc ()
  (establish-operators)
  (setup)
  (solve))

(defun setup ()
  (let* ((lb   (make-instance 'bank
                              :missionaries '(m m m)
                              :cannibals    '(c c c)
                              :boat         'b))
         (rb   (make-instance 'bank
                              :missionaries ()
                              :cannibals    ()
                              :boat         nil))
         (init (make-instance 'state
                              :left-bank lb
                              :right-bank rb))
         (root (make-instance 'node
                              :state init
                              :name  'root)))
    (setf *unexplored* (list root))
    (setf *explored*   ())
    (setf *ng*         (make-instance 'name-generator :prefix "N"))))

(defmethod solve ()
  (when *tracing-search*
    (terpri) (write-line ">>> Solve") (terpri)
    (display-explored-nodes)
    (display-unexplored-nodes))
  
  (if (null *unexplored*)
    (write-line "There is no solution.")
    (let ((e-node (pop *unexplored*)))
      (when *tracing-search*
        (display-e-node e-node))

      (cond
       ((goalp (node-state e-node))
        (display-solution e-node))

       ((exploredp e-node)
        (solve))

       (T
        (push e-node *explored*)
        (let (kids (children-of e-node))
          (setf *unexplored* (append *unexplored* kids))
          (solve))))))
  nil)

(defmethod children-of ((e-node node))
  ;...;
  )

(defmethod child-of ((n node) (o operator))
  (let ((new-node (make-instance 'node)))
    (setf (node-name new-node) (next *ng*))
    (setf (node-parent new-node) n)
    (setf (node-operator new-node) o)
    (let ((c (copy-state (node-state n))))
      (apply-operator o c)
      (setf (node-state new-node) c)
      new-node)))
