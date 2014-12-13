(load "classes.l")
(load "util.l")

(defvar *tracing-search* nil)
(defvar *graphviz* nil)

(defun mc ()
  (establish-operators)
  (setup)

  (when *graphviz*
    (format t "digraph mc {~%")
    (format t "  size=\"10,10\"~%")
    (format t "  rankdir=TB;~%")
    (format t "  node [style=filled];~%~%"))
  (solve)

  (when *graphviz*
    (format t "}~%")))

(defun establish-operators ()
  (setf *m->lb*
    (make-instance 'operator
      :name 'm->lb
      :precondition "missionary & boat on right bank"
      :description "move missionary from right bank to left"))
  (setf *m->rb*
    (make-instance 'operator
      :name 'm->rb
      :precondition "missionary & boat on left bank"
      :description "move missionary from left bank to right"))
  (setf *c->lb*
    (make-instance 'operator
      :name 'c->lb
      :precondition "cannibal & boat on right bank"
      :description "move cannibal from right bank to left"))
  (setf *c->rb*
    (make-instance 'operator
      :name 'c->rb
      :precondition "cannibal & boat on left bank"
      :description "move cannibal from left bank to right"))
  (setf *mm->lb*
    (make-instance 'operator
      :name 'mm->lb
      :precondition "two missionaries & boat on right bank"
      :description "move two missionaries from right bank to left"))
  (setf *mm->rb*
    (make-instance 'operator
      :name 'mm->rb
      :precondition "two missionaries & boat on left bank"
      :description "move two missionaries from left bank to right"))
  (setf *cc->lb*
    (make-instance 'operator
      :name 'cc->lb
      :precondition "two cannibals & boat on right bank"
      :description "move two cannibals from right bank to left"))
  (setf *cc->rb*
    (make-instance 'operator
      :name 'cc->rb
      :precondition "two cannibals & boat on left bank"
      :description "move two cannibals from left bank to right"))
  (setf *mc->lb*
    (make-instance 'operator
      :name 'mc->lb
      :precondition "missionary, cannibal, & boat on right bank"
      :description "move missionary & cannibal from right bank to left"))
  (setf *mc->rb*
    (make-instance 'operator
      :name 'mc->rb
      :precondition "missionary, cannibal, & boat on left bank"
      :description "move missionary & cannibal from right bank to left"))
  (setf *operator-list*
        (list *m->lb*  *m->rb*  *c->lb*  *c->rb*
              *mm->lb* *mm->rb* *cc->lb* *cc->rb* *mc->lb* *mc->rb*))
  nil)

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
    (format t "~%>>> Solve~%~%")
    (display-explored-nodes)
    (display-unexplored-nodes))
  
  (if (null *unexplored*)
    (format t "There is no solution.~%")
    (let ((e-node (pop *unexplored*)))
      (when *tracing-search*
        (format t "CURRENT:~%")
        (display e-node)
        (format t "---------~%"))

      (cond
       ((goalp (node-state e-node))
        (if *graphviz*
          (display-graphviz e-node)
          (display-solution e-node)))

       ((exploredp e-node)
        (solve))

       ((feed-state-p e-node)
        (cond
          (*graphviz*       (display-graphviz e-node))
          (*tracing-search* (format t "DEATH~%")))
        (solve))

       (:else
        (push e-node *explored*)
        (when *graphviz*
          (display-graphviz e-node))
        (let ((kids (children-of e-node)))
          (setf *unexplored* (append *unexplored* kids))
          (solve))))))
  nil)

(defmethod display-solution ((n node))
  (format t "SOLUTION:~%")
  (display-parents n))

(defmethod display-explored-nodes ()
  (format t "EXPLORED:~%")
  (mapcar #'display *explored*)
  (format t "---------~%"))

(defmethod display-unexplored-nodes ()
  (format t "UNEXPLORED:~%")
  (mapcar #'display *unexplored*)
  (format t "---------~%"))

(defmethod apply-operator ((o operator) (s state))
  (case (operator-name o)
    ('m->lb  (move-left  s :M 1))

    ('m->rb  (move-right s :M 1))

    ('c->lb  (move-left  s      :C 1))

    ('c->rb  (move-right s      :C 1))

    ('mm->lb (move-left  s :M 2))

    ('mm->rb (move-right s :M 2))

    ('cc->lb (move-left  s      :C 2))

    ('cc->rb (move-right s      :C 2))

    ('mc->lb (move-left  s :M 1 :C 1))

    ('mc->rb (move-right s :M 1 :C 1))))

(defmethod children-of ((e-node node) &aux kids)
  (when (applicablep *m->lb* e-node)
    (push (child-of e-node *m->lb*) kids))
  (when (applicablep *m->rb* e-node)
    (push (child-of e-node *m->rb*) kids))
  (when (applicablep *c->lb* e-node)
    (push (child-of e-node *c->lb*) kids))
  (when (applicablep *c->rb* e-node)
    (push (child-of e-node *c->rb*) kids))
  (when (applicablep *mm->lb* e-node)
    (push (child-of e-node *mm->lb*) kids))
  (when (applicablep *mm->rb* e-node)
    (push (child-of e-node *mm->rb*) kids))
  (when (applicablep *cc->lb* e-node)
    (push (child-of e-node *cc->lb*) kids))
  (when (applicablep *cc->rb* e-node)
    (push (child-of e-node *cc->rb*) kids))
  (when (applicablep *mc->lb* e-node)
    (push (child-of e-node *mc->lb*) kids))
  (when (applicablep *mc->rb* e-node)
    (push (child-of e-node *mc->rb*) kids))
  kids)

(defmethod child-of ((n node) (o operator))
  (let* ((new-node-name  (gen-name *ng*))
         (new-node-state (apply-operator o (node-state n)))
         (new-node       (make-instance 'node
                             :name     new-node-name
                             :state    new-node-state
                             :parent   n
                             :operator o)))
    new-node))

;;;; MOVES

(defmethod move-left ((s state) &key (M 0) (C 0))
  (let* ((new-state  (copy s))
         (left-bank  (state-left-bank new-state))
         (right-bank (state-right-bank new-state)))
    ;; remove from right bank
    (setf (bank-boat right-bank)
          NIL)
    (setf (bank-missionaries right-bank)
          (remove-n (bank-missionaries right-bank) M))
    (setf (bank-cannibals right-bank)
          (remove-n (bank-cannibals right-bank) C))
    ;; add to left bank
    (setf (bank-boat left-bank)
          'B)
    (setf (bank-missionaries left-bank)
          (add-n (bank-missionaries left-bank) M 'M))
    (setf (bank-cannibals left-bank)
          (add-n (bank-cannibals left-bank) C 'C))
    ;; return new state
    new-state))

(defmethod move-right ((s state) &key (M 0) (C 0))
  (let* ((new-state  (copy s))
         (left-bank  (state-left-bank new-state))
         (right-bank (state-right-bank new-state)))
    ;; remove from left bank
    (setf (bank-boat left-bank)
          NIL)
    (setf (bank-missionaries left-bank)
          (remove-n (bank-missionaries left-bank) M))
    (setf (bank-cannibals left-bank)
          (remove-n (bank-cannibals left-bank) C))
    ;; add to right bank
    (setf (bank-boat right-bank)
          'B)
    (setf (bank-missionaries right-bank)
          (add-n (bank-missionaries right-bank) M 'M))
    (setf (bank-cannibals right-bank)
          (add-n (bank-cannibals right-bank) C 'C))
    ;; return new state
    new-state))



;;;; PREDICATES

(defmethod rootp ((n node))
  (eq (node-name n) 'root))

(defmethod goalp ((n node))
  (goalp (node-state n)))

(defmethod goalp ((s state))
  (and (left-bank-ok-p  (state-left-bank  s))
       (right-bank-ok-p (state-right-bank s))))

(defmethod left-bank-ok-p ((b bank))
  (and (= 0 (missionary-count b)
            (cannibal-count b))
       (not (bank-boat b))))

(defmethod right-bank-ok-p ((b bank))
  (and (= 3 (missionary-count b)
            (cannibal-count b))
       (bank-boat b)))

(defmethod exploredp ((n node))
  (member-node-p n *explored*))

(defmethod member-node-p ((n node) (l list))
  (cond
   ((null l)
    nil)

   ((equal-node-p n (first l))
    t)

   (T
    (member-node-p n (rest l)))))

(defmethod equal-node-p ((n1 node) (n2 node))
  (let ((s1 (node-state n1))
        (s2 (node-state n2)))
    (and (equal-bank-p (state-left-bank  s1) (state-left-bank  s2))
         (equal-bank-p (state-right-bank s1) (state-right-bank s2)))))

(defmethod equal-bank-p ((b1 bank) (b2 bank))
  (let ((banks (list b1 b2)))
    (and (apply #'equal
                (mapcar #'bank-missionaries banks))
         (apply #'equal
                (mapcar #'bank-cannibals    banks))
         (apply #'equal
                (mapcar #'bank-boat         banks)))))

(defmethod applicablep ((op operator) (n node))
  (let ((s (node-state n)))
   (case (operator-name op)
     ('m->lb  (applicable-m->lb-p  s))
     ('m->rb  (applicable-m->rb-p  s))
     ('c->lb  (applicable-c->lb-p  s))
     ('c->rb  (applicable-c->rb-p  s))
     ('mm->lb (applicable-mm->lb-p s))
     ('mm->rb (applicable-mm->rb-p s))
     ('cc->lb (applicable-cc->lb-p s))
     ('cc->rb (applicable-cc->rb-p s))
     ('mc->lb (applicable-mc->lb-p s))
     ('mc->rb (applicable-mc->rb-p s)))))

(defmethod applicable-m->lb-p ((s state))
  (and (bank-boat (state-right-bank s))
       (<= 1 (missionary-count (state-right-bank s)))))

(defmethod applicable-m->rb-p ((s state))
  (and (bank-boat (state-left-bank s))
       (<= 1 (missionary-count (state-left-bank s)))))

(defmethod applicable-c->lb-p ((s state))
  (and (bank-boat (state-right-bank s))
       (<= 1 (cannibal-count (state-right-bank s)))))

(defmethod applicable-c->rb-p ((s state))
  (and (bank-boat (state-left-bank s))
       (<= 1 (cannibal-count (state-left-bank s)))))

(defmethod applicable-mm->lb-p ((s state))
  (and (bank-boat (state-right-bank s))
       (<= 2 (missionary-count (state-right-bank s)))))

(defmethod applicable-mm->rb-p ((s state))
  (and (bank-boat (state-left-bank s))
       (<= 2 (missionary-count (state-left-bank s)))))

(defmethod applicable-cc->lb-p ((s state))
  (and (bank-boat (state-right-bank s))
       (<= 2 (cannibal-count (state-right-bank s)))))

(defmethod applicable-cc->rb-p ((s state))
  (and (bank-boat (state-left-bank s))
       (<= 2 (cannibal-count (state-left-bank s)))))

(defmethod applicable-mc->lb-p ((s state))
  (and (applicable-m->lb-p s)
       (applicable-c->lb-p s)))

(defmethod applicable-mc->rb-p ((s state))
  (and (applicable-m->rb-p s)
       (applicable-c->rb-p s)))

(defmethod feed-state-p ((n node))
  (feed-state-p (node-state n)))

(defmethod feed-state-p ((s state))
  (or (feed-state-p (state-left-bank  s))
      (feed-state-p (state-right-bank s))))

(defmethod feed-state-p ((b bank))
  (< 0 (missionary-count b) (cannibal-count b)))
