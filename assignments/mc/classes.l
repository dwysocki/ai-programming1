(defclass bank ()
  ((missionaries :accessor bank-missionaries     :initarg :missionaries)
   (cannibals    :accessor bank-cannibals        :initarg :cannibals)
   (boat         :accessor bank-boat             :initarg :boat)))

(defmethod display ((b bank))
  (format t "MISSIONARIES=~A CANNIBALS=~A BOAT=~A~%"
          (bank-missionaries b)
          (bank-cannibals    b)
          (bank-boat         b)))

(defmethod display-graphviz ((b bank))
  (format t "M:~D, C:~D, B:~D"
          (length (bank-missionaries b))
          (length (bank-cannibals    b))
          (if (bank-boat b)
            1
            0)))

(defmethod copy ((b bank))
  (make-instance 'bank
      :missionaries (copy-list (bank-missionaries b))
      :cannibals    (copy-list (bank-cannibals    b))
      :boat                    (bank-boat         b)))

(defmethod missionary-count ((b bank))
  (length (bank-missionaries b)))

(defmethod cannibal-count ((b bank))
  (length (bank-cannibals b)))



(defclass state ()
  ((left-bank    :accessor state-left-bank       :initarg :left-bank)
   (right-bank   :accessor state-right-bank      :initarg :right-bank)))

(defmethod display ((s state))
  (display (state-left-bank s))
  (display (state-right-bank s)))

(defmethod display-graphviz ((s state))
  (format t "L = ")
  (display-graphviz (state-left-bank  s))
  (format t "<BR/>")
  (format t "R = ")
  (display-graphviz (state-right-bank s)))

(defmethod copy ((s state))
  (make-instance 'state
      :left-bank  (copy (state-left-bank  s))
      :right-bank (copy (state-right-bank s))))



(defclass node ()
  ((name         :accessor node-name             :initarg :name)
   (state        :accessor node-state            :initarg :state)
   (parent       :accessor node-parent           :initarg :parent)
   (operator     :accessor node-operator         :initarg :operator)))

(defmethod display ((n node))
  (format t "NAME: ~A~%" (node-name n))
  (when (not (rootp n))
    (display (node-operator n)))
  (display (node-state n))
  (format t "-----~%")
  nil)

(defmethod display-parents ((n node))
  (when (not (rootp n))
    (display-parents (node-parent n)))
  (display n))

(defmethod display-graphviz ((n node))
  (format t "  \"~A\" [ label=<"
          (node-name n))
  (display-graphviz (node-state n))
  (format t ">, color=\"~A\" ];~%"
          (node-color n))
  (display-graphviz-edge n))

(defmethod display-graphviz-edge ((n node))
  (when (not (rootp n))
    (format t "  \"~A\" -> \"~A\" [ label=\"~A\" ];~%"
            (node-name (node-parent n))
            (node-name n)
            (operator-name (node-operator n))))
  (format t "~%"))

(defmethod node-color ((n node))
  (cond
    ((goalp (node-state n))
     "royalblue")

    ((feed-state-p (node-state n))
     "red3")

    (:else
     "rosybrown")))


(defclass operator ()
  ((name         :accessor operator-name         :initarg :name)
   (precondition :accessor operator-precondition :initarg :precondition)
   (description  :accessor operator-description  :initarg :description)))

(defmethod display ((o operator))
  (format t "~A~%" (operator-description o)))




(defclass name-generator ()
  ((prefix       :accessor name-generator-prefix :initarg :prefix)
   (count        :accessor name-generator-count  :initform 0)))

(defmethod inc ((ng name-generator))
  (setf (name-generator-count ng)
        (1+ (name-generator-count ng))))

(defmethod gen-name ((ng name-generator))
  (let ((name (format nil "~A-~D" (name-generator-prefix ng)
                                  (name-generator-count  ng))))
    (inc ng)
    name))
