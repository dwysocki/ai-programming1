(defclass bank ()
  ((missionaries :accessor bank-missionaries     :initarg :missionaries)
   (cannibals    :accessor bank-cannibals        :initarg :cannibals)
   (boat         :accessor bank-boat             :initarg :boat)))

(defmethod display ((b bank))
  (format T "MISSIONARIES=~A CANNIBALS=~A BOAT=~A"
          (bank-missionaries b)
          (bank-cannibals    b)
          (bank-boat         b)))

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




(defclass node ()
  ((name         :accessor node-name             :initarg :name)
   (state        :accessor node-state            :initarg :state)
   (parent       :accessor node-parent           :initarg :parent)
   (operator     :accessor node-operator         :initarg :operator)))

(defmethod display ((n node))
  (princ (node-name n))
  (write-string " ")
  (when (not (rootp n))
    (princ (node-name (node-parent n)))
    (princ " ")
    (display (node-operator n)))
  (terpri)
  (display (node-state n))
  nil)




(defclass operator ()
  ((name         :accessor operator-name         :initarg :name)
   (precondition :accessor operator-precondition :initarg :precondition)
   (description  :accessor operator-description  :initarg :description)))





(defclass name-generator ()
  ((prefix       :accessor name-generator-prefix :initarg :prefix)
   (count        :accessor name-generator-count  :initform 0)))

(defmethod inc ((ng name-generator))
  (setf (name-generator-count)
        (1+ (name-generator-count))))

(defmethod gen-name ((ng name-generator))
  (let ((name (format nil "~A-~D" (name-generator-prefix ng)
                                  (name-generator-count  ng))))
    (inc ng)
    name))
