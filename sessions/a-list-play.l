;; an association list for pitch names and MIDI numbers ;;

(defconstant pitch-names
  '(c4 d4 e4 f4 g4 a4 b4))
(defconstant midi-names
  '(60 62 64 65 67 69 71 72))

(defconstant name-midi-pairs
  (mapcar #'cons pitch-names
                 midi-names))
