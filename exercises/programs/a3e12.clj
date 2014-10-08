#!/usr/bin/env lein-exec

;; Clojure program which iteratively finds the first solution to the ;;
;; 8-Queens problem using the constraints of Chapter 3 Exercise 12.  ;;

(defn abs-difference [x y]
  "Finds the absolute value of the difference between two sequences."
  (map #(Math/abs (- %1 %2)) x y))

(defn diagonal-clash? [taken pair]
  "Returns true if the given pair clashes diagonally with any of the taken
  pairs by checking if the abs-difference between the pair and any taken pair
  is the same.

  Examples:
  [1 1] and [2 2] clash because |1-2| == |1-2|
  [0 1] and [1 2] clash because |0-1| == |1-2|"
  (some true?
        (for [taken-pair taken]
          (apply = (abs-difference taken-pair pair)))))

(defn find-solution-iter [rows cols taken]
  "Recursively finds the first found sets of solution pairs to the 8-queens
  problem."
  (some identity
        (for [r rows, c cols,
              :let [pair [r c]]]
          (when-not (diagonal-clash? taken pair)
            (let [rows  (disj rows  r)
                  cols  (disj cols  c)
                  taken (conj taken pair)]
              (if (and (seq rows)
                       (seq cols))
                (find-solution-iter rows cols taken)
                taken))))))

(defn find-solution []
  "Finds a set of solution pairs to the 8-queens problem by calling a
  recursive function which searches over rows 0-7 and cols 0-7."
  (let [rows      (set (range 8))
        cols      (set (range 8))
        solution (find-solution-iter rows cols #{})]
    solution))

(defn print-solution [solution]
  "Print the solution, with X's marking queens and -'s marking empty spaces."
  (when (seq solution)
    (doseq [r (range 8)]
      (doseq [c (range 8)]
        (if (solution [r c])
          (print "X ")
          (print "- ")))
      (print "\n"))))

;; Print the first solution found
(print-solution (find-solution))
;; Output
;
; X - - - - - - - 
; - - - - X - - - 
; - - - - - - - X 
; - - - - - X - - 
; - - X - - - - - 
; - - - - - - X - 
; - X - - - - - - 
; - - - X - - - -
;
