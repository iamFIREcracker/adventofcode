(defpackage :aoc/2023/04 #.cl-user::*aoc-use*)
(in-package :aoc/2023/04)


(defun winning-numbers (s)
  (bnd1 (numbers (second (split-sequence:split-sequence #\: s)))
    (destructuring-bind (winning yours) (split-sequence:split-sequence #\| numbers)
      (intersection (extract-positive-integers winning)
                    (extract-positive-integers yours)))))

(defun card-points (s) (ash 1 (1- (length (winning-numbers s)))))


(defun part2 (&optional (strings (uiop:read-file-lines #P"src/2023/day04.txt")))
  (bnd1 (cards-count (make-array (length strings) :initial-element 1))
    (looping
      (doseq ((curr s) (enumerate strings))
        (bnd* ((current-card-count (aref cards-count curr)))
          (sum! current-card-count)
          (dorangei (i 1 (length (winning-numbers s)))
            (incf (aref cards-count (+ curr i)) current-card-count)))))))


(define-solution (2023 04) (strings)
  (values (reduce #'+ strings :key #'card-points)
          (part2)))

(define-test (2023 04) (20667 5833065))
