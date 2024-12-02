(defpackage :aoc/2024/02 #.cl-user::*aoc-use*)
(in-package :aoc/2024/02)

(defun parse-reports (&optional (strings (uiop:read-file-lines #P"src/2024/day02.txt")))
  (mapcar #'extract-integers strings))
#+#:excluded (parse-reports)

(defun all-increasing? (seq) (apply #'< seq))

(defun all-decreasing? (seq) (apply #'> seq))

(defun deltas-in-range? (seq)
  (looping
    (doseqs ((a seq)
             (b (rest seq)))
      (let1 delta (abs (- a b))
        (never! (or (< delta 1) (> delta 3)))))))


(defun safe? (seq)
  (and (or (all-increasing? seq)
           (all-decreasing? seq))
       (deltas-in-range? seq)))


(defun safe-with-one-level-tolerance? (seq)
  (looping
    (dotimes (i (length seq))
      (let1 seq1 (concatenate 'list (subseq seq 0 i) (subseq seq (1+ i)))
        (thereis! (safe? seq1))))))


(define-solution (2024 02) (reports parse-reports)
  (values (count-if #'safe? reports)
          (count-if #'safe-with-one-level-tolerance? reports)))

(define-test (2024 02) (236 308))
