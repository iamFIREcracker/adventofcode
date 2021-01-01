(defpackage :aoc/2015/01 #.cl-user::*aoc-use*)
(in-package :aoc/2015/01)

(defun parse-instruction (ch)
  (ecase ch (#\( 1) (#\) -1)))

(defun parse-input (lines)
  (map 'list #'parse-instruction (first lines)))

(defun part1 (instructions)
  (reduce #'+ instructions))

(defun part2 (instructions &aux (floor 0))
  (loop for d in instructions and pos from 1
        do (incf floor d)
        when (= floor -1) return pos))

(define-solution (2015 1) (instructions parse-input)
  (values (part1 instructions) (part2 instructions)))

(define-test (2015 1) (138 1771))
