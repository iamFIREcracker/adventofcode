(defpackage :aoc/2020/01 #.cl-user::*aoc-use*)
(in-package :aoc/2020/01)

(defun find-pair-that-adds-up-to (target integers)
  (loop for (n . rest) on integers
        for m = (find target rest :key (partial-1 #'+ n))
        when m return (list n m)))

(define-problem (2020 1) (integers parse-integers)
  (values (reduce #'* (find-pair-that-adds-up-to 2020 integers))
          (loop for (n . rest) on integers
                for (m o) = (find-pair-that-adds-up-to (- 2020 n) rest)
                when m return (* n m o))))

(1am:test test-2020/01
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 542619 part1))
    (1am:is (= 32858450 part2))))
