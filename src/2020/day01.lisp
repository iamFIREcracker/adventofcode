(defpackage :aoc/2020/01 #.cl-user::*aoc-use*)
(in-package :aoc/2020/01)

(defun find-pair-that-adds-up-to (target integers)
  (loop for (n . rest) on integers
        for m = (find target rest :key (partial-1 #'+ n))
        when m return (list n m)))

(define-solution (2020 1) (integers parse-integers)
  (values (reduce #'* (find-pair-that-adds-up-to 2020 integers))
          (loop for (n . rest) on integers
                for (m o) = (find-pair-that-adds-up-to (- 2020 n) rest)
                when m return (* n m o))))

(define-test (2020 1) (542619 32858450))
