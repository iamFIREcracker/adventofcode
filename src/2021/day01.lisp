(defpackage :aoc/2021/01 #.cl-user::*aoc-use*)
(in-package :aoc/2021/01)


(defun part1 (numbers)
  (loop for (a b) on numbers
        when b count (< a b)))


(defun part2 (numbers)
  (part1
    (loop for (a b c) on numbers
          when c collect (+ a b c))))


(define-solution (2021 01) (numbers parse-integers)
  (values (part1 numbers) (part2 numbers)))

(define-test (2021 01) (1557 1608))
