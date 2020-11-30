(defpackage :aoc/2016/12 #.cl-user::*aoc-use*)
(in-package :aoc/2016/12)

(define-problem (2016 12) (program assembunnycode:parse-program)
  (values
    (assembunnycode:run program (list 0 0 0 0))
    (assembunnycode:run program (list 0 0 1 0))))

(1am:test test-2016/12
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 318083 part1))
    (1am:is (= 9227737 part2))))
