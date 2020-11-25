(defpackage :aoc/2019/01 #.cl-user::*aoc-use*)
(in-package :aoc/2019/01)

(defun fuel-req (mass)
  (- (floor mass 3) 2))

(defun fuel-req-recursive (mass)
  (loop
    :for fuel = (fuel-req mass) :then (fuel-req fuel)
    :while (> fuel 0)
    :summing fuel))

(define-problem (2019 1) (data parse-integers)
  (values
    (summation data :key #'fuel-req)
    (summation data :key #'fuel-req-recursive)))

(1am:test test-2019/01
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 3273715 part1))
    (1am:is (= 4907702 part2))))
