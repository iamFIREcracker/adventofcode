(defpackage :aoc/2019/01 #.cl-user::*aoc-use*)
(in-package :aoc/2019/01)


(defun fuel-req (mass)
  (->< mass
    (floor >< 3)
    (- >< 2)))

(defun fuel-req-recursive (mass &aux (fuel (fuel-req mass)))
  (if (plusp fuel)
    (+ fuel (fuel-req-recursive fuel))
    0))

(define-problem (2019 1) (data parse-integers)
  (values
    (summation data :key #'fuel-req)
    (summation data :key #'fuel-req-recursive)))

(1am:test test-2019/01
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 3273715 part1))
    (1am:is (= 4907702 part2))))
