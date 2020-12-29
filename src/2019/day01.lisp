(defpackage :aoc/2019/01 #.cl-user::*aoc-use*)
(in-package :aoc/2019/01)

(defun fuel-req (mass)
  (- (floor mass 3) 2))

(defun fuel-req-recursive (mass)
  (loop
    :for fuel = (fuel-req mass) :then (fuel-req fuel)
    :while (> fuel 0)
    :summing fuel))

(define-solution (2019 1) (data parse-integers)
  (values
    (reduce #'+ data :key #'fuel-req)
    (reduce #'+ data :key #'fuel-req-recursive)))

(define-test (2019 1) (3273715 4907702))
