(defpackage :aoc/2018/01 #.cl-user::*aoc-use*)
(in-package :aoc/2018/01)


(define-problem (2018 1) (data parse-integers)
  (values
    (summation data)
    (progn
      (setf (cdr (last data)) data) ; make data a circular list for easy looping
      (loop
        :with seen = (make-hash-table)
        :for number :in data
        :summing number :into frequency
        :do (if (gethash frequency seen)
              (return frequency)
              (setf (gethash frequency seen) T))))))

(1am:test test-2018/01
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 484 part1))
    (1am:is (= 367 part2))))
