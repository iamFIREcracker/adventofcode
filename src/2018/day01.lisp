(defpackage :aoc/2018/01 #.cl-user::*aoc-use*)
(in-package :aoc/2018/01)


(define-solution (2018 1) (data parse-integers)
  (values
    (summation data)
    (loop
      :initially (ncycle data) ; make data a circular list for easy looping
      :with seen = (make-hash-table)
      :for number :in data
      :summing number :into frequency
      :do (if (gethash frequency seen)
            (return frequency)
            (hash-table-insert seen frequency T))))) ; XXX hash-set

(define-test (2018 1) (484 367))
