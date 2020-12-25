(defpackage :aoc/2020/25 #.cl-user::*aoc-use*)
(in-package :aoc/2020/25)

(defparameter *magic* 20201227)

(defun transform (subject loop-size &aux (value 1))
  (dotimes (n loop-size value)
    (setf value (rem (* value subject) *magic*))))

(defun find-loop-size (target &aux (value 1))
  (loop for loop-size from 0
        when (= value target) return loop-size
        do (setf value (rem (* value 7) *magic*))))

(define-solution (2020 25) (integers parse-integers)
  (transform (first integers) (find-loop-size (second integers))))

(define-test (2020 25) (10548634))
