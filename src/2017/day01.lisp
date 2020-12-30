(defpackage :aoc/2017/01 #.cl-user::*aoc-use*)
(in-package :aoc/2017/01)

(defun read-digits (data)
  (reverse (digits (parse-integer (first data)))))

(define-solution (2017 1) (digits read-digits)
  (let ((size (length digits))
        (digits (ncycle digits)))
    (values
      (loop repeat size
            for a in digits and b in (nthcdr 1 digits)
            sum (if (= a b) a 0))
      (loop repeat size
            for a in digits and b in (nthcdr (/ size 2) digits)
            sum (if (= a b) a 0)))))

(define-test (2017 1) (1341 1348))
