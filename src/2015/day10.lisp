(defpackage :aoc/2015/10 #.cl-user::*aoc-use*)
(in-package :aoc/2015/10)

(defun parse-digits (lines)
  (map 'list #'parse-integer (cl-ppcre:split "" (first lines))))

(defun look-and-say (digits)
  (loop with n = 1
        for (curr . rest) on digits for next = (car rest)
        if (eql curr next) do (incf n)
        else append (list n curr) and do (setf n 1)))

(defun play (digits times)
  (dotimes (n times digits)
    (setf digits (look-and-say digits))))

(defun part1 (digits) (length (play digits 40)))

(defun part2 (digits) (length (play digits 50)))

(define-solution (2015 10) (digits parse-digits)
  (values (part1 digits) (part2 digits)))

(define-test (2015 10) (492982 6989950))
