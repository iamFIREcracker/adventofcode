(defpackage :aoc/2015/08 #.cl-user::*aoc-use*)
(in-package :aoc/2015/08)

(defun length-unescaped (string &aux (index 0))
  (- (loop while (< index (length string)) for ch = (char string index)
           sum 1 do
           (when (and (char= ch #\\) (char= (char string (incf index)) #\x))
             (incf index 2))
           (incf index))
     2))

(defun part1 (strings)
  (- (reduce #'+ strings :key #'length)
     (reduce #'+ strings :key #'length-unescaped)))

(defun length-escaped (string &aux (index 0))
  (+ (loop while (< index (length string)) for ch = (char string index)
           sum (case ch ((#\" #\\) 2) (t 1))
           do (incf index))
     2))

(defun part2 (strings)
  (- (reduce #'+ strings :key #'length-escaped)
     (reduce #'+ strings :key #'length)))

(define-solution (2015 8) (strings)
  (values (part1 strings) (part2 strings)))

(define-test (2015 8) (1342 2074))
