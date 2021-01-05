(defpackage :aoc/2015/08 #.cl-user::*aoc-use*)
(in-package :aoc/2015/08)

(defun length-unescaped (string &aux (index 0))
  (- (loop while (< index (length string))
           for ch = (char string index)
           for ch1 = (and (< (1+ index) (length string))
                              (char string (1+ index)))
           sum 1
           if (and (char= ch #\\) (char= ch1 #\x)) do (incf index 4)
           else if (char= ch #\\) do (incf index 2)
           else do (incf index 1))
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
