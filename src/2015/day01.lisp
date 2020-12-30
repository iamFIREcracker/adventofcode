(defpackage :aoc/2015/01 #.cl-user::*aoc-use*)
(in-package :aoc/2015/01)

(defun read-input (data) (first data))

(defun part1 (instructions)
  (loop for ch across instructions
        sum (if (char= ch #\() 1 -1)))

(defun part2 (instructions)
  (loop for ch across instructions and pos from 1
        sum (if (char= ch #\() 1 -1) into floor
        thereis (and (= floor -1) pos)))

(define-solution (2015 1) (instructions read-input)
  (values (part1 instructions) (part2 instructions)))

(define-test (2015 1) (138 1771))
