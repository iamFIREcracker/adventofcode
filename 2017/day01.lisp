(defpackage :aoc/2017/01 #.cl-user::*aoc-use*)
(in-package :aoc/2017/01)

(defun char-to-int (char)
  (- (char-int char) (char-int #\0)))

(defun explode-digits (str)
  (map 'vector #'char-to-int str))

(define-problem (2017 1) (data first)
  (let* ((digits (explode-digits data))
         (size (length digits)))
    (values
      (loop
        :for i :below size
        :for j = (mod (+ i 1) size)
        :for curr = (aref digits i)
        :for next = (aref digits j)
        :when (eql curr next)
        :summing curr) 
      (loop
        :for i :below size
        :for j = (mod (+ i (/ size 2)) size)
        :for curr = (aref digits i)
        :for next2 = (aref digits j)
        :when (eql curr next2)
        :summing curr))))

(1am:test test-2017/01
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 1341 part1))
    (1am:is (= 1348 part2))))
