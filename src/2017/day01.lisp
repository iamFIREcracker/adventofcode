(defpackage :aoc/2017/01 #.cl-user::*aoc-use*)
(in-package :aoc/2017/01)

(define-solution (2017 1) (data first)
  (let* ((digits (str-digits data 'vector))
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

(define-test (2017 1) (1341 1348))
