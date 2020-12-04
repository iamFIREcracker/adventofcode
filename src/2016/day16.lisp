(defpackage :aoc/2016/16 #.cl-user::*aoc-use*)
(in-package :aoc/2016/16)

(defun generate-random-data (seed size)
  (let ((array (make-array size
                           :element-type 'base-char
                           :adjustable t
                           :fill-pointer 0)))
    (loop for c across seed do (vector-push c array))
    (loop while (< (length array) size)
          for last = (1- (fill-pointer array)) do
          (vector-push #\0 array)
          (loop for i from last downto 0
                for c = (aref array i) do
                (vector-push (if (char= #\0 c) #\1 #\0) array)))
    array))

(defun checksum-reduction (array)
  (loop for i below (length array) by 2
        for j from 0
        for c1 = (aref array i)
        for c2 = (aref array (1+ i)) do
        (setf (aref array j) (if (char= c1 c2) #\1 #\0))
        finally (setf (fill-pointer array) (1+ j)))
  array)

(defun checksum (array)
  (prog1 array
    (loop do (checksum-reduction array)
          while (evenp (length array)))))

(define-solution (2016 16) (seed first)
  (values
    (checksum (generate-random-data seed 272))
    (checksum (generate-random-data seed 35651584))))

(define-test (2016 16) ("01110011101111011" "11001111011000111"))
