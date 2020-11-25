(defpackage :aoc/2017/06 #.cl-user::*aoc-use*)
(in-package :aoc/2017/06)

(defun parse-single-line-of-integers (x)
  (let* ((splits (split-sequence:split-sequence #\Tab (first x)))
         (numbers (mapcar #'parse-integer splits)))
   (make-array (length numbers) :initial-contents numbers)))

(defun reallocate-blocks (banks &aux (ret (copy-seq banks)))
  (let* ((most-blocks (maximization banks))
         (bank (position most-blocks banks)))
    (setf (aref ret bank) 0)
    (loop
      :with i = bank
      :repeat most-blocks
      :do (setf i (mod (1+ i) (length ret)))
      :do (incf (aref ret i)))
    ret))

(define-problem (2017 6) (data parse-single-line-of-integers)
  (loop
    :with cache = (make-hash-table :test 'equalp)
    :with banks = data
    :for cycles :from 1
    :do (setf banks (reallocate-blocks banks))
    :do (if (gethash banks cache)
          (return (values cycles (- cycles (gethash banks cache))))
          (hash-table-insert cache banks cycles))))

(1am:test test-2017/06
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 12841 part1))
    (1am:is (= 8038 part2))))
