(defpackage :aoc/2017/06 #.cl-user::*aoc-use*)
(in-package :aoc/2017/06)

(defun parse-single-line-of-integers (x)
  (let* ((splits (split-sequence:split-sequence #\Tab (first x)))
         (numbers (mapcar #'parse-integer splits)))
   (make-array (length numbers) :initial-contents numbers)))

(defun reallocate-blocks (banks &aux (ret (copy-seq banks)))
  (let* ((most-blocks (reduce #'max banks))
         (bank (position most-blocks banks)))
    (setf (aref ret bank) 0)
    (loop
      :with i = bank
      :repeat most-blocks
      :do (setf i (mod (1+ i) (length ret)))
      :do (incf (aref ret i)))
    ret))

(define-solution (2017 6) (data parse-single-line-of-integers)
  (loop
    :with cache = (make-hash-table :test 'equalp)
    :with banks = data
    :for cycles :from 1
    :do (setf banks (reallocate-blocks banks))
    :do (if (gethash banks cache)
          (return (values cycles (- cycles (gethash banks cache))))
          (hash-table-insert cache banks cycles))))

(define-test (2017 6) (12841 8038))
