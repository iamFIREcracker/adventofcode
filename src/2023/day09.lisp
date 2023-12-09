(defpackage :aoc/2023/09 #.cl-user::*aoc-use*)
(in-package :aoc/2023/09)


(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day09.txt")))
  (mapcar #'extract-integers strings))


(defun deltas (nums)
  (loop for n in nums by #'cdr for m in (cdr nums) by #'cdr
        collect (- m n)))

(defun prediction-sequences (history)
  (looping
    (while (some (complement #'zerop) history)
      (collect! history)
      (setf history (deltas history)))))


(defun extrapolate (history)
  (looping
    (dolist (nums (prediction-sequences history))
      (sum! (car (last nums))))))


(defun extrapolate-backward (history)
  (bnd1 (res 0)
    (dolist (nums (reverse (prediction-sequences history)) res)
      (setf res (- (first nums) res)))))


(define-solution (2023 09) (input parse-input)
  (values (reduce #'+ input :key #'extrapolate)
          (reduce #'+ input :key #'extrapolate-backward)))

(define-test (2023 09) (1930746032 1154))
