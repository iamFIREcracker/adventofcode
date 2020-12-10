(defpackage :aoc/2020/10 #.cl-user::*aoc-use*)
(in-package :aoc/2020/10)

(defun part1 (adapters)
  (loop for p = 0 then c for c in adapters for delta = (- c p)
        count (= delta 1) into ones
        count (= delta 3) into threes
        finally (return (* ones (1+ threes)))))

(defun part2 (numbers &aux (dp (make-hash-table)))
  (loop initially (setf (gethash 0 dp) 1)
        for n in numbers do
        (setf (gethash n dp) (+ (gethash (- n 1) dp 0)
                                (gethash (- n 2) dp 0)
                                (gethash (- n 3) dp 0)))
        finally (return (gethash n dp))))

(define-solution (2020 10) (numbers parse-integers)
  (let* ((sorted (sort numbers #'<)))
    (values (part1 sorted) (part2 sorted))))

(define-test (2020 10) (1998 347250213298688))
