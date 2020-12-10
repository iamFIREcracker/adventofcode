(defpackage :aoc/2020/10 #.cl-user::*aoc-use*)
(in-package :aoc/2020/10)

(defun part1 (adapters)
  (loop for p = 0 then c for c in adapters for delta = (- c p)
        count (= delta 1) into ones
        count (= delta 3) into threes
        finally (return (* ones (1+ threes)))))

(defun part2 (adapters)
  (let ((memo (make-hash-table :test 'equal)))
    (labels ((recur (p rest &aux (n (first rest)) (key (list p n)))
               (cond ((or (null rest) (> (- n p) 3)) 0) ; no luck
                     ((null (rest rest)) 1) ; found it
                     ((gethash key memo) (gethash key memo))
                     (t (setf (gethash key memo)
                              (+ (recur p (rest rest))
                                 (recur n (rest rest))))))))
      (recur 0 adapters))))

(define-solution (2020 10) (numbers parse-integers)
  (let* ((sorted (sort numbers #'<)))
    (values (part1 sorted) (part2 sorted))))

(define-test (2020 10) (1998 347250213298688))
