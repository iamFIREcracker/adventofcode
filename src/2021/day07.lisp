(defpackage :aoc/2021/07 #.cl-user::*aoc-use*)
(in-package :aoc/2021/07)


(defun parse-crabs (data) (extract-positive-integers (first data)))


(defun minimize-fuel (crabs distance-fun)
  (loop for p from (find-min crabs) to (find-max crabs) minimize
        (loop for c in crabs sum (funcall distance-fun c p))))


(define-solution (2021 07) (crabs parse-crabs)
  (values (minimize-fuel crabs (lambda (p1 p2) (abs (- p1 p2))))
          (minimize-fuel crabs (lambda (p1 p2 &aux (n (abs (- p1 p2))))
                                 (floor (* n (1+ n)) 2)))))

(define-test (2021 07) (359648 100727924))
