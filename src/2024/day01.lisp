(defpackage :aoc/2024/01 #.cl-user::*aoc-use*)
(in-package :aoc/2024/01)

(defun parse-location-lists (&optional (strings (uiop:read-file-lines #P"src/2024/day01.txt")))
  (let (x1 x2)
    (dolist (s strings)
      (destructuring-bind (n1 n2) (extract-integers s)
        (push n1 x1)
        (push n2 x2)))
    (list x1 x2)))


(defun part1 (&optional (loc-lists (parse-location-lists)))
  (destructuring-bind (x1 x2) loc-lists
    (setf x1 (sort (copy-seq x1) #'<)
          x2 (sort (copy-seq x2) #'<))
    (looping
      (doseqs ((n1 x1) (n2 x2))
        (sum! (abs (- n1 n2)))))))


(defun part2 (&optional (loc-lists (parse-location-lists)))
  (destructuring-bind (x1 x2) loc-lists
    (looping
      (dolist (n1 x1)
        (sum! (* (count n1 x2) n1))))))


(define-solution (2024 01) (loc-lists parse-location-lists)
  (values (part1 loc-lists)
          (part2 loc-lists)))

(define-test (2024 01) (2378066 18934359))
