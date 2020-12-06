(defpackage :aoc/2020/06 #.cl-user::*aoc-use*)
(in-package :aoc/2020/06)

(defun parse-groups (data)
  (let (groups current)
    (dolist (string (append data '("")) groups)
      (if (string= string "")
        (setf groups (cons current groups) current nil)
        (setf current (cons (coerce string 'list) current))))))

(define-solution (2020 6) (groups parse-groups)
  (loop for answers in groups
        sum (length (reduce #'union answers)) into part1
        sum (length (reduce #'intersection answers)) into part2
        finally (return (values part1 part2))))

(define-test (2020 6) (6809 3394))
