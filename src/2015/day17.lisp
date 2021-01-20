(defpackage :aoc/2015/17 #.cl-user::*aoc-use*)
(in-package :aoc/2015/17)

(defun solve (containers &aux solutions)
  (labels ((recur (containers target solution)
             (cond ((zerop target) (push solution solutions))
                   ((< target 0) nil)
                   ((null containers) nil)
                   (t (recur (rest containers) target solution)
                      (recur (rest containers)
                             (- target (first containers))
                             (cons (first containers) solution))))))
    (recur containers 150 nil)
    solutions))

(defun part1 (containers) (length (solve containers)))

(defun part2 (containers &aux (solutions (solve containers)))
  (let ((min (reduce #'min solutions :key #'length)))
    (count min solutions :key #'length)))

(define-solution (2015 17) (containers parse-integers)
  (values (part1 containers) (part2 containers)))

(define-test (2015 17) (1304 18))
