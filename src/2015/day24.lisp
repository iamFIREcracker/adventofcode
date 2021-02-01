(defpackage :aoc/2015/24 #.cl-user::*aoc-use*)
(in-package :aoc/2015/24)

(defun target-weight (weights groups) (/ (reduce #'+ weights) groups))
(defun quantum-entanglement (group) (reduce #'* group))

(defun find-perfect-balance (weights groups
                                     &aux (target (target-weight weights groups)))
  (labels ((recur (n target remaining)
             (cond ((= n 1) (when-let ((found (find target remaining)))
                                  (list (list found))))
                   ((null remaining) '())
                   (t (append
                        (recur n target (rest remaining))
                        (loop for rest in (recur (1- n)
                                                 (- target (first remaining))
                                                 (rest remaining))
                              collect (cons (first remaining) rest)))))))
    (loop for n from 1 for solutions = (recur n target weights)
          when solutions return (reduce #'min solutions
                                        :key #'quantum-entanglement))))

(defun part1 (weights) (find-perfect-balance weights 3))
(defun part2 (weights) (find-perfect-balance weights 4))

(define-solution (2015 24) (weights parse-integers)
  (values (part1 weights) (part2 weights)))

(define-test (2015 24) (11266889531 77387711))
