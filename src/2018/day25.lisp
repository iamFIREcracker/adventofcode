(defpackage :aoc/2018/25 #.cl-user::*aoc-use*)
(in-package :aoc/2018/25)

(defun parse-point (s)
  (parse-integers (split-sequence:split-sequence #\, s)))

(defun parse-points (x)
  (mapcar #'parse-point x))

(define-problem (2018 25) (data parse-points)
  (values
    (let ((sets (mapcar #'make-disjointset data)))
      (loop
        :for (s1 . remaining) :on sets
        :do (loop
              :for s2 :in remaining
              :do (let* ((p1 (disjointset-value s1))
                         (p2 (disjointset-value s2))
                         (distance (manhattan-distance p1 p2)))
                    (if (<= distance 3)
                      (disjointset-union s1 s2)))))
      (length (distinct-disjointsets sets)))))

(1am:test test-2018/25
  (multiple-value-bind (part1) (problem-run)
    (1am:is (= 420 part1))))
