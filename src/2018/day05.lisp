(defpackage :aoc/2018/05 #.cl-user::*aoc-use*)
(in-package :aoc/2018/05)

(defun char-different-case-p (u1 u2)
  (and
    (char-equal u1 u2) ; `char-equal` doesn't take case into account
    (not (char= u1 u2))))

(defun reduce-polymer (polymer)
  (let (reduced)
    (dovector (c polymer (coerce (nreverse reduced) 'string))
      (if (and reduced (char-different-case-p (first reduced) c))
        (pop reduced)
        (push c reduced)))))

(define-problem (2018 5) (data first)
  (values
    (length (reduce-polymer data))
    (loop
      :for u :across (remove-duplicates data :test #'char-equal)
      :for candidate = (reduce-polymer
                         (remove u data :test #'char-equal))
      :minimizing (length candidate))))

(1am:test test-2018/05
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 9202 part1))
    (1am:is (= 6394 part2))))
