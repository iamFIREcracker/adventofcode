(defpackage :aoc/2018/05 #.cl-user::*aoc-use*)
(in-package :aoc/2018/05)

(defun char-different-case-p (u1 u2)
  (and
    (char-equal u1 u2) ; `char-equal` doesn't take case into account
    (not (char= u1 u2))))

(defun reduce-polymer (polymer)
  (let (reduced)
    (doseq (c polymer (coerce (nreverse reduced) 'string))
      (if (and reduced (char-different-case-p (first reduced) c))
        (pop reduced)
        (push c reduced)))))

(define-solution (2018 5) (data first)
  (values
    (length (reduce-polymer data))
    (loop
      :for u :across (remove-duplicates data :test #'char-equal)
      :for candidate = (reduce-polymer
                         (remove u data :test #'char-equal))
      :minimizing (length candidate))))

(define-test (2018 5) (9202 6394))
