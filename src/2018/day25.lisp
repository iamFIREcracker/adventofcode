(defpackage :aoc/2018/25 #.cl-user::*aoc-use*)
(in-package :aoc/2018/25)

(defun parse-point (s)
  (parse-integers (split-sequence:split-sequence #\, s)))

(defun parse-points (x)
  (mapcar #'parse-point x))

(define-solution (2018 25) (data parse-points)
  (values
    (let ((sets (mapcar #'make-dset data)))
      (loop
        :for (s1 . remaining) :on sets
        :do (loop
              :for s2 :in remaining
              :do (let* ((p1 (dset-value s1))
                         (p2 (dset-value s2))
                         (distance (manhattan-distance p1 p2)))
                    (if (<= distance 3)
                      (dset-union s1 s2)))))
      (length (remove-duplicates sets :key #'dset-find :test 'eq)))))

(define-test (2018 25) (420))
