(defpackage :aoc/2016/03 #.cl-user::*aoc-use*)
(in-package :aoc/2016/03)

(defun read-single-triangle-specification (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer a b c))
      ("(\\d+)\\s+(\\d+)\\s+(\\d+)" string)
    (list a b c)))

(defun read-specifications (data)
  (mapcar #'read-single-triangle-specification data))

(defun valid-triangle-p (spec)
  (destructuring-bind (a b c) spec
    (and (> (+ a b) c)
         (> (+ a c) b)
         (> (+ b c) a))))

(defun rotate-specs (specs)
  (loop
    :for (x y z) :on specs :by #'cdddr
    :for (ax bx cx) = x
    :for (ay by cy) = y
    :for (az bz cz) = z
    :collect (list ax ay az)
    :collect (list bx by bz)
    :collect (list cx cy cz)))

(define-problem (2016 03) (specs read-specifications)
  (values (count-if #'valid-triangle-p specs)
          (count-if #'valid-triangle-p (rotate-specs specs))))

(1am:test test-2016/03
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 993 part1))
    (1am:is (= 1849 part2))))
