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

(define-solution (2016 3) (specs read-specifications)
  (values (count-if #'valid-triangle-p specs)
          (count-if #'valid-triangle-p (rotate-specs specs))))

(define-test (2016 3) (993 1849))
