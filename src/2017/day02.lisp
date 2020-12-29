(defpackage :aoc/2017/02 #.cl-user::*aoc-use*)
(in-package :aoc/2017/02)

(defun parse-lines-of-integers (x)
  (mapcar
    #'(lambda (str)
        (mapcar
          #'parse-integer
          (split-sequence:split-sequence #\Tab str)))
    x))

(defun evenly-divisible-values (x)
  (loop
    :for (a . remaining) :on (sort x #'<)
    :for b = (find T remaining :key (partial-1 #'dividesp a))
    :when b
    :do (return (values a b))))

(define-solution (2017 2) (data parse-lines-of-integers)
  (values
    (loop
      :for values :in data
      :for min = (reduce #'min values)
      :for max = (reduce #'max values)
      :summing (- max min))
    (loop
      :for values :in data
      :for (divisor number) = (multiple-value-list (evenly-divisible-values values))
      :summing (/ number divisor))))

(define-test (2017 2) (34925 221))
