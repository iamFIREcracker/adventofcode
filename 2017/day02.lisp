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
    :for b = (find T remaining :key (curry #'dividesp a))
    :when b
    :do (return (values a b))))

(define-problem (2017 2) (data parse-lines-of-integers)
  (values
    (loop
      :for values :in data
      :for min = (minimization values)
      :for max = (maximization values)
      :summing (- max min))
    (loop
      :for values :in data
      :for (divisor number) = (multiple-value-list (evenly-divisible-values values))
      :summing (/ number divisor))))

(1am:test test-2017/02
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 34925 part1))
    (1am:is (= 221 part2))))
