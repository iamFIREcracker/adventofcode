(defpackage :aoc/2019/04 #.cl-user::*aoc-use*)
(in-package :aoc/2019/04)

(defun read-range (data &aux (str (first data)))
  (let* ((parts (split-sequence:split-sequence #\- str)))
    (mapcar #'parse-integer parts)))

(defun digits (number)
  (loop
    :for rest = number :then (floor rest 10)
    :for d = (mod rest 10)
    :while (> rest 0)
    :collect d :into digits
    :finally (return (reverse digits))))

(defun not-decreasing (digits)
  (loop
    :for (d . remaining) :on digits
    :for next = (first remaining)
    :while next
    :always (<= d next)))

(defun at-least-one-digit-repeated (digits)
  "`DIGITS` are expected to be sorted"
  (loop
    :for (d . remaining) :on digits
    :for next = (first remaining)
    :while next
    :when (= d next) :return T))

(defun solve-part1 (from to)
  (loop
    :for password :from from :upto (1+ to)
    :for digits = (digits password)
    :counting (and (not-decreasing digits) (at-least-one-digit-repeated digits))))

(defun at-least-digit-with-frequency-2 (digits)
  "`DIGITS` are expected to be sorted"
  (loop
    :for (d . remaining) :on digits
    :thereis (= 1 (count d remaining))))

(defun solve-part2 (from to)
  (loop
    :for password :from from :upto (1+ to)
    :for digits = (digits password)
    :counting (and
                (not-decreasing digits)
                (at-least-one-digit-repeated digits)
                (at-least-digit-with-frequency-2 digits))))

(define-problem (2019 4) (range read-range)
  (destructuring-bind (from to) range
    (values
      (solve-part1 from to)
      (solve-part2 from to))))

(1am:test test-2019/04
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 960 part1))
    (1am:is (= 626 part2))))
