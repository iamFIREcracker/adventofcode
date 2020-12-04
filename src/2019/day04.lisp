(defpackage :aoc/2019/04 #.cl-user::*aoc-use*)
(in-package :aoc/2019/04)

(defun read-range (data &aux (str (first data)))
  (let* ((parts (split-sequence:split-sequence #\- str)))
    (mapcar #'parse-integer parts)))

(defun not-decreasing (digits)
  (loop
    :for (d . (next . remaining)) :on digits
    :while next
    :always (<= d next)))

(defun at-least-one-digit-repeated (digits)
  "`DIGITS` are expected to be sorted"
  (loop
    :for (d . (next . remaining)) :on digits
    :while next
    :when (= d next) :return T))

(defun solve-part1 (from to)
  (loop
    :for password :from from :upto (1+ to)
    :for digits = (digits-reverse password)
    :counting (and
                (not-decreasing digits)
                (at-least-one-digit-repeated digits))))

(defun at-least-digit-with-frequency-2 (digits)
  (loop
    :for d :in digits
    :thereis (= 2 (count d digits))))

(defun solve-part2 (from to)
  (loop
    :for password :from from :upto (1+ to)
    :for digits = (digits-reverse password)
    :counting (and
                (not-decreasing digits)
                (at-least-one-digit-repeated digits)
                (at-least-digit-with-frequency-2 digits))))

(define-solution (2019 4) (range read-range)
  (destructuring-bind (from to) range
    (values
      (solve-part1 from to)
      (solve-part2 from to))))

(define-test (2019 4) (960 626))
