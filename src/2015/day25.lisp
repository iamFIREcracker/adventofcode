(defpackage :aoc/2015/25 #.cl-user::*aoc-use*)
(in-package :aoc/2015/25)

(defun parse-target-position (lines)
  (cl-ppcre:register-groups-bind ((#'parse-integer row col))
      ("row (\\d+), column (\\d+)" (first lines))
    (list row col)))

(defun next-grid-position (&optional prev)
  (if (not prev)
    (list 1 1)
    (let ((next (mapcar #'+ prev (list -1 1))))
      (if (= (first next) 0)
        (list (second next) 1)
        next))))

(defun next-code (&optional prev)
  (if (not prev) 20151125 (mod (* prev 252533) 33554393)))

(defun part1 (target)
  (loop for pos = (next-grid-position) then (next-grid-position pos)
        for code = (next-code) then (next-code code)
        when (equal pos target) return code))

(define-solution (2015 25) (pos parse-target-position)
  (values (part1 pos)))

(define-test (2015 25) (2650453))
