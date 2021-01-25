(defpackage :aoc/2015/20 #.cl-user::*aoc-use*)
(in-package :aoc/2015/20)

(defun parse-target-presents (lines) (parse-integer (first lines)))

(defun part1 (target &aux
                     (all-houses (/ target 10))
                     (houses (make-array all-houses :initial-element 0)))
  (loop for elf from 1 below all-houses do
        (loop for house from elf below all-houses by elf do
              (incf (aref houses house) (* elf 10))))
  (position-if (partial-1 #'>= _ target) houses))

(defun part2 (target &aux
                     (all-houses (/ target 10))
                     (houses (make-array all-houses :initial-element 0)))
  (loop for elf from 1 below all-houses do
        (loop repeat 50
              for house from elf below all-houses by elf do
              (incf (aref houses house) (* elf 11))))
  (position-if (partial-1 #'>= _ target) houses))

(define-solution (2015 20) (target parse-target-presents)
  (values (part1 target) (part2 target)))

(define-test (2015 20) (831600 884520))
