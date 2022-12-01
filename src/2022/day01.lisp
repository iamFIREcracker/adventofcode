(defpackage :aoc/2022/01 #.cl-user::*aoc-use*)
(in-package :aoc/2022/01)


(defun elf-bags (strings)
  (mapcar [mapcar #'parse-integer _]
          (split-sequence:split-sequence "" strings :test #'string=)))

(defun bag-cals (bag) (reduce #'+ bag))


(defun part1 (bags) (reduce #'max bags :key #'bag-cals))

(defun part2 (bags)
  (reduce #'+ (sort (mapcar #'bag-cals bags) #'>) :end 3))


(define-solution (2022 01) (bags elf-bags)
  (values (part1 bags) (part2 bags)))

(define-test (2022 01) (75501 215594))
