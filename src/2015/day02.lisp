(defpackage :aoc/2015/02 #.cl-user::*aoc-use*)
(in-package :aoc/2015/02)

(defun parse-dimentions (string)
  (mapcar #'parse-integer (cl-ppcre:split "x" string)))

(defun parse-input (lines) (mapcar #'parse-dimentions lines))

(defun paper (dimensions)
  (destructuring-bind (l w h) dimensions
    (let ((lw (* l w))
          (wh (* w h))
          (hl (* h l)))
      (+ (+ (* 2 lw) (* 2 wh) (* 2 hl))
         (min (* lw) (* wh) (* hl))))))

(defun part1 (list-of-dimentions)
  (reduce #'+ list-of-dimentions :key #'paper))

(defun ribbon (dimensions)
  (destructuring-bind (l w h) dimensions
    (let ((lw (+ l w))
          (wh (+ w h))
          (hl (+ h l)))
      (+ (* 2 (min lw wh hl))
         (* l w h)))))

(defun part2 (list-of-dimentions)
  (reduce #'+ list-of-dimentions :key #'ribbon))

(define-solution (2015 2) (list-of-dimensions parse-input)
  (values (part1 list-of-dimensions) (part2 list-of-dimensions)))

(define-test (2015 2) (1598415 3812909))
