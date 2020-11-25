(defpackage :aoc/2017/11 #.cl-user::*aoc-use*)
(in-package :aoc/2017/11)

(defstruct point
  (x 0)
  (y 0)
  (z 0))

(defun point-+ (p1 p2)
  (make-point
    :x (+ (point-x p1) (point-x p2))
    :y (+ (point-y p1) (point-y p2))
    :z (+ (point-z p1) (point-z p2))))

(defvar *direction-to-complex*)
(setf *direction-to-complex* (list
                               ;; https://www.redblobgames.com/grids/hexagons/
                               ;; y increases in the N-E direction
                               ;; x increases in the S-E direction
                               ;; z increases in the W direction
                               (list "n"  (make-point :x -1 :y  1))
                               (list "ne" (make-point :y  1 :z -1))
                               (list "se" (make-point :x  1 :z -1))
                               (list "s"  (make-point :x  1 :y -1))
                               (list "sw" (make-point :y -1 :z  1))
                               (list "nw" (make-point :x -1 :z  1))))

(defun parse-one-line-of-csv (x)
  (split-sequence:split-sequence #\, (first x)))

(define-problem (2017 11) (data parse-one-line-of-csv)
  (labels ((convert-to-delta (dir)
             (second (assoc dir *direction-to-complex* :test 'equal)))
           (point-list (p)
             (list (point-x p) (point-y p) (point-z p)))
           (distance (p1 p2)
             ;; every step takes us 2 units away from the previous point
             ;; that's why we divide `MANHATTAN-DISTANCE` by 2
             (/ (manhattan-distance (point-list p1) (point-list p2)) 2)))
    (loop
      :with origin = (make-point)
      :with p = origin
      :for d :in data
      :for delta = (convert-to-delta d)
      :do (setf p (point-+ p delta))
      :maximizing (distance origin p) :into furthest
      :finally (return (values (distance origin p)
                               furthest)))))

(1am:test test-2017/11
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 761 part1))
    (1am:is (= 1542 part2))))
