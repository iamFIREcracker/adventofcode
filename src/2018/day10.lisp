(defpackage :aoc/2018/10 #.cl-user::*aoc-use*)
(in-package :aoc/2018/10)

(defstruct star x y vx vy)

(defun parse-star (str)
  (let* ((parts (split-sequence:split-sequence #\< str))
         (coordsstr (second parts))
         (speedstr (third parts))
         (coords-parts (split-sequence:split-sequence #\, coordsstr))
         (speed-parts (split-sequence:split-sequence #\, speedstr)))
    (make-star
      :x (parse-integer (first coords-parts) :junk-allowed T)
      :y (parse-integer (second coords-parts) :junk-allowed T)
      :vx (parse-integer (first speed-parts) :junk-allowed T)
      :vy (parse-integer (second speed-parts) :junk-allowed T))))

(defun min-max (values)
  (loop
    :for v :in values
    :minimizing v :into min-v
    :maximizing v :into max-v
    :finally (return (values min-v max-v))))

(defun parse-starrs (x)
  (mapcar #'parse-star x))

(defun bounding-box-area (points &optional (x 'first) (y 'second))
  (multiple-value-bind (minx maxx) (min-max (mapcar x points))
    (multiple-value-bind (miny maxy) (min-max (mapcar y points))
      (* (- maxx minx)
         (- maxy miny)))))

(define-solution (2018 10) (data parse-stars)
  (multiple-value-bind (players marbles) (parse-marble-setup data)
    (values
      (play players marbles)
      (play players (* marbles 100)))))

(define-test (2018 10) (384892 3169872331))
