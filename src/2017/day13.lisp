(defpackage :aoc/2017/13 #.cl-user::*aoc-use*)
(in-package :aoc/2017/13)

(defstruct layer
  depth
  range
  curr
  dir)

(defun parse-layers (x)
  (flet ((parse-layer (s)
           (let* ((splits (split-sequence:split-sequence #\Space s))
                  (depth (parse-integer (first splits) :junk-allowed T))
                  (range (parse-integer (second splits))))
             (make-layer :depth depth
                         :range range
                         :curr 0
                         :dir +1))))
    (mapcar #'parse-layer x)))

(defun trip-severity (layers)
  (loop
    :for layer :in layers
    :for depth = (layer-depth layer)
    :for time-to-layer = depth
    :for range = (layer-range layer)
    :for period = (* (1- range) 2)
    :when (dividesp period time-to-layer)
    :sum (* depth range))) 

(defun caughtp (layers delay)
  (loop
    :for layer :in layers
    :for depth = (layer-depth layer)
    :for time-to-layer = (+ delay depth)
    :for range = (layer-range layer)
    :for period = (* (1- range) 2)
    :thereis (dividesp period time-to-layer)))

(define-solution (2017 13) (data parse-layers)
  (values
    (trip-severity data)
    (loop
      :for delay = 1 :then (1+ delay)
      :unless (caughtp data delay)
      :do (return delay))))

(define-test (2017 13) (2508 3913186))
