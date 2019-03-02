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

(defun update-layers (layers)
  (dolist (l layers)
    (let* ((next-cur (+ (layer-curr l) (layer-dir l))))
      (cond ((>= next-cur (layer-range l))
             (setf (layer-dir l) -1))
            ((< next-cur 0)
             (setf (layer-dir l) 1)))
      (setf (layer-curr l) (+ (layer-curr l) (layer-dir l))))))

(defun copy-layers (layers)
  (mapcar #'copy-structure layers))

(defun trip-severity (layers depth-max)
  (loop
    :for depth :to depth-max
    :for layer = (find depth layers :key #'layer-depth)
    :when (and layer (zerop (layer-curr layer)))
    :sum (* (layer-depth layer) (layer-range layer))
    :do (update-layers layers)))

(defun caughtp (layers depth-max)
  (loop
    :for depth :to depth-max
    :for layer = (find depth layers :key #'layer-depth)
    :thereis (and layer (zerop (layer-curr layer)))
    :do (update-layers layers)))

(define-problem (2017 13) (data parse-layers)
  (let ((max-depth (reduce #'max data :key #'layer-depth)))
    (values
      (trip-severity (copy-layers data) max-depth)
      (loop
        :with layers = (copy-layers data)
        :for delay = 1 :then (1+ delay)
        :do (update-layers layers)
        :unless (caughtp (copy-layers layers) max-depth)
        :do (return delay)))))

(1am:test test-2017/13
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 2508 part1))
    (1am:is (= 3913186 part2))))
