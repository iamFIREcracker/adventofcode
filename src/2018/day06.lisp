(defpackage :aoc/2018/06 #.cl-user::*aoc-use*)
(in-package :aoc/2018/06)

(defvar *tag* 0)

(defun parse-points (data)
  (let ((*tag* 0))
    (mapcar #'parse-point data)))

(defun parse-point (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer col row))
      ("(\\d+), (\\d+)" string)
    (make-point (incf *tag*) row col)))

(defun make-point (tag row col) (cons tag (make-coord row col)))
(defun tag (coord) (car coord))
(defun (setf tag) (value coord) (setf (car coord) value))
(defun coord (point) (cdr point))

(defun make-coord (row col) (complex col (- row)))
(defun row (coord) (- (imagpart coord)))
(defun col (coord) (realpart coord))


(defun part1 (points &aux (bbox (bounding-box points)))
  (destructuring-bind (row-min col-min row-max col-max) bbox
    (let ((sizes (make-hash-table)))
      (loop :for row :from row-min :to row-max :do
            (loop :for col :from col-min :to col-max
                  :for test := (make-coord row col)
                  :for closest := (closest-point test points)
                  :do (if (on-the-border-p test bbox)
                        (setf (gethash (tag closest) sizes) most-negative-fixnum)
                        (incf (gethash (tag closest) sizes 0)))))
      (loop :for size :being :the :hash-values :of sizes :using (hash-key tag)
            :when tag :maximize size))))

(defun bounding-box (points)
  (loop :for p :in points
        :for row := (row (coord p)) :for col := (col (coord p))
        :minimizing row :into row-min
        :maximizing row :into row-max
        :minimizing col :into col-min
        :maximizing col :into col-max
        :finally (return (list row-min col-min row-max col-max))))

(defun closest-point (test points)
  (let (closest closest-distance)
    (dolist (p points)
      (let ((distance (manhattan-distance test (coord p))))
        (when (or (not closest-distance) (<= distance closest-distance))
          (if (or (not closest-distance) (< distance closest-distance))
            (setf closest (list p) closest-distance distance)
            (push p closest)))))
    (when (= (length closest) 1)
      (first closest))))

(defun on-the-border-p (point bbox)
  (destructuring-bind (row-min col-min row-max col-max) bbox
    (or (= (row point) row-min) (= (row point) row-max)
        (= (col point) col-min) (= (col point) col-max))))


(defun part2 (points &aux (bbox (bounding-box points)))
  (destructuring-bind (row-min col-min row-max col-max) bbox
    (loop :for row :from row-min :to row-max :summing
          (loop :for col :from col-min :to col-max
                :for test := (make-coord row col)
                :for total-distance := (total-distance test points)
                :counting (< total-distance 10000)))))

(defun total-distance (test points)
  (loop :for p :in points
        :summing (manhattan-distance test (coord p))))


(define-solution (2018 6) (points parse-points)
  (values (part1 points) (part2 points)))

(define-test (2018 6) (3894 39398))
