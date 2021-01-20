(defpackage :aoc/2015/18 #.cl-user::*aoc-use*)
(in-package :aoc/2015/18)

(defun parse-grid (lines &aux (grid (make-hset '())))
  (let ((row 1))
    (dolist (string lines grid)
      (loop for ch across string for col from 1
            when (char= ch #\#) do (hset-add (complex col row) grid))
      (incf row))))

(defparameter *nhood-deltas* '(#C(-1 1) #C(0 1) #C(1 1) #C(-1 0) #C(1 0) #C(-1 -1) #C(0 -1) #C(1 -1)))

(defun neighbors (pos)
  (loop for d in *nhood-deltas* for n = (+ pos d)
        when (and (<= 1 (realpart n) 100) (<= 1 (imagpart n) 100))
        collect n))

(defun part1 (grid)
  (dotimes (n 100 (hset-size grid))
    (setf grid (gol:next grid :neighbors #'neighbors))))

(defun turn-corners-on (grid)
  (hset-add #C(1 1) grid)
  (hset-add #C(1 100) grid)
  (hset-add #C(100 1) grid)
  (hset-add #C(100 100) grid)
  grid)

(defun part2 (grid &aux (grid (turn-corners-on grid)))
  (dotimes (n 100 (hset-size grid))
    (setf grid (turn-corners-on (gol:next grid :neighbors #'neighbors)))))

(define-solution (2015 18) (grid parse-grid)
  (values (part1 grid) (part2 grid)))

(define-test (2015 18) (814 924))
