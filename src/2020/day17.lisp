(defpackage :aoc/2020/17 #.cl-user::*aoc-use*)
(in-package :aoc/2020/17)

(defun parse-coords (data)
  (let* ((width (length (first data)))
         (height (length data)))
    (loop for y upto height for row in data nconc
          (loop for x upto width for c across row
                when (eql c #\#) collect (list x y 0)))))

(defparameter *neighbors-deltas* nil)

(defun neighbors-deltas (dimensions)
  (labels ((recur (n)
             (cond ((= n 0) (list nil))
                   (t (loop for d from -1 upto 1 nconc
                            (loop for rest in (recur (1- n))
                                  collect (cons d rest)))))))
    (remove-if (lambda (x) (apply #'= 0 x)) (recur dimensions))))

(defun neighbors (pos)
    (loop for delta in *neighbors-deltas* collect (mapcar #'+ pos delta)))

(defun play (coords &aux (dimensions (length (first coords))))
  (let* ((*neighbors-deltas* (neighbors-deltas dimensions))
         (game (make-hset coords :test 'equal)))
    (dotimes (n 6 (hset-size game))
      (setf game (gol:next game :neighbors #'neighbors)))))

(define-solution (2020 17) (coords parse-coords)
  (values
    (play coords)
    (play (mapcar #'(lambda (x) (append x (list 0))) coords))))

(define-test (2020 17) (315 1520))
