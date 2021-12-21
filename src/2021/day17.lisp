(defpackage :aoc/2021/17 #.cl-user::*aoc-use*)
(in-package :aoc/2021/17)


(defstruct (probe (:conc-name) (:constructor %make-probe)) x y vx vy)

(defun make-probe (vx vy) (%make-probe :x 0 :y 0 :vx vx :vy vy))

(defun move (probe)
  (with-slots (x y vx vy) probe
    (let ((dx (<=> vx 0)))
      (incf x vx)
      (incf y vy)
      (decf vx dx)
      (decf vy)))
  probe)


(defstruct (area (:conc-name) (:constructor %make-area)) x1 x2 y1 y2)

(defun make-area (x1 x2 y1 y2)
  (%make-area :x1 (min x1 x2) :x2 (max x1 x2)
              :y1 (min y1 y2) :y2 (max y1 y2)))

(defun parse-input (data)
  (flet ((parse (string)
           (cl-ppcre:register-groups-bind ((#'parse-integer x1 x2 y1 y2))
               ("target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)" string)
             (make-area x1 x2 y1 y2))))
    (parse (first data))))


(defun solve (target &aux (part1 0) (part2 0))
  (loop for vx from (vx-min target) to (vx-max target) do
        (loop for vy from (vy-min target) to (vy-max target) do
              (uiop:if-let (highest (shoot target vx vy))
                (setf part1 (max part1 highest)
                      part2 (1+ part2)))))
  (values part1 part2))


(defun shoot (target vx vy &aux (probe (make-probe vx vy)))
  (loop until (past-target-p target probe)
        when (on-target-p target probe) return y-max
        do (move probe)
        maximize (y probe) into y-max))

(defun past-target-p (target probe) (< (y probe) (y1 target)))

(defun on-target-p (target probe)
  (and (<= (x1 target) (x probe) (x2 target))
       (<= (y1 target) (y probe) (y2 target))))


(defun vx-max (target) (x2 target))

(defun vx-min (target &aux (x (x1 target)))
  (loop while (> x 0) for vx from 0 do (decf x vx) finally (return vx)))

(defun vy-max (target) (- (y1 target)))

#+#:used-for-part-1 (defun vy-min (target) (declare (ignore target)) 0)
(defun vy-min (target) (y1 target))

(define-solution (2021 17) (target parse-input) (solve target))

(define-test (2021 17) (5565 2118))
