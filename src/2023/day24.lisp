(defpackage :aoc/2023/24 #.cl-user::*aoc-use*)
(in-package :aoc/2023/24)


(defun hailstone (s) (extract-integers s))

(defun parse-input (&optional (strings (aoc::read-problem-input 2023 24)))
  (mapcar #'hailstone strings))


;; Copied from Wikipedia: https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
(defun intersect-2d? (x1 y1 x2 y2 x3 y3 x4 y4)
  (bnd* ((xnum (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4))
                  (* (- x1 x2) (- (* x3 y4) (* y3 x4)))))
         (ynum (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4))
                  (* (- y1 y2) (- (* x3 y4) (* y3 x4)))))
         (den (- (* (- x1 x2) (- y3 y4))
                 (* (- y1 y2) (- x3 x4)))))
    (unless (zerop den)
      (list (/ xnum den) (/ ynum den)))))


(defun ignore-axis (n h)
  (destructuring-bind (x y z vx vy vz) h
    (ecase n
      (0 (list y z vy vz))
      (1 (list x z vx vz))
      (2 (list x y vx vy)))))

(defun in-the-future? (x1 y1 x2 y2 x3 y3)
  (bnd* ((dx21 (- x2 x1)) (dx31 (- x3 x1))
         (dy21 (- y2 y1)) (dy31 (- y3 y1)))
    (and (>= (* dx21 dx31) 0)
         (>= (* dy21 dy31) 0))))

(defun all-intersections (&optional (hh (parse-input)))
  (looping
    (dosublists (((x1 y1 vx1 vy1) . rest) hh)
      (bnd* ((x2 (+ x1 vx1)) (y2 (+ y1 vy1)))
        (doseq ((x3 y3 vx3 vy3) rest)
          (bnd* ((x4 (+ x3 vx3)) (y4 (+ y3 vy3)))
            (awhen (intersect-2d? x1 y1 x2 y2 x3 y3 x4 y4)
              (destructuring-bind (x y) it
                (when (and (in-the-future? x1 y1 x2 y2 x y)
                           (in-the-future? x3 y3 x4 y4 x y))
                  (collect! (list x y)))))))))))


(defun part1 (&optional (hh (parse-input)))
  (looping
    (doseq ((x y) (all-intersections (mapcar [ignore-axis 2 _] hh)))
      (count! (and (<= 200000000000000 x 400000000000000)
                   (<= 200000000000000 y 400000000000000))))))


;; We know a position <x, y, z> exists such that, if we throw a hail rock with
;; velocity <vx, vy, vz>, eventually it will collide with all the other rocks.
;; This means that if we subtract <vx, vy, vz> from each rock in our input,
;; eventually the rocks will all collide in <x, y, z>.
;;
;; So the idea is to take a few rocks from our input, brute-force a bunch of
;; velocities along each axis, adjust rocks velocities, and see where the rocks
;; collide.
;;
;; Re-using the logic from part1, we can find first where the rocks collide
;; along x and y first, and then along x and z.
;;
;; Kudos to: https://www.youtube.com/watch?v=nP2ahZs40U8
(defun adjust-velocity (dvx dvy h)
  (destructuring-bind (x y vx vy) h
    (list x y (+ vx dvx) (+ vy dvy))))

(defun find-single-intersection (hh)
  (dorangei (vx -250 250)
    (dorangei (vy -250 250)
      (bnd1 (points (all-intersections (mapcar [adjust-velocity vx vy _]
                                               hh)))
        (when (and (> (length points) 1)
                   (= (length (remove-duplicates points :test #'equal)) 1))
          (return-from find-single-intersection (first points)))))))

(defun part2 (&optional (hh (parse-input)))
  (recursively ((hh (copy-seq hh)))
    (setf hh (shuffle hh))
    (bnd1 (sample (subseq hh 0 3))
      (awhen (find-single-intersection (mapcar [ignore-axis 2 _] sample))
        (destructuring-bind (px1 py) it
          (awhen (find-single-intersection (mapcar [ignore-axis 1 _] sample))
            (destructuring-bind (px2 pz) it
              (assert (= px1 px2))
              (return-from recur (+ px1 py pz)))))))
    (recur hh)))


(define-solution (2023 24) (hh parse-input)
  (values (part1 hh) (part2 hh)))

(define-test (2023 24) (16779 871983857253169))
