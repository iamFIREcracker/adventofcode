(defpackage :aoc/2023/24 #.cl-user::*aoc-use*)
(in-package :aoc/2023/24)


(defun hailstone (s) (extract-integers s))
#+#:excluded (hailstone "248315803897794, 386127890875011, 326651351825022 @ -89, -119, 32")

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day24.txt")))
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

(defun ignore-axis (n h)
  (destructuring-bind (x y z vx vy vz) h
    (ecase n
      (0 (list y z vy vz))
      (1 (list x z vx vz))
      (2 (list x y vx vy)))))

#+#:excluded (length (all-intersections))
(defun part1 (&optional (hh (parse-input)))
  (looping
    (doseq ((x y) (all-intersections (mapcar [ignore-axis 2 _] hh)))
      (count! (and (<= 200000000000000 x 400000000000000)
                   (<= 200000000000000 y 400000000000000))))))
#+#:excluded (part1 7 27)
#+#:excluded (part1  )
; 24999 nope
; 16779 !!!

(defun adjust-velocity (dvx dvy h)
  (destructuring-bind (x y vx vy) h
    (list x y (+ vx dvx) (+ vy dvy))))
#+#:excluded (untrace adjust-velocity)

(defun part2 (&optional (hh (parse-input)))
  (flet ((magic (axis &aux )
           (bnd1 (hh (mapcar [ignore-axis axis _] (subseq hh 0 3)))
             (dorangei (vx -300 300)
               (dorangei (vy -300 300)
                 (bnd1 (points (all-intersections (mapcar [adjust-velocity vx vy _]
                                                          hh)))
                   (when (and (> (length points) 1)
                              (= (length (remove-duplicates points :test #'equal)) 1))
                     (return-from magic (first points)))))))))
    (destructuring-bind (px1 py) (magic 2)
      (destructuring-bind (px2 pz) (magic 1)
        (assert (= px1 px2))
        (+ px1 py pz)))))
#+#:excluded (time (part2))
; 871983857253169
