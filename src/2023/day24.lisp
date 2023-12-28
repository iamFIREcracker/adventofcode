(defpackage :aoc/2023/24 #.cl-user::*aoc-use*)
(in-package :aoc/2023/24)


(defun hailstone (s) (extract-integers s))
#+#:excluded (hailstone "248315803897794, 386127890875011, 326651351825022 @ -89, -119, 32")

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day24.txt")))
  (mapcar #'hailstone strings))


;; https://stackoverflow.com/a/46288706
(defun line (h)
  (destructuring-bind (x y _1 vx vy _2) h
    (declare (ignore _1 _2))
    ;; # y = mx + b
    ;; # b = y - mx
    ;; # b = P1[1] - slope * P1[0]
    (bnd* ((slope (/ vy vx)) (y-intercept (- y (* slope x))))
      (list slope y-intercept))))

(defun intersect-2d? (h1 h2)
  (destructuring-bind (m1 b1) (line h1)
    (destructuring-bind (m2 b2) (line h2)
      (unless (= m1 m2)
        ;; # y = mx + b
        ;; # Set both lines equal to find the intersection point in the x direction
        ;; # m1 * x + b1 = m2 * x + b2
        ;; # m1 * x - m2 * x = b2 - b1
        ;; # x * (m1 - m2) = b2 - b1
        ;; # x = (b2 - b1) / (m1 - m2)
        ;; x = (b2 - b1) / (m1 - m2)
        ;; # Now solve for y -- use either line, because they are equal here
        ;; # y = mx + b
        ;; y = m1 * x + b1
        (bnd* ((x (/ (- b2 b1) (- m1 m2))) (y (+ (* m1 x) b1)))
          (list x y))))))

(defun in-the-future? (h ix iy)
  (destructuring-bind (x1 y1 _1 vx vy _2) h
    (declare (ignore _1 _2))
    (bnd* ((x2 (+ x1 vx)) (y2 (+ y1 vy))
           (dx1 (- x2 x1)) (dx2 (- ix x1)))
      (plusp (* dx1 dx2)))))

(defun part1 (min max &optional (hh (parse-input)))
  (looping
    (dosublists ((h1 . rest) hh)
      (dolist (h2 rest)
        (awhen (intersect-2d? h1 h2)
          (destructuring-bind (x y) it
            (when (and (in-the-future? h1 x y)
                       (in-the-future? h2 x y))
              (count! (and (<= min x max)
                           (<= min y max))))))))))
#+#:excluded (part1 7 27)
#+#:excluded (part1 200000000000000 400000000000000)
; 24999 nope
; 16779 !!!

(defun hail-at (h time)
  (destructuring-bind (x y z vx vy vz) h
    (list (+ x (* time vx)) (+ y (* time vy)) (+ z (* time vz)))))

(defun speed-magnitude (h)
  (destructuring-bind (vx vy vz) (nthcdr 3 h)
    (sqrt (+ (* vx vx) (* vy vy) (* vz vz)))))

(defun v- (v1 v2)
  "Subtracts vector `v2` from vector `v1`"
  (mapcar #'- v1 v2))

(defun v/s (v s)
  "Divides vector `v` by scalar `s`"
  (mapcar [/ _ s] v))

(defun v*s (v s)
  "Multiplies vector `v` by scalar `s`"
  (mapcar [* _ s] v))

(defun v/ (v1 v2)
  "Divides vector `v1` by vector `v2` -- whatever that means"
  (mapcar #'/ v1 v2))
(defun part2 (&optional (hh (parse-input)))
  (setf hh (sort hh #'> :key #'speed-magnitude))
  (bnd* (((h1 h2 h3 . rest) (subseq hh 0 3)))
    (dorangei (time1 1 10000)
      (bnd* ((c1 (hail-at h1 time1)))
        (dorangei (time2 1 10000)
          (unless (= time1 time2)
            (bnd* ((c2 (hail-at h2 time2))
                   (vel (v/s (v- c1 c2)
                             (- time1 time2))))

              (when (every #'integerp vel)
                (dorangei (time3 1 10000)
                  (bnd* ((c3 (hail-at h3 time3))
                         (pos (v- c3 (v*s vel time3))))

                    (dolist (h rest)
                      (bnd* (time ( ))))
                    (pr vel pos)
                    (break)))))))))
    #+#:excluded (while t
                   )
    (pr h1 h2))
  #+#:excluded (destructuring-bind (tx ty tx tvx tvy tvz) throw
                 (destructuring-bind (hx hy hx hvx hvy hvz) h

                   ))
  (values))
#+#:excluded (part2)
; (integerp 1)
; (length (parse-input))
