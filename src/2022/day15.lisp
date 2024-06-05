(defpackage :aoc/2022/15 #.cl-user::*aoc-use*)
(in-package :aoc/2022/15)

(defstruct (sensor (:type list)
                   (:conc-name))
  pos beacon)
(defun x (p) (car p))
(defun y (p) (cadr p))
(defun r (s) (manhattan-distance (pos s) (beacon s)))

(defun parse-sensor (string)
  (destructuring-bind (sx sy bx by) (extract-integers string)
    (make-sensor :pos (list sx sy) :beacon (list bx by))))

(defun sensors (&optional (file #P"src/2022/day15.txt"))
  (mapcar #'parse-sensor (uiop:read-file-lines file)))

(defun bounding-square (sensors)
  (loop for s in sensors
        for r = (r s) for (x y) = (car s)
        minimize (- x r) into x-min
        minimize (- y r) into y-min
        maximize (+ x r) into x-max
        maximize (+ y r) into y-max
        finally (return (list x-min x-max y-min y-max))))

(defun part1 (sensors &optional (y 2000000))
  (destructuring-bind (x-min x-max . _) (bounding-box sensors)
    (declare (ignore _))
    (- x-max x-min
       (loop for x from x-min upto x-max for point = (list x y)
             count (loop for sensor in sensors
                         if (equal point (beacon sensor)) return nil
                         never (in-range-p sensor point))))))

(defun in-range-p (sensor point)
  (<= (manhattan-distance (pos sensor) point)
      (r sensor)))


(defun part2 (sensors &optional (size 4000000))
  (or
    ;; Around the vertices of each sensor
    #+#:excluded (loop for sensor in sensors
                       do (loop for v in (vertices sensor)
                                do (loop for p in (around v)
                                         when (and (insidep p size)
                                                   (notany [in-range-p _ p] sensors))
                                         do (return-from part2 (tuning-frequency p)))))
    (loop for s1 in sensors
          do (loop for s2 in sensors unless (eq s1 s2)
                   do (loop for v in (overlapping-points s1 s2)
                            do (loop for p in (around v)
                                     when (and (insidep p size)
                                               (notany [in-range-p _ p] sensors))
                                     do (return-from part2 (tuning-frequency p))))))
    ;; On the perimeter...
    #+#:excluded (loop for x from 0 upto size for p = (list x    0) when (notany [in-range-p _ p] sensors) return (tuning-frequency p))
    #+#:excluded (loop for x from 0 upto size for p = (list x size) when (notany [in-range-p _ p] sensors) return (tuning-frequency p))
    #+#:excluded (loop for y from 0 upto size for p = (list 0    y) when (notany [in-range-p _ p] sensors) return (tuning-frequency p))
    #+#:excluded (loop for y from 0 upto size for p = (list size y) when (notany [in-range-p _ p] sensors) return (tuning-frequency p))))

(defun overlap? (sensor1 sensor2)
  (< (manhattan-distance (pos sensor1) (pos sensor2))
     (+ (r sensor1) (r sensor2))))
#+#:excluded (overlap? '((0 0) (0 3)) '((3 2) (3 0)))
#+#:excluded (overlap? '((0 0) (0 3)) '((3 1) (3 -1)))

(defun overlapping-points (sensor1 sensor2)
  (when (overlap? sensor1 sensor2)
    (let ((r (abs (- (r sensor1) (r sensor2))))
          (dir-x (<=> (x (pos sensor2)) (x (pos sensor1))))
          (dir-y (<=> (y (pos sensor2)) (y (pos sensor1)))))
      ; (pr r (- (r sensor1)) dir-x dir-y)
      (loop for r1 in (list 0 r) for r2 = (- (r sensor1) r1)
            collect (list (+ (x (pos sensor1)) (* r1 dir-x))
                          (+ (y (pos sensor1)) (* r2 dir-y)))
            collect (list (+ (x (pos sensor1)) (* r2 dir-x))
                          (+ (y (pos sensor1)) (* r1 dir-y)))))))
(overlapping-points '((0 0) (0 3)) '((3 -1) (3 1)))
(overlapping-points '((3 -1) (3 1)) '((0 0) (0 3)))


(defun vertices (sensor)
  (destructuring-bind ((x y) . _) sensor
    (declare (ignore _))
    (bnd1 r (r sensor)
      (list
        (list x (- y r))
        (list (+ x r) y)
        (list x (+ y r))
        (list (- x r) y)))))

(defun around (point)
  (destructuring-bind (x y) point
    (list
      (list x (- y 1))
      (list (+ x 1) y)
      (list x (+ y 1))
      (list (- x 1) y))))

(defun insidep (point size)
  (and (<= 0 (x point) size)
       (<= 0 (y point) size)))

(defun tuning-frequency (point) (+ (* (x point) 4000000) (y point)))

#; Scratch
(part1 (sensors #P"scratch.txt") 10)
(part1 (sensors))

(part2 (sensors #P"scratch.txt") 20)
(part2 (sensors))

(let ((sensors (sensors))
      (size 4000000))
  (loop for s1 in sensors
        do (loop for s2 in sensors unless (eq s1 s2)
                 do (loop for v in (overlapping-points s1 s2)
                          do (loop for p1 in (around v)
                                   do (loop for p2 in (around v)
                                            do (loop for p in (around p2)
                                                     when (and (insidep p size)
                                                               (notany [in-range-p _ p] sensors))
                                                     do (error (tuning-frequency p)))))))))

;; Find two sensors which are just a tiny bit too far from each other,
;; to leave room for a bach to sit there
;; for each point of the border
;; for each point _around_ that
;; see if none of the sensors are in range
(let ((sensors (sensors))
      (size 4000000))
  (loop named outer for (s1 . remaining-sensors) on sensors nconc
        (loop for s2 in remaining-sensors
              when (= (manhattan-distance (pos s1) (pos s2))
                      (+ (r s1) (r s2) 2)) ; why 2 and not... 1?!
              do (loop for v in (border-points s1 s2)
                       do (loop for p in (around v)
                                when (and (insidep p size)
                                          (notany [in-range-p _ p] sensors))
                                do (return-from outer (tuning-frequency p)))))))

(defun border-points (sensor1 sensor2)
  (let ((dir-x (<=> (x (pos sensor2)) (x (pos sensor1))))
        (dir-y (<=> (y (pos sensor2)) (y (pos sensor1)))))
    (loop for delta-x from 0 to (r sensor1) for delta-y = (- (r sensor1) delta-x)
          collect (list (+ (x (pos sensor1)) (* delta-x dir-x))
                        (+ (y (pos sensor1)) (* delta-y dir-y))))))
(border-points '((0 0) (0 3)) '((2 -4) (2 -3)))

(let ((sensors (sensors)))
  (loop named outer for x from 0 upto 4000000
        do (loop for y from 0 upto 4000000 for p = (list x y)
                 when (notany [in-range-p _ p] sensors)
                 do (return-from outer (tuning-frequency p)))))
;; looks like this can be solved using ranges
;; given a certain y and certain sensor, which are values of x for which the sensor is still in range?
;; you then intersect all these ranges and you should be good to go
;; even for part 2 a mere bruteforce should do just fine
